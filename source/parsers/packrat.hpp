/**
 * packrat.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A memorizing parser that caches it's result.
 */

#ifndef CPPCMB_PARSERS_PACKRAT_HPP
#define CPPCMB_PARSERS_PACKRAT_HPP

#include <cstddef>
#include <utility>
#include "combinator.hpp"
#include "../memo_context.hpp"

namespace cppcmb {

namespace detail {

template <typename Self>
class packrat_base : public combinator<Self> {
private:
    std::uintptr_t m_ID;

protected:
    constexpr packrat_base() noexcept
        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
        : m_ID(reinterpret_cast<std::uintptr_t>(this)) {
    }

    [[nodiscard]] constexpr auto const& original_id() const noexcept {
        return m_ID;
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src, typename TFwd>
    constexpr auto& put_memo(reader<Src> const& r,
        TFwd&& val, std::size_t furth) const {

        auto& table = r.context().memo();
        return table.put(original_id(), r, cppcmb_fwd(val), furth);
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr std::any* get_memo(reader<Src> const& r) const {
        auto& table = r.context().memo();
        return table.get(original_id(), r);
    }
};

} /* namespace detail */

template <typename P>
class packrat_t : public detail::packrat_base<packrat_t<P>> {
private:
    cppcmb_self_check(packrat_t);

    P m_Parser;

public:
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr packrat_t(PFwd&& p)
        noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
        : m_Parser(cppcmb_fwd(p)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr decltype(auto) apply(reader<Src> const& r) const {
        cppcmb_assert_parser(P, Src);

        using result_t = parser_result_t<P, Src>;

        auto* entry = this->get_memo(r);
        if (entry == nullptr) {
            auto res = m_Parser.apply(r);
            return this->put_memo(r, std::move(res), res.furthest());
        }
        return std::any_cast<result_t&>(*entry);
    }
};

template <typename PFwd>
packrat_t(PFwd) -> packrat_t<PFwd>;

/**
 * Wrapper to make any combinator a packrat parser.
 */
template <typename PFwd>
[[nodiscard]] constexpr auto memo(PFwd&& p)
    cppcmb_return(packrat_t(cppcmb_fwd(p)))

struct as_self_t {};
struct as_memo_t {};

inline constexpr auto as_self = as_self_t();
inline constexpr auto as_memo = as_memo_t();

// Identity
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
constexpr auto operator%=(P&& parser, as_self_t)
    cppcmb_return(cppcmb_fwd(parser))

// Simple packrat
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
constexpr auto operator%=(P&& parser, as_memo_t)
    cppcmb_return(memo(cppcmb_fwd(parser)))

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_PACKRAT_HPP */
