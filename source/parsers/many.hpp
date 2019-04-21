/**
 * many.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A combinator that applies it's parser as many times as it can without
 * failing. Collects the results into a collection (std::vector by default).
 * Succeeds even when there are 0 matches.
 */

#ifndef CPPCMB_PARSERS_MANY_HPP
#define CPPCMB_PARSERS_MANY_HPP

#include <vector>
#include "combinator.hpp"
#include "../result.hpp"

// XXX(LPeter1997): We could check the collection for push_back (better errors)

namespace cppcmb {

/**
 * A type-pack that describes a collection except it's type.
 * Used for the many and many1 combinators.
 */
template <
    template <typename...> typename Coll,
    template <typename> typename... Ts
>
struct collect_to_t {
    template <typename T>
    using type = Coll<T, Ts<T>...>;
};

template <
    template <typename...> typename Coll,
    template <typename> typename... Ts
>
inline constexpr auto collect_to = collect_to_t<Coll, Ts...>();

namespace detail {

/**
 * Tag-type for many and many1.
 */
struct many_tag {};

/**
 * SFINAE for many types.
 */
template <typename T>
inline constexpr bool is_many_v = std::is_base_of_v<many_tag, T>;

} /* namespace detail */

template <typename P, typename To = collect_to_t<std::vector>>
class many_t : public combinator<many_t<P>>,
               private detail::many_tag {
private:
    cppcmb_self_check(many_t);

    template <typename Src>
    using value_t = typename To::template type<parser_value_t<P, Src>>;

    P m_Parser;

public:
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr many_t(PFwd&& p)
        noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
        : m_Parser(cppcmb_fwd(p)) {
    }

    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) &
        cppcmb_return(many_t<P, To2>(m_Parser))

    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) const&
        cppcmb_return(many_t<P, To2>(m_Parser))
    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) &&
        cppcmb_return(many_t<P, To2>(std::move(m_Parser)))
    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) const&&
        cppcmb_return(many_t<P, To2>(std::move(m_Parser)))

    cppcmb_getter(underlying, m_Parser)

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<value_t<Src>> {
        cppcmb_assert_parser(P, Src);

        using result_t = result<value_t<Src>>;

        std::size_t furthest = 0U;
        std::size_t matched = 0U;
        auto coll = value_t<Src>();
        auto rr = r;
        while (true) {
            auto p_inv = m_Parser.apply(rr);
            furthest = std::max(furthest, matched + p_inv.furthest());
            if (p_inv.is_failure()) {
                // Stop applying
                break;
            }
            auto p_succ = std::move(p_inv).success();
            matched += p_succ.matched();
            // Add to collection
            coll.push_back(std::move(p_succ).value());
            // Move reader
            rr.seek(rr.cursor() + p_succ.matched());
        }
        return result_t(success(std::move(coll), matched), furthest);
    }
};

template <typename PFwd>
many_t(PFwd) -> many_t<PFwd>;

/**
 * Operator for making many parser.
 */
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
[[nodiscard]] constexpr auto operator*(P&& p)
    cppcmb_return(many_t(cppcmb_fwd(p)))

/**
 * Operator to collect 'many' and 'many1' to a different container.
 */
template <typename P, typename To,
    cppcmb_requires_t(detail::is_many_v<detail::remove_cvref_t<P>>)>
[[nodiscard]] constexpr auto operator>>(P&& p, To to)
    cppcmb_return(cppcmb_fwd(p).collect_to(to))

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_MANY_HPP */
