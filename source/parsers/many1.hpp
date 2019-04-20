/**
 * many1.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * Applies it's parser as many times as it can. Collects the results into a
 * collection (std::vector by default). Only succeeds when there is at least one
 * element.
 */

#ifndef CPPCMB_PARSERS_MANY1_HPP
#define CPPCMB_PARSERS_MANY1_HPP

#include "many.hpp"

namespace cppcmb {

template <typename P, typename To = collect_to_t<std::vector>>
class many1_t : public combinator<many1_t<P>>,
                private detail::many_tag {
private:
    cppcmb_self_check(many1_t);

    template <typename Src>
    using value_t = parser_value_t<many_t<P, To>, Src>;

    many_t<P, To> m_Parser;

public:
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr many1_t(PFwd&& p)
        noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
        : m_Parser(many_t<P, To>(cppcmb_fwd(p))) {
    }

    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) &
        cppcmb_return(many1_t<P, To2>(m_Parser.underlying()))
    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) const&
        cppcmb_return(many1_t<P, To2>(m_Parser.underlying()))
    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) &&
        cppcmb_return(many1_t<P, To2>(std::move(m_Parser).underlying()))
    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) const&&
        cppcmb_return(many1_t<P, To2>(std::move(m_Parser).underlying()))

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<value_t<Src>> {
        cppcmb_assert_parser(P, Src);

        using result_t = result<value_t<Src>>;

        auto p_inv = m_Parser.apply(r);

        cppcmb_assert(
            "The underlying 'many' parser must always succeed!",
            p_inv.is_success()
        );

        auto p_succ = std::move(p_inv).success();
        if (p_succ.value().size() > 0) {
            // Succeed
            return result_t(std::move(p_succ), p_inv.furthest());
        }
        else {
            // Fail
            return result_t(failure(), p_inv.furthest());
        }
    }
};

template <typename PFwd>
many1_t(PFwd) -> many1_t<PFwd>;

/**
 * Operator for making many1 parser.
 */
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
[[nodiscard]] constexpr auto operator+(P&& p)
    cppcmb_return(many1_t(cppcmb_fwd(p)))

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_MANY1_HPP */
