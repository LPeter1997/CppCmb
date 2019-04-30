/**
 * alt.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A combinator that tries to apply the first alternative. If it fails, it tries
 * the second.
 */

#ifndef CPPCMB_PARSERS_ALT_HPP
#define CPPCMB_PARSERS_ALT_HPP

#include "combinator.hpp"
#include "../result.hpp"
#include "../sum.hpp"

namespace cppcmb {

/**
 * A tag-type for a more uniform alternative syntax.
 * This can be put as the first element of an alternative chain so every new
 * line can start with the alternative operator. It's completely ignored.
 * Example:
 * auto parser = pass
 *             | first
 *             | second
 *             ;
 */
struct pass_t {};

inline constexpr auto pass = pass_t();

template <typename P1, typename P2>
class alt_t : public combinator<alt_t<P1, P2>> {
private:
    template <typename Src>
    using value_t = sum_values_t<
        parser_value_t<P1, Src>,
        parser_value_t<P2, Src>
    >;

    P1 m_First;
    P2 m_Second;

public:
    template <typename P1Fwd, typename P2Fwd>
    constexpr alt_t(P1Fwd&& p1, P2Fwd&& p2)
        noexcept(
            std::is_nothrow_constructible_v<P1, P1Fwd&&>
         && std::is_nothrow_constructible_v<P2, P2Fwd&&>
        )
        : m_First(cppcmb_fwd(p1)), m_Second(cppcmb_fwd(p2)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<value_t<Src>> {
        cppcmb_assert_parser(P1, Src);
        cppcmb_assert_parser(P2, Src);

        using result_t = result<value_t<Src>>;

        // Try to apply the first alternative
        auto p1_inv = m_First.apply(r);
        if (p1_inv.is_success()) {
            auto p1_succ = std::move(p1_inv).success();
            return result_t(
                success(
                    sum_values<value_t<Src>>(std::move(p1_succ).value()),
                    p1_succ.matched()
                ),
                p1_inv.furthest()
            );
        }

        // Try to apply the second alternative
        auto p2_inv = m_Second.apply(r);
        if (p2_inv.is_success()) {
            auto p2_succ = std::move(p2_inv).success();
            return result_t(
                success(
                    sum_values<value_t<Src>>(std::move(p2_succ).value()),
                    p2_succ.matched()
                ),
                std::max(p1_inv.furthest(), p2_inv.furthest())
            );
        }

        // Both failed, return the error which got further
        auto p1_err = std::move(p1_inv).failure();
        auto p2_err = std::move(p2_inv).failure();

        if (p1_inv.furthest() > p2_inv.furthest()) {
            return result_t(std::move(p1_err), p1_inv.furthest());
        }
        if (p1_inv.furthest() < p2_inv.furthest()) {
            return result_t(std::move(p2_err), p2_inv.furthest());
        }
        // They got to the same distance, need to merge errors
        // XXX(LPeter1997): Implement, for now we just return the first
        return result_t(std::move(p1_err), p1_inv.furthest());
    }
};

template <typename P1Fwd, typename P2Fwd>
alt_t(P1Fwd, P2Fwd) -> alt_t<P1Fwd, P2Fwd>;

/**
 * Operator for making alternatives.
 */
template <typename P1, typename P2,
    cppcmb_requires_t(detail::all_combinators_cvref_v<P1, P2>)>
[[nodiscard]] constexpr auto operator|(P1&& p1, P2&& p2)
    cppcmb_return(alt_t(cppcmb_fwd(p1), cppcmb_fwd(p2)))

/**
 * Ignore pass.
 */
template <typename P2,
    cppcmb_requires_t(detail::is_combinator_cvref_v<P2>)>
[[nodiscard]] constexpr auto operator|(pass_t, P2&& p2)
    cppcmb_return(cppcmb_fwd(p2))

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_ALT_HPP */
