/**
 * seq.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A combinator that sequences two parsers one after the other. Only applies the
 * second one if the first succeeded. Concatenates results in a product.
 */

#ifndef CPPCMB_PARSERS_SEQ_HPP
#define CPPCMB_PARSERS_SEQ_HPP

#include "combinator.hpp"
#include "../product.hpp"
#include "../result.hpp"

namespace cppcmb {

template <typename P1, typename P2>
class seq_t : public combinator<seq_t<P1, P2>> {
private:
    template <typename Src>
    using value_t = decltype(product_values(
        std::declval<parser_value_t<P1, Src>>(),
        std::declval<parser_value_t<P2, Src>>()
    ));

    P1 m_First;
    P2 m_Second;

public:
    template <typename P1Fwd, typename P2Fwd>
    constexpr seq_t(P1Fwd&& p1, P2Fwd&& p2)
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

        auto p1_inv = m_First.apply(r);
        if (p1_inv.is_failure()) {
            // Early failure, don't continue
            return result_t(std::move(p1_inv).failure(), p1_inv.furthest());
        }
        // Get the success alternative
        auto p1_succ = std::move(p1_inv).success();
        // Create the next reader
        auto r2 = reader(
            r.source(), r.cursor() + p1_succ.matched(), r.context_ptr()
        );
        // Invoke the second parser
        auto p2_inv = m_Second.apply(r2);
        // Max peek distance
        auto max_furthest = std::max(
            p1_inv.furthest(),
            p1_succ.matched() + p2_inv.furthest()
        );
        if (p2_inv.is_failure()) {
            // Second failed, fail on that error
            return result_t(
                std::move(p2_inv).failure(),
                max_furthest
            );
        }
        // Get the success alternative
        auto p2_succ = std::move(p2_inv).success();
        // Combine the values
        return result_t(
            success(
                product_values(
                    std::move(p1_succ).value(),
                    std::move(p2_succ).value()
                ),
                p1_succ.matched() + p2_succ.matched()
            ),
            max_furthest
        );
    }
};

template <typename P1Fwd, typename P2Fwd>
seq_t(P1Fwd, P2Fwd) -> seq_t<P1Fwd, P2Fwd>;

/**
 * Operator for making a sequence.
 */
template <typename P1, typename P2,
    cppcmb_requires_t(detail::all_combinators_cvref_v<P1, P2>)>
[[nodiscard]] constexpr auto operator&(P1&& p1, P2&& p2)
    cppcmb_return(seq_t(cppcmb_fwd(p1), cppcmb_fwd(p2)))

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_SEQ_HPP */
