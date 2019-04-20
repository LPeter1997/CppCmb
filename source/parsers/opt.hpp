/**
 * opt.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A parser that wraps the underlying parse result into a maybe-type. Always
 * succeeds, but the maybe-type only contains a value if the underlying parser
 * succeeds.
 */

#ifndef CPPCMB_PARSERS_OPT_HPP
#define CPPCMB_PARSERS_OPT_HPP

#include "combinator.hpp"

namespace cppcmb {

template <typename P>
class opt_t : public combinator<opt_t<P>> {
private:
    cppcmb_self_check(opt_t);

    template <typename Src>
    using value_t = maybe<parser_value_t<P, Src>>;

    P m_Parser;

public:
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr opt_t(PFwd&& p)
        noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
        : m_Parser(cppcmb_fwd(p)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<value_t<Src>> {
        cppcmb_assert_parser(P, Src);

        using result_t = result<value_t<Src>>;

        auto p_inv = m_Parser.apply(r);
        if (p_inv.is_failure()) {
            return result_t(
                success(value_t<Src>(none()), 0U),
                p_inv.furthest()
            );
        }
        else {
            auto succ = std::move(p_inv).success();
            return result_t(
                success(
                    value_t<Src>(some(std::move(succ).value())),
                    succ.matched()
                ),
                p_inv.furthest()
            );
        }
    }
};

template <typename PFwd>
opt_t(PFwd) -> opt_t<PFwd>;

/**
 * Operator for making optional parser.
 */
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
[[nodiscard]] constexpr auto operator-(P&& p)
    cppcmb_return(opt_t(cppcmb_fwd(p)))

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_OPT_HPP */
