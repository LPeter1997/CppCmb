/**
 * action.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * The action/transformation combinator that transforms the successful result
 * with a user-provided function.
 */

#ifndef CPPCMB_PARSERS_ACTION_HPP
#define CPPCMB_PARSERS_ACTION_HPP

#include <type_traits>
#include <utility>
#include "combinator.hpp"
#include "../result.hpp"

namespace cppcmb {

namespace detail {

/**
 * Just to improve error messages.
 */
struct action_apply_helper {
    template <typename Fn, typename T>
    constexpr auto operator()(Fn const& f, T&& v) const
        cppcmb_return(apply_value(f, cppcmb_fwd(v)))
};

} /* namespace detail */

template <typename P, typename Fn>
class action_t : public combinator<action_t<P, Fn>> {
private:
    P  m_Parser;
    Fn m_Fn;

public:
    // XXX(LPeter1997): Is it right to have such a long noexcept specifier?
    template <typename PFwd, typename FnFwd>
    constexpr action_t(PFwd&& cmb, FnFwd&& fn)
        noexcept(
            std::is_nothrow_constructible_v<P, PFwd&&>
         && std::is_nothrow_constructible_v<Fn, FnFwd&&>
        )
        : m_Parser(cppcmb_fwd(cmb)), m_Fn(cppcmb_fwd(fn)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr decltype(auto) apply(reader<Src> const& src) const {
        cppcmb_assert_parser(P, Src);

        using value_t = parser_value_t<P, Src>;
        using apply_t = decltype(&action_t::apply_fn<value_t&&>);
        using fn_result_t = std::invoke_result_t<apply_t, action_t, value_t>;
        using dispatch_tag =
            detail::is_maybe<detail::remove_cvref_t<fn_result_t>>;

        return apply_impl<fn_result_t>(src, dispatch_tag());
    }

private:
    /**
     * Original solution:
     *
     * template <typename T>
     * using maybe_value_t = detail::remove_cvref_t<decltype(
     *      std::declval<T>().some().value()
     * )>;
     *
     * But it triggered a GCC internal compiler error.
     */
    template <typename T>
    using maybe_some_t = typename detail::remove_cvref_t<T>::some_type;

    template <typename T>
    using maybe_value_t = typename maybe_some_t<T>::value_type;

    // XXX(LPeter1997): Noexcept specifier
    // Action can fail
    template <typename FRes, typename Src>
    [[nodiscard]] constexpr auto apply_impl(
        reader<Src> const& src,
        std::true_type) const -> result<maybe_value_t<FRes>> {

        using result_t = result<maybe_value_t<FRes>>;

        auto inv = m_Parser.apply(src);
        if (inv.is_failure()) {
            // Early failure
            return result_t(std::move(inv).failure(), inv.furthest());
        }

        auto succ = std::move(inv).success();
        // Try to apply the action
        auto act_inv = apply_fn(std::move(succ).value());
        // In any case we will have to decorate the result with the position
        if (act_inv.is_none()) {
            return result_t(failure(), inv.furthest());
        }
        else {
            return result_t(
                success(std::move(act_inv).some().value(), succ.matched()),
                inv.furthest()
            );
        }
    }

    // XXX(LPeter1997): Noexcept specifier
    // Action can't fail
    template <typename FRes, typename Src>
    [[nodiscard]] constexpr auto apply_impl(
        reader<Src> const& src,
        std::false_type) const -> result<FRes> {

        auto inv = m_Parser.apply(src);
        if (inv.is_failure()) {
            // Early failure
            return result<FRes>(
                std::move(inv).failure(),
                inv.furthest()
            );
        }

        auto succ = std::move(inv).success();
        // Apply the action
        auto act_val = apply_fn(std::move(succ).value());
        // Wrap it in a success
        return result<FRes>(
            success(std::move(act_val), succ.matched()),
            inv.furthest()
        );
    }

    // Invoke the function with a value
    template <typename T>
    [[nodiscard]] constexpr decltype(auto) apply_fn(T&& val) const {
        static_assert(
            std::is_invocable_v<detail::action_apply_helper, Fn, T&&>,
             "The given action function must be invocable with the parser's "
             "successful value type! "
             "(note: the function's invocation must be const-qualified!)"
        );
        return apply_value(m_Fn, cppcmb_fwd(val));
    }
};

template <typename PFwd, typename FnFwd>
action_t(PFwd, FnFwd) -> action_t<PFwd, FnFwd>;

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_ACTION_HPP */
