/**
 * apply_value.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * Applies a value to a given function.
 * This is used for applying values for thansformations. It automatically
 * dispatches sum-types and splats products.
 */

#ifndef CPPCMB_APPLY_VALUE_HPP
#define CPPCMB_APPLY_VALUE_HPP

#include <tuple>
#include <type_traits>
#include <variant>
#include "product.hpp"
#include "sum.hpp"

namespace cppcmb {

namespace detail {

// XXX(LPeter1997): Noexcept specifier
// Arg is a product
template <typename Fn, typename T>
[[nodiscard]] constexpr decltype(auto) apply_value_impl(
    std::true_type,
    std::false_type,
    Fn&& fn, T&& arg) {

    return std::apply(
        cppcmb_fwd(fn), cppcmb_fwd(arg).as_tuple()
    );
}

// XXX(LPeter1997): Noexcept specifier
// Arg is a sum
template <typename Fn, typename T>
[[nodiscard]] constexpr decltype(auto) apply_value_impl(
    std::false_type,
    std::true_type,
    Fn&& fn, T&& arg) {

    return std::visit(
        cppcmb_fwd(fn), cppcmb_fwd(arg).as_variant()
    );
}

// XXX(LPeter1997): Noexcept specifier
// Arg is a single value
template <typename Fn, typename T>
[[nodiscard]] constexpr decltype(auto) apply_value_impl(
    std::false_type,
    std::false_type,
    Fn&& fn, T&& arg) {

    return cppcmb_fwd(fn)(cppcmb_fwd(arg));
}

} /* namespace detail */

// XXX(LPeter1997): Noexcept specifier
// XXX(LPeter1997): Make apply recursive, so that underlying sums or
// products can get unwrapped too
template <typename Fn, typename T>
[[nodiscard]] constexpr decltype(auto) apply_value(Fn&& fn, T&& arg) {
    return detail::apply_value_impl(
        detail::is_product<detail::remove_cvref_t<T>>(),
        detail::is_sum<detail::remove_cvref_t<T>>(),
        cppcmb_fwd(fn),
        cppcmb_fwd(arg)
    );
}

} /* namespace cppcmb */

#endif /* CPPCMB_APPLY_VALUE_HPP */
