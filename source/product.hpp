/**
 * product.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A tuple-like type that stores a product/sequence of values.
 */

#ifndef CPPCMB_PRODUCT_HPP
#define CPPCMB_PRODUCT_HPP

#include <cstddef>
#include <tuple>
#include <type_traits>
#include <utility>
#include "detail.hpp"

namespace cppcmb {

template <typename... Ts>
class product {
private:
    using tuple_type = decltype(std::make_tuple(std::declval<Ts>()...));

    cppcmb_self_check(product);

    tuple_type m_Value;

public:
    static constexpr auto index_sequence =
        std::make_index_sequence<sizeof...(Ts)>();

    template <typename T,
        cppcmb_requires_t(sizeof...(Ts) == 1 && !is_self_v<T>)>
    constexpr product(T&& val)
        noexcept(std::is_nothrow_constructible_v<tuple_type, T&&>)
        : m_Value(cppcmb_fwd(val)) {
    }

    template <typename... Us, cppcmb_requires_t(sizeof...(Us) != 1)>
    constexpr product(Us&&... vals)
        noexcept(std::is_nothrow_constructible_v<tuple_type, Us&&...>)
        : m_Value(cppcmb_fwd(vals)...) {
    }

    template <std::size_t Idx>
    [[nodiscard]] constexpr auto& get() & noexcept {
        return std::get<Idx>(m_Value);
    }
    template <std::size_t Idx>
    [[nodiscard]] constexpr auto const& get() const& noexcept {
        return std::get<Idx>(m_Value);
    }
    template <std::size_t Idx>
    [[nodiscard]] constexpr auto&& get() && noexcept {
        return std::get<Idx>(std::move(m_Value));
    }
    template <std::size_t Idx>
    [[nodiscard]] constexpr auto const&& get() const&& noexcept {
        return std::get<Idx>(std::move(m_Value));
    }

    cppcmb_getter(as_tuple, m_Value)
};

template <typename... Ts>
product(Ts...) -> product<Ts...>;

// To fix a GCC bug
template <typename T, typename... Ts>
product(T, Ts...) -> product<T, Ts...>;

/**
 * Make products comparable.
 */
template <typename... Ts, typename... Us>
[[nodiscard]] constexpr auto operator==(
    product<Ts...> const& l,
    product<Us...> const& r
) cppcmb_return(l.as_tuple() == r.as_tuple())

template <typename... Ts, typename... Us>
[[nodiscard]] constexpr auto operator!=(
    product<Ts...> const& l,
    product<Us...> const& r
) cppcmb_return(l.as_tuple() != r.as_tuple())

namespace detail {

cppcmb_is_specialization(product);

// Base-case for sizeof...(Ts) != 1
template <typename... Ts>
[[nodiscard]]
constexpr auto product_values_impl(product<Ts...>&& res) noexcept {
    // XXX(LPeter1997): Is this right?
    // XXX(LPeter1997): Is the noexcept specifier right?
    return std::move(res);
}

// Base-case for exactly one element
// XXX(LPeter1997): Noexcept specifier
template <typename T>
[[nodiscard]] constexpr auto product_values_impl(product<T>&& res) {
    return std::move(res).template get<0>();
}

// XXX(LPeter1997): Noexcept specifier
template <typename... Ts, typename Head, typename... Tail>
[[nodiscard]] constexpr auto
product_values_impl(product<Ts...>&& res, Head&& h, Tail&&... t);

// XXX(LPeter1997): Noexcept specifier
template <std::size_t... Is, typename... Ts, typename Head, typename... Tail>
[[nodiscard]] constexpr auto product_values_expand(
    std::index_sequence<Is...>,
    product<Ts...>&& res, Head&& h, Tail&&... t) {

    return product_values_impl(
        std::move(res),
        cppcmb_fwd(h).template get<Is>()...,
        cppcmb_fwd(t)...
    );
}

// XXX(LPeter1997): Noexcept specifier
template <std::size_t... Is, typename... Ts, typename Head, typename... Tail>
[[nodiscard]] constexpr auto product_values_append(
    std::index_sequence<Is...>,
    product<Ts...>&& res, Head&& h, Tail&&... t) {

    return product_values_impl(
        product(std::move(res).template get<Is>()..., cppcmb_fwd(h)),
        cppcmb_fwd(t)...
    );
}

// XXX(LPeter1997): Noexcept specifier
template <typename... Ts, typename Head, typename... Tail>
[[nodiscard]] constexpr auto product_values_head(
    std::true_type,
    product<Ts...>&& res, Head&& h, Tail&&... t) {

    return product_values_expand(
        remove_cvref_t<Head>::index_sequence,
        std::move(res),
        cppcmb_fwd(h),
        cppcmb_fwd(t)...
    );
}

// XXX(LPeter1997): Noexcept specifier
template <typename... Ts, typename Head, typename... Tail>
[[nodiscard]] constexpr auto product_values_head(
    std::false_type,
    product<Ts...>&& res, Head&& h, Tail&&... t) {

    return product_values_append(
        product<Ts...>::index_sequence,
        std::move(res),
        cppcmb_fwd(h),
        cppcmb_fwd(t)...
    );
}

// XXX(LPeter1997): Noexcept specifier
template <typename... Ts, typename Head, typename... Tail>
[[nodiscard]] constexpr auto
product_values_impl(product<Ts...>&& res, Head&& h, Tail&&... t) {
    return product_values_head(
        is_product<remove_cvref_t<Head>>(),
        std::move(res),
        cppcmb_fwd(h),
        cppcmb_fwd(t)...
    );
}

} /* namespace detail */

// XXX(LPeter1997): Noexcept specifier
/**
 * Concatenate products and values.
 */
template <typename... Ts>
[[nodiscard]] constexpr auto product_values(Ts&&... vs) {
    // XXX(LPeter1997): GCC bug
    return detail::product_values_impl(product<>(), cppcmb_fwd(vs)...);
}

} /* namespace cppcmb */

#endif /* CPPCMB_PRODUCT_HPP */
