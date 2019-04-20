/**
 * sum.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A variant-like sum-type that stores one alternative of possible types.
 */

#ifndef CPPCMB_SUM_HPP
#define CPPCMB_SUM_HPP

#include <type_traits>
#include <utility>
#include <variant>
#include "detail.hpp"

namespace cppcmb {

template <typename... Ts>
class sum {
private:
    using variant_type = std::variant<Ts...>;

    cppcmb_self_check(sum);

    variant_type m_Value;

public:
    template <typename T, cppcmb_requires_t(!is_self_v<T>)>
    constexpr sum(T&& val)
        noexcept(std::is_nothrow_constructible_v<variant_type, T&&>)
        : m_Value(cppcmb_fwd(val)) {
    }

    template <typename U>
    [[nodiscard]] constexpr auto& get() & {
        return std::get<U>(m_Value);
    }
    template <typename U>
    [[nodiscard]] constexpr auto const& get() const& {
        return std::get<U>(m_Value);
    }
    template <typename U>
    [[nodiscard]] constexpr auto&& get() && {
        return std::get<U>(std::move(m_Value));
    }
    template <typename U>
    [[nodiscard]] constexpr auto const&& get() const&& {
        return std::get<U>(std::move(m_Value));
    }

    cppcmb_getter(as_variant, m_Value)
};

/**
 * Make sums comparable.
 */
template <typename... Ts>
[[nodiscard]] constexpr auto operator==(
    sum<Ts...> const& l,
    sum<Ts...> const& r
) cppcmb_return(l.as_variant() == r.as_variant())

template <typename... Ts>
[[nodiscard]] constexpr auto operator!=(
    sum<Ts...> const& l,
    sum<Ts...> const& r
) cppcmb_return(l.as_variant() != r.as_variant())

namespace detail {

cppcmb_is_specialization(sum);

template <typename T, typename... Ts>
inline constexpr bool contains_type_v = (... || std::is_same_v<T, Ts>);

////////////////////////////////////////////////////

template <typename...>
struct sum_values_t_impl;

template <typename T>
struct sum_values_t_impl<sum<T>> {
    using type = T;
};

template <typename... Ts>
struct sum_values_t_impl<sum<Ts...>> {
    using type = sum<Ts...>;
};

template <typename... Ts, typename... Us, typename... Vs>
struct sum_values_t_impl<sum<Ts...>, sum<Us...>, Vs...> {
    using type = typename sum_values_t_impl<sum<Ts...>, Us..., Vs...>::type;
};

template <typename... Ts, typename Head, typename... Tail>
struct sum_values_t_impl<sum<Ts...>, Head, Tail...> {
    using type = std::conditional_t<
        contains_type_v<Head, Ts...>,
        typename sum_values_t_impl<sum<Ts...>, Tail...>::type,
        typename sum_values_t_impl<sum<Ts..., Head>, Tail...>::type
    >;
};

} /* namespace detail */

template <typename... Ts>
using sum_values_t = typename detail::sum_values_t_impl<sum<>, Ts...>::type;

namespace detail {

// XXX(LPeter1997): Noexcept specifier
template <typename RetT, typename T>
constexpr auto sum_values_impl(std::false_type, T&& val) {
    return RetT(cppcmb_fwd(val));
}

// XXX(LPeter1997): Noexcept specifier
template <typename RetT, typename T>
constexpr decltype(auto) sum_values_impl(std::true_type, T&& val) {
    return std::visit(
        [](auto&& v) -> RetT { return RetT(cppcmb_fwd(v)); },
        cppcmb_fwd(val).as_variant()
    );
}

} /* namespace detail */

// XXX(LPeter1997): Noexcept specifier
template <typename... Ts, typename T>
constexpr auto sum_values(T&& val) {
    return detail::sum_values_impl<sum_values_t<Ts...>>(
        detail::is_sum<detail::remove_cvref_t<T>>(),
        cppcmb_fwd(val)
    );
}

} /* namespace cppcmb */

#endif /* CPPCMB_SUM_HPP */
