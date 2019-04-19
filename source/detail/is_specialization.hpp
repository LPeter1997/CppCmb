/**
 * is_specialization.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * Checks if a type is a specialization of a template.
 */

#ifndef CPPCMB_DETAIL_IS_SPECIALIZATION_HPP
#define CPPCMB_DETAIL_IS_SPECIALIZATION_HPP

#include <type_traits>

namespace cppcmb {
namespace detail {

template <typename, template <typename...> typename>
struct is_specialization : std::false_type {};

template <template <typename...> typename T, typename... Ts>
struct is_specialization<T<Ts...>, T> : std::true_type {};

template <typename T, template <typename...> typename Templ>
inline constexpr bool is_specialization_v = is_specialization<T, Templ>::value;

} /* namespace detail */
} /* namespace cppcmb */

#endif /* CPPCMB_DETAIL_IS_SPECIALIZATION_HPP */
