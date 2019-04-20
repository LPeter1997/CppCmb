/**
 * remove_cvref.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * Implementation of std::remove_cvref.
 * See: https://en.cppreference.com/w/cpp/types/remove_cvref
 */

#ifndef CPPCMB_DETAIL_REMOVE_CVREF_HPP
#define CPPCMB_DETAIL_REMOVE_CVREF_HPP

#include <type_traits>

namespace cppcmb {
namespace detail {

template <typename T>
struct remove_cvref : std::remove_cv<std::remove_reference_t<T>> {};

template <typename T>
using remove_cvref_t = typename remove_cvref<T>::type;

} /* namespace detail */
} /* namespace cppcmb */

#endif /* CPPCMB_DETAIL_REMOVE_CVREF_HPP */
