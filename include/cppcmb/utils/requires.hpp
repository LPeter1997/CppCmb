/**
 * requires.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT license.
 *
 * SFINAE helper macro.
 */

#ifndef CPPCMB_UTILS_REQUIRES_HPP
#define CPPCMB_UTILS_REQUIRES_HPP

#include <cstddef>
#include <type_traits>

#define CPPCMB_REQUIRES(...) CPPCMB_REQUIRES_IMPL(__LINE__, __VA_ARGS__)
#define CPPCMB_REQUIRES_IMPL(line, ...) CPPCMB_REQUIRES_IMPL1(line, __VA_ARGS__)
#define CPPCMB_REQUIRES_IMPL1(id, ...) \
bool cppcmb_req_##id = false,          \
::std::enable_if_t<cppcmb_req_##id || (__VA_ARGS__), ::std::nullptr_t> = nullptr

#endif /* CPPCMB_UTILS_REQUIRES_HPP */
