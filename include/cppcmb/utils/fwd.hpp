/**
 * fwd.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT license.
 *
 * A simple forward-macro for the common cases.
 */

#ifndef CPPCMB_UTILS_FWD_HPP
#define CPPCMB_UTILS_FWD_HPP

#include <utility>

#define CPPCMB_FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

#endif /* CPPCMB_UTILS_FWD_HPP */
