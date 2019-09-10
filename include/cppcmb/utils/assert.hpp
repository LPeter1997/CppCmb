/**
 * assert.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT license.
 *
 * Simple assertions for the library (and it's development).
 */

#ifndef CPPCMB_UTILS_ASSERT_HPP
#define CPPCMB_UTILS_ASSERT_HPP

#include <cassert>

#define CPPCMB_ASSERT(...) assert((__VA_ARGS__))
#define CPPCMB_PANIC(message) CPPCMB_ASSERT(message, false)

#endif /* CPPCMB_UTILS_ASSERT_HPP */
