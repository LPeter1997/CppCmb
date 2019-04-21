/**
 * todo.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A TODO parser that the user can write to check compilation while there are
 * still holes in the implementation. At runtime it will trigger an assertion
 * failure.
 */

#ifndef CPPCMB_PARSERS_TODO_HPP
#define CPPCMB_PARSERS_TODO_HPP

#include "epsilon.hpp"
#include "../transformations/todo.hpp"

namespace cppcmb {

// XXX(LPeter1997): Maybe it's better to explicitly implement the parser for
// better error messages?

template <typename T>
inline constexpr auto todo_parser = epsilon[todo<T>];

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_TODO_HPP */
