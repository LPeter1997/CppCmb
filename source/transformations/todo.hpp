/**
 * todo.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A TODO transformation that the user can write to check compilation while
 * there are still holes in the implementation. At runtime it will trigger an
 * assertion failure.
 */
#ifndef CPPCMB_TRANSFORMATIONS_TODO_HPP
#define CPPCMB_TRANSFORMATIONS_TODO_HPP

namespace cppcmb {

template <typename T>
struct todo_t  {
    template <typename... Ts>
    [[nodiscard]] constexpr T operator()(Ts&&...) const noexcept {
        cppcmb_panic("Unimplemented feature! (TODO used)");
        return *(T*)nullptr;
    }
};

// Value template
template <typename T>
inline constexpr auto todo = todo_t<T>();

} /* namespace cppcmb */

#endif /* CPPCMB_TRANSFORMATIONS_TODO_HPP */
