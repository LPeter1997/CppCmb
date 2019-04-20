/**
 * combinator.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A base-type for all combinators. Used to inject the subscript operator to
 * apply transformations.
 */

#ifndef CPPCMB_COMBINATOR_HPP
#define CPPCMB_COMBINATOR_HPP

#include <type_traits>
#include <utility>
#include "detail.hpp"

namespace cppcmb {

namespace detail {

/**
 * A tag-type for every combinator, so it's easier to check inheritance.
 */
class combinator_base {};

} /* namespace detail */

/**
 * Check if a type correctly derives from the combinator base.
 * The user actually has to derive from combinator<Self>, but that already
 * derives from combinator base, so this check is sufficient.
 */
template <typename T>
using is_combinator = std::is_base_of<detail::combinator_base, T>;

template <typename T>
inline constexpr bool is_combinator_v = is_combinator<T>::value;

namespace detail {

/**
 * Helpers, mainly for operators.
 */

template <typename T>
inline constexpr bool is_combinator_cvref_v =
    is_combinator_v<remove_cvref_t<T>>;

template <typename... Ts>
inline constexpr bool all_combinators_cvref_v =
    (... & is_combinator_cvref_v<Ts>);

} /* namespace detail */

// Forward-declare the action combinator, the base combinator has to see it
template <typename Cmb, typename Fn>
class action_t;

#define cppcmb_noexcept_subscript(...) \
noexcept(noexcept(action_t(std::declval<__VA_ARGS__>(), cppcmb_fwd(fn))))

/**
 * The actual type that all other combinators have to derive from.
 */
template <typename Self>
class combinator : private detail::crtp<Self>,
                   private detail::combinator_base {
public:
    template <typename Fn>
    [[nodiscard]] constexpr auto operator[](Fn&& fn) &
        cppcmb_noexcept_subscript(Self&) {

        return action_t(this->self(), cppcmb_fwd(fn));
    }

    template <typename Fn>
    [[nodiscard]] constexpr auto operator[](Fn&& fn) const&
        cppcmb_noexcept_subscript(Self const&) {

        return action_t(this->self(), cppcmb_fwd(fn));
    }

    template <typename Fn>
    [[nodiscard]] constexpr auto operator[](Fn&& fn) &&
        cppcmb_noexcept_subscript(Self&&) {

        return action_t(std::move(this->self()), cppcmb_fwd(fn));
    }

    template <typename Fn>
    [[nodiscard]] constexpr auto operator[](Fn&& fn) const&&
        cppcmb_noexcept_subscript(Self const&&) {

        return action_t(std::move(this->self()), cppcmb_fwd(fn));
    }
};

#undef cppcmb_noexcept_subscript

namespace detail {

/**
 * Concept check for parser interface.
 */
template <typename T, typename Src>
using apply_t = decltype(
    std::declval<T>().apply(std::declval<reader<Src> const&>())
);

template <typename T, typename Src>
inline constexpr bool has_parser_interface_v =
       is_detected_v<apply_t, T, Src>,
    && is_combinator_v<T>;

} /* namespace detail */

/**
 * Every parser can use this at the beginning of the apply function to check
 * sub-parsers.
 */
#define cppcmb_assert_parser(p, src)                  \
static_assert(                                        \
    ::cppcmb::detail::has_parser_interface_v<p, src>, \
    "A parser must be derived from combinator<Self> " \
    " and have a member function apply(reader<Src>)!" \
    " (note: apply has to be const-qualified!)"       \
)

/**
 * Helper to get the parser result.
 */
template <typename P, typename Src>
using parser_result_t = detail::remove_cvref_t<decltype(
    std::declval<P>().apply(std::declval<reader<Src> const&>())
)>;

/**
 * Helper to get the result value of a parse success.
 */
template <typename P, typename Src>
using parser_value_t = detail::remove_cvref_t<decltype(
    std::declval<parser_result_t<P, Src>>().success().value()
)>;

} /* namespace cppcmb */

#endif /* CPPCMB_COMBINATOR_HPP */
