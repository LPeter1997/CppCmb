/**
 * macros.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * Common macros used in the library.
 */

#ifndef CPPCMB_DETAIL_MACROS_HPP
#define CPPCMB_DETAIL_MACROS_HPP

#include <cassert>
#include <cstddef>
#include <type_traits>
#include <utility>
#include "remove_cvref.hpp"

namespace cppcmb {
namespace detail {

/**
 * Assertion with a custom message.
 */
#define cppcmb_assert(msg, ...) assert(((void)msg, (__VA_ARGS__)))

/**
 * Concatenates two tokens.
 */
#define cppcmb_cat(x, y) cppcmb_prelude_cat(x, y)

/**
 * Generates a unique identifier with a given prefix.
 */
#define cppcmb_unique_id(prefix) cppcmb_cat(prefix, __LINE__)

/**
 * Simplifies forwarding syntax, we don't have to provide the template argument.
 */
#define cppcmb_fwd(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

/**
 * A simple concept emulation macro. Does SFINAE in a safe way.
 */
#define cppcmb_requires_t(...) \
cppcmb_prelude_requires_t1(cppcmb_unique_id(cppcmb_concept_req), __VA_ARGS__)

/**
 * Generates an is_self_v template variable so the types won't get littered with
 * the same self-check code.
 */
#define cppcmb_self_check(type) \
cppcmb_prelude_self_check(type, cppcmb_unique_id(cppcmb_self_type))

/**
 * Generates specialization-check for a template-type.
 */
#define cppcmb_is_specialization(type) \
cppcmb_prelude_is_specialization(type, cppcmb_unique_id(cppcmb_tspec_type))

/**
 * Generates a getter with all member-qualifiers for owned values.
 */
#define cppcmb_getter(name, ...)                                        \
[[nodiscard]]                                                           \
constexpr auto& name() & noexcept(noexcept(__VA_ARGS__)) {              \
    return __VA_ARGS__;                                                 \
}                                                                       \
[[nodiscard]]                                                           \
constexpr auto const& name() const& noexcept(noexcept(__VA_ARGS__)) {   \
    return __VA_ARGS__;                                                 \
}                                                                       \
[[nodiscard]]                                                           \
constexpr auto&& name() && noexcept(noexcept(__VA_ARGS__)) {            \
    return std::move(__VA_ARGS__);                                      \
}                                                                       \
[[nodiscard]]                                                           \
constexpr auto const&& name() const&& noexcept(noexcept(__VA_ARGS__)) { \
    return std::move(__VA_ARGS__);                                      \
}

/**
 * Returns an expression with automatic noexcept qualifier. Only for computed,
 * non-owned values.
 */
#define cppcmb_return(...) \
noexcept(noexcept(__VA_ARGS__)) -> decltype(__VA_ARGS__) { return __VA_ARGS__; }

// Macro details

#define cppcmb_prelude_cat(x, y) x ## y

#define cppcmb_prelude_requires_t1(id, ...) 						\
bool id = false,                                                    \
::std::enable_if_t<id || (__VA_ARGS__), ::std::nullptr_t> = nullptr

#define cppcmb_prelude_self_check(type, id)                      \
template <typename id>                                           \
static constexpr bool is_self_v =                                \
    ::std::is_same_v<::cppcmb::detail::remove_cvref_t<id>, type>

#define cppcmb_prelude_is_specialization(type, id)               \
template <typename id>                                           \
using is_##type = ::cppcmb::detail::is_specialization<id, type>; \
template <typename id>                                           \
inline constexpr bool is_##type##_v = is_##type<id>::value

} /* namespace detail */
} /* namespace cppcmb */

#endif /* CPPCMB_DETAIL_MACROS_HPP */
