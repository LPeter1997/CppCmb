/**
 * is_detected.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * Implementation of the detector idiom.
 * See: https://en.cppreference.com/w/cpp/experimental/is_detected
 */

#ifndef CPPCMB_DETAIL_IS_DETECTED_HPP
#define CPPCMB_DETAIL_IS_DETECTED_HPP

#include <type_traits>

namespace cppcmb {
namespace detail {

/**
 * @see https://en.cppreference.com/w/cpp/experimental/nonesuch
 */
struct nonesuch {
    ~nonesuch()                     = delete;
    nonesuch(nonesuch const&)       = delete;
    void operator=(nonesuch const&) = delete;
};

/**
 * @see https://en.cppreference.com/w/cpp/experimental/is_detected
 */
template <typename Default, typename AlwaysVoid,
    template <typename...> typename Op, typename... Args>
struct detector {
    using value_t = std::false_type;
    using type = Default;
};

template <typename Default,
    template <typename...> typename Op, typename... Args>
struct detector<Default, std::void_t<Op<Args...>>, Op, Args...> {
    using value_t = std::true_type;
    using type = Op<Args...>;
};

template <template <typename...> typename Op, typename... Args>
using is_detected = typename detector<nonesuch, void, Op, Args...>::value_t;

template <template <typename...> typename Op, typename... Args>
using detected_t = typename detector<nonesuch, void, Op, Args...>::type;

template <typename Default,
    template <typename...> typename Op, typename... Args>
using detected_or = detector<Default, void, Op, Args...>;

// Additional utilities

template <template <typename...> typename Op, typename... Args>
inline constexpr auto is_detected_v = is_detected<Op, Args...>::value;

template <typename Default,
    template <typename...> typename Op, typename... Args>
using detected_or_t = typename detected_or<Default, Op, Args...>::type;

template <typename Expected,
    template <typename...> typename Op, typename... Args>
using is_detected_exact = std::is_same<Expected, detected_t<Op, Args...>>;

template <typename Expected,
    template <typename...> typename Op, typename... Args>
inline constexpr auto is_detected_exact_v =
    is_detected_exact<Expected, Op, Args...>::value;

template <typename To, template <typename...> typename Op, typename... Args>
using is_detected_convertible =
    std::is_convertible<detected_t<Op, Args...>, To>;

template <typename To, template <typename...> typename Op, typename... Args>
inline constexpr auto is_detected_convertible_v =
    is_detected_convertible<To, Op, Args...>::value;

} /* namespace detail */
} /* namespace cppcmb */

#endif /* CPPCMB_DETAIL_IS_DETECTED_HPP */
