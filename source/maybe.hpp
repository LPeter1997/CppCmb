/**
 * maybe.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A generic Maybe type that's either is some value, or nothing.
 * Just like std::optional but with type-constructors.
 */

#ifndef CPPCMB_MAYBE_HPP
#define CPPCMB_MAYBE_HPP

#include <variant>
#include "detail.hpp"

namespace cppcmb {

/**
 * Some type-constructor for maybe.
 */
template <typename T>
class some {
private:
    cppcmb_self_check(some);

    T m_Value;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename TFwd, cppcmb_requires_t(!is_self_v<TFwd>)>
    constexpr some(TFwd&& val)
        : m_Value(cppcmb_fwd(val)) {
    }

    cppcmb_getter(value, m_Value)
};

template <typename TFwd>
some(TFwd) -> some<TFwd>;

/**
 * None type-constructor for maybe.
 */
class none {};

/**
 * Generic maybe-type.
 */
template <typename T>
class maybe {
private:
    cppcmb_self_check(maybe);

    using some_type = ::cppcmb::some<T>;
    using none_type = ::cppcmb::none;

    std::variant<some_type, none_type> m_Data;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename TFwd, cppcmb_requires_t(!is_self_v<TFwd>)>
    constexpr maybe(TFwd&& val)
        : m_Data(cppcmb_fwd(val)) {
    }

    [[nodiscard]] constexpr bool is_some() const noexcept {
        return std::holds_alternative<some_type>(m_Data);
    }

    [[nodiscard]] constexpr bool is_none() const noexcept {
        return std::holds_alternative<none_type>(m_Data);
    }

    cppcmb_getter(some, std::get<some_type>(m_Data))
    cppcmb_getter(none, std::get<none_type>(m_Data))
};

namespace detail {

cppcmb_is_specialization(maybe);

} /* namespace detail */

} /* namespace cppcmb */

#endif /* CPPCMB_MAYBE_HPP */
