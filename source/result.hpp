/**
 * result.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A generic Either-like type that's either a parse success or a parse failure.
 * Just like std::variant<A, B> but with type-constructors.
 */

#ifndef CPPCMB_RESULT_HPP
#define CPPCMB_RESULT_HPP

#include <cstddef>
#include <type_traits>
#include <variant>
#include "detail.hpp"

namespace cppcmb {

/**
 * Success "type-constructor". The type that the parser returns when it
 * succeeded.
 */
template <typename T>
class success {
public:
    using value_type = T;

private:
    value_type  m_Value;
    std::size_t m_Matched;

public:
    template <typename TFwd>
    constexpr success(TFwd&& val, std::size_t matched)
        noexcept(std::is_nothrow_constructible_v<value_type, TFwd&&>)
        : m_Value(cppcmb_fwd(val)), m_Matched(matched) {
    }

    cppcmb_getter(value, m_Value)

    [[nodiscard]] constexpr auto const& matched() const noexcept {
        return m_Matched;
    }
};

template <typename TFwd>
success(TFwd, std::size_t) -> success<TFwd>;

/**
 * Failure "type-constructor". The type that the parser returns when it fails.
 */
class failure { };

/**
 * The result type of a parser. It's either a success or a failure type.
 */
template <typename T>
class result {
public:
    using success_type = ::cppcmb::success<T>;
    using failure_type = ::cppcmb::failure;

private:
    using either_type = std::variant<success_type, failure_type>;

    /**
     * The packrat parsers will have to fiddle with the furthest values.
     */
    template <typename>
    friend class drec_packrat_t;
    template <typename>
    friend class irec_packrat_t;

    either_type m_Data;
    std::size_t m_Furthest;

public:
    template <typename TFwd>
    constexpr result(TFwd&& val, std::size_t furthest)
        noexcept(std::is_nothrow_constructible_v<either_type, TFwd&&>)
        : m_Data(cppcmb_fwd(val)), m_Furthest(furthest) {
    }

    [[nodiscard]] constexpr bool is_success() const noexcept {
        return std::holds_alternative<success_type>(m_Data);
    }

    [[nodiscard]] constexpr bool is_failure() const noexcept {
        return std::holds_alternative<failure_type>(m_Data);
    }

    cppcmb_getter(success, std::get<success_type>(m_Data))
    cppcmb_getter(failure, std::get<failure_type>(m_Data))

    [[nodiscard]] constexpr auto const& furthest() const noexcept {
        return m_Furthest;
    }
};

} /* namespace cppcmb */

#endif /* CPPCMB_RESULT_HPP */
