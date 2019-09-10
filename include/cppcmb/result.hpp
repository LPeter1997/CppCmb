/**
 * result.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT license.
 *
 * Here are the type definitions that the combinators return.
 */

#ifndef CPPCMB_RESULT_HPP
#define CPPCMB_RESULT_HPP

#include <iterator>
#include <type_traits>
#include <variant>
#include <cppcmb/utils/fwd.hpp>
#include <cppcmb/utils/requires.hpp>

namespace cppcmb {

class error {};

class ok {};

class result {
public:
    using ok_type = ok;
    using error_type = error;

private:
    using variant_type = std::variant<ok_type, error_type>;

public:
    template <typename TFwd,
        CPPCMB_REQUIRES(!std::is_same_v<std::decay_t<TFwd>, result>)>
    constexpr result(TFwd&& value)
        noexcept(std::is_nothrow_constructible_v<variant_type, TFwd&&>)
        : m_Result(CPPCMB_FWD(value)) {
    }

    [[nodiscard]] constexpr bool is_ok() const noexcept {
        return std::holds_alternative<ok_type>(m_Result);
    }

    [[nodiscard]] constexpr bool is_error() const noexcept {
        return std::holds_alternative<error_type>(m_Result);
    }

private:
    variant_type m_Result;
};

} /* namespace cppcmb */

#endif /* CPPCMB_RESULT_HPP */
