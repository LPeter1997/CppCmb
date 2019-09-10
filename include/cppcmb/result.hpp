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
#include <variant>

namespace cppcmb {

class error {};

class ok {};

class result {
public:
    using ok_type = ok;
    using error_type = error;

public:
    [[nodiscard]] constexpr bool is_ok() const noexcept {
        return std::holds_alternative<ok_type>(m_Result);
    }

    [[nodiscard]] constexpr bool is_error() const noexcept {
        return std::holds_alternative<error_type>(m_Result);
    }

private:
    std::variant<ok_type, error_type> m_Result;
};

} /* namespace cppcmb */

#endif /* CPPCMB_RESULT_HPP */
