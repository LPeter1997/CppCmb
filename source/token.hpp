/**
 * token.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A generic token type.
 */

#ifndef CPPCMB_TOKEN_HPP
#define CPPCMB_TOKEN_HPP

#include <string_view>

namespace cppcmb {

template <typename CharT, typename Tag>
class token {
private:
    std::basic_string_view<CharT> m_Content;
    Tag                           m_Type;

public:
    constexpr token(std::basic_string_view<CharT> cont, Tag ty) noexcept
        : m_Content(cont), m_Type(ty) {
    }

    [[nodiscard]] constexpr auto const& content() const noexcept {
        return m_Content;
    }

    [[nodiscard]] constexpr auto const& type() const noexcept {
        return m_Type;
    }
};

} /* namespace cppcmb */

#endif /* CPPCMB_TOKEN_HPP */
