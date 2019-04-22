/**
 * regex.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * Functionality to turn a compile-time RegEx string into a parser. Can be used
 * for efficient tokenization.
 */

#ifndef CPPCMB_PARSERS_REGEX_HPP
#define CPPCMB_PARSERS_REGEX_HPP

#include <cstddef>
#include <string_view>
#include <type_traits>
#include "alt.hpp"
#include "many.hpp"
#include "many1.hpp"
#include "one.hpp"
#include "seq.hpp"
#include "../detail.hpp"
#include "../reader.hpp"
#include "../transformations/filter.hpp"
#include "../transformations/select.hpp"

namespace cppcmb {

namespace detail {
namespace regex {

/**
 * <top>    ::= <term> '|' <top>
 *            | <term>
 *            ;
 *
 * <term>   ::= <factor> <term>
 *            ;
 *
 * <factor> ::= <atom> '*'?
 *            ;
 *
 * <atom>   ::= CHAR
 *            | '\' CHAR
 *            | <char_grouping>
 *            | '(' <top> ')'
 *            ;
 */

template <char Ch>
constexpr bool is_char(char c) { return c == Ch; }

template <char Ch>
inline constexpr auto ch = one[filter(is_char<Ch>)][select<>];

// XXX(LPeter1997): We publish something like this in the API
/**
 * A dummy collection interface.
 */
template <typename>
class drop_collection {
private:
    std::size_t cnt = 0;

public:
    template <typename TFwd>
    constexpr void push_back(TFwd&&) noexcept {
        ++cnt;
    }

    [[nodiscard]] constexpr std::size_t size() const noexcept { return cnt; }
};

template <typename T>
[[nodiscard]] constexpr auto star(T p) noexcept {
    return action_t((*p >> collect_to<drop_collection>), select<>);
}

struct parser {
    template <typename T>
    static constexpr bool is_failure(T) {
        return std::is_same_v<remove_cvref_t<T>, failure>;
    }

    template <std::size_t Idx, typename Src>
    [[nodiscard]] static constexpr char char_at(Src src) noexcept {
        return src()[Idx];
    }

    template <std::size_t Idx, typename Src>
    [[nodiscard]] static constexpr auto top(Src src) noexcept {
        constexpr auto lhs = term<Idx>(src);
        static_assert(!is_failure(lhs));
        constexpr std::size_t NextIdx = Idx + lhs.matched();
        if constexpr (char_at<NextIdx>(src) == '|') {
            constexpr auto rhs = top<NextIdx + 1>(src);
            static_assert(!is_failure(rhs));
            return success(
                lhs.value() | rhs.value(),
                lhs.matched() + 1 + rhs.matched()
            );
        }
        else {
            return lhs;
        }
    }

    template <std::size_t Idx, typename Src>
    [[nodiscard]] static constexpr auto term(Src src) noexcept {
        constexpr auto lhs = factor<Idx>(src);
        static_assert(!is_failure(lhs));
        constexpr std::size_t NextIdx = Idx + lhs.matched();
        return term_impl<NextIdx>(lhs, src);
    }

    template <std::size_t Idx, typename Res, typename Src>
    [[nodiscard]] static constexpr auto term_impl(Res res, Src src) noexcept {
        if constexpr (src().size() <= Idx) {
            return res;
        }
        else {
            constexpr auto lhs = factor<Idx>(src);
            if constexpr (is_failure(lhs)) {
                return res;
            }
            else {
                constexpr std::size_t NextIdx = Idx + lhs.matched();
                return term_impl<NextIdx>(
                    success(
                        res.value() & lhs.value(),
                        res.matched() + lhs.matched()
                    ),
                    src
                );
            }
        }
    }

    template <std::size_t Idx, typename Src>
    [[nodiscard]] static constexpr auto factor(Src src) noexcept {
        constexpr auto lhs = atom<Idx>(src);
        if constexpr (is_failure(lhs)) {
            return failure();
        }
        else {
            constexpr std::size_t NextIdx = Idx + lhs.matched();
            if constexpr (char_at<NextIdx>(src) == '*') {
                constexpr auto res = star(lhs.value());
                return success(
                    res,
                    lhs.matched() + 1
                );
            }
            else {
                return lhs;
            }
        }
    }

    template <std::size_t Idx, typename Src>
    [[nodiscard]] static constexpr auto atom(Src src) noexcept {
        if constexpr (char_at<Idx>(src) == '(') {
            // Grouping
            constexpr auto sub = top<Idx + 1>(src);
            static_assert(!is_failure(sub));
            constexpr std::size_t NextIdx = Idx + 1 + sub.matched();
            static_assert(char_at<NextIdx>(src) == ')');
            return success(
                sub.value(),
                sub.matched() + 2
            );
        }
        else if constexpr (Idx < src().size()) {
            // Single character
            if constexpr (char_at<Idx>(src) == ')') {
                return failure();
            }
            else if constexpr (char_at<Idx>(src) == '|') {
                return failure();
            }
            else {
                return success(
                    ch<char_at<Idx>(src)>,
                    1
                );
            }
        }
        else {
            failure();
        }
    }
};

} /* namespace regex */
} /* namespace detail */

/**
 * A way to define compile-time strings.
 */
#define cppcmb_str(str) ([]() -> std::string_view { return str; })

template <typename Str>
[[nodiscard]] constexpr auto regex(Str str) noexcept {
    constexpr auto res = detail::regex::parser::top<0>(str);
    static_assert(
        !detail::regex::parser::is_failure(res),
        "Invalid regular-expression!"
    );
    return res.value();
}

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_REGEX_HPP */
