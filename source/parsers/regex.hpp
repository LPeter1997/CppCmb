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
#include "action.hpp"
#include "alt.hpp"
#include "combinator.hpp"
#include "many.hpp"
#include "many1.hpp"
#include "one.hpp"
#include "seq.hpp"
#include "../detail.hpp"
#include "../product.hpp"
#include "../reader.hpp"
#include "../result.hpp"
#include "../transformations/filter.hpp"
#include "../transformations/select.hpp"

namespace cppcmb {

namespace detail {
namespace regex {

/**
 * <top>           ::= <term> '|' <top>
 *                   | <term>
 *                   ;
 *
 * <term>          ::= <factor> <term>
 *                   | <factor>
 *                   ;
 *
 * <factor>        ::= <atom> '*'
 *                   | <atom> '+'
 *                   | <atom> '?'
 *                   | <atom>
 *                   ;
 *
 * <atom>          ::= '(' <top> ')'
 *                   | '[' <char_grouping> ']'
 *                   | <literal>
 *                   ;
 *
 * <char_grouping> ::= <group_element> <char_grouping>
 *                   | <group_element>
 *                   ;
 *
 * <group_element> ::= '\' '-'
 *                   | <literal> '-' <literal>
 *                   | <literal>
 *                   ;
 *
 * <literal>       ::= CHAR
 *                   | '\' SPECIAL_CHAR
 *                   ;
 */

template <char Ch>
constexpr bool is_char(char c) { return c == Ch; }

template <char Ch>
inline constexpr auto ch = one[filter(is_char<Ch>)][select<>];

template <char Ch1, char Ch2>
constexpr bool is_range(char c) {
    static_assert(Ch1 <= Ch2);
    return c >= Ch1 && c <= Ch2;
}

template <char Ch1, char Ch2>
inline constexpr auto range = one[filter(is_range<Ch1, Ch2>)][select<>];

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

/**
 * Succeeds when the underlying character-parser fails. Can only be used with
 * single character parsers!
 */
template <typename P>
class not_char : public combinator<not_char<P>> {
private:
    cppcmb_self_check(not_char);

    P m_Parser;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr not_char(PFwd&& p)
        : m_Parser(cppcmb_fwd(p)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<product<>> {
        cppcmb_assert_parser(P, Src);

        using result_t = result<product<>>;

        auto inv = m_Parser.apply(r);
        if (inv.is_success()) {
            // We fail
            return result_t(failure(), inv.furthest());
        }
        else {
            // We succeed
            return result_t(success(product<>(), 1), inv.furthest());
        }
    }
};

template <typename PFwd>
not_char(PFwd) -> not_char<PFwd>;

struct parser {
    template <typename T>
    [[nodiscard]] static constexpr auto star(T p) noexcept {
        return action_t((*p >> collect_to<drop_collection>), select<>);
    }

    template <typename T>
    [[nodiscard]] static constexpr auto plus(T p) noexcept {
        return action_t((+p >> collect_to<drop_collection>), select<>);
    }

    template <typename T>
    [[nodiscard]] static constexpr auto qmark(T p) noexcept {
        return action_t(-p, select<>);
    }

    template <typename T>
    static constexpr bool is_failure(T) {
        return std::is_same_v<remove_cvref_t<T>, failure>;
    }

    template <std::size_t Idx, typename Src>
    [[nodiscard]] static constexpr char char_at(Src src) noexcept {
        return src()[Idx];
    }

    [[nodiscard]] static constexpr bool is_special(char ch) noexcept {
        return ch == '(' || ch == ')'
            || ch == '[' || ch == ']'
            || ch == '*' || ch == '+'
            || ch == '|' || ch == '?'
            || ch == '\\'
            ;
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
            constexpr char curr = char_at<NextIdx>(src);
            if constexpr (curr == '*') {
                return success(star(lhs.value()), lhs.matched() + 1);
            }
            else if constexpr (curr == '+') {
                return success(plus(lhs.value()), lhs.matched() + 1);
            }
            else if constexpr (curr == '?') {
                return success(qmark(lhs.value()), lhs.matched() + 1);
            }
            else {
                return lhs;
            }
        }
    }

    template <std::size_t Idx, typename Src>
    [[nodiscard]] static constexpr auto atom(Src src) noexcept {
        if constexpr (Idx < src().size()) {
            constexpr char curr = char_at<Idx>(src);
            if constexpr (curr == '(') {
                // Grouping
                constexpr auto sub = top<Idx + 1>(src);
                static_assert(!is_failure(sub));
                constexpr std::size_t NextIdx = Idx + 1 + sub.matched();
                static_assert(char_at<NextIdx>(src) == ')');
                return success(sub.value(), sub.matched() + 2);
            }
            else if constexpr (curr == '[') {
                // Character classes
                if constexpr (char_at<Idx + 1>(src) == '^') {
                    // Negated group
                    constexpr auto sub = char_grouping<Idx + 2>(src);
                    static_assert(!is_failure(sub));
                    constexpr std::size_t NextIdx = Idx + 2 + sub.matched();
                    static_assert(char_at<NextIdx>(src) == ']');
                    return success(not_char(sub.value()), sub.matched() + 3);
                }
                else {
                    constexpr auto sub = char_grouping<Idx + 1>(src);
                    static_assert(!is_failure(sub));
                    constexpr std::size_t NextIdx = Idx + 1 + sub.matched();
                    static_assert(char_at<NextIdx>(src) == ']');
                    return success(sub.value(), sub.matched() + 2);
                }
            }
            else {
                return literal<Idx>(src);
            }
        }
        else {
            return failure();
        }
    }

    template <std::size_t Idx, typename Src>
    [[nodiscard]] static constexpr auto char_grouping(Src src) noexcept {
        constexpr auto lhs = group_element<Idx>(src);
        static_assert(!is_failure(lhs));
        return char_grouping_impl<Idx + lhs.matched()>(lhs, src);
    }

    template <std::size_t Idx, typename Res, typename Src>
    [[nodiscard]]
    static constexpr auto char_grouping_impl(Res res, Src src) noexcept {
        constexpr auto lhs = group_element<Idx>(src);
        if constexpr (is_failure(lhs)) {
            return res;
        }
        else {
            return char_grouping_impl<Idx + lhs.matched()>(
                success(
                    res.value() | lhs.value(),
                    res.matched() + lhs.matched()
                ),
                src
            );
        }
    }

    template <std::size_t Idx, typename Src>
    [[nodiscard]] static constexpr auto group_element(Src src) noexcept {
        if constexpr (char_at<Idx>(src) == '\\'
                   && char_at<Idx + 1>(src) == '-') {
            return success(ch<'-'>, 2);
        }
        else {
            constexpr auto lit = literal_ch<Idx>(src);
            if constexpr (is_failure(lit)) {
                return failure();
            }
            else {
                constexpr std::size_t NextIdx = Idx + lit.matched();
                if constexpr (char_at<NextIdx>(src) == '-') {
                    constexpr auto lit2 = literal_ch<NextIdx + 1>(src);
                    if constexpr (is_failure(lit2)) {
                        // No right-hand-side, only consumed lit
                        return success(ch<lit.value()>, lit.matched());
                    }
                    else {
                        // Char range
                        return success(
                            range<lit.value(), lit2.value()>,
                            lit.matched() + 1 + lit2.matched()
                        );
                    }
                }
                else {
                    return success(ch<lit.value()>, lit.matched());
                }
            }
        }
    }

    template <std::size_t Idx, typename Src>
    [[nodiscard]] static constexpr auto literal(Src src) noexcept {
        constexpr auto lc = literal_ch<Idx>(src);
        if constexpr (is_failure(lc)) {
            return failure();
        }
        else {
            return success(ch<lc.value()>, lc.matched());
        }
    }

    template <std::size_t Idx, typename Src>
    [[nodiscard]] static constexpr auto literal_ch(Src src) noexcept {
        constexpr char curr = char_at<Idx>(src);
        if constexpr (curr == '\\') {
            // Escaped
            constexpr char nxt = char_at<Idx + 1>(src);
            static_assert(is_special(nxt));
            return success(nxt, 2);
        }
        else if constexpr (is_special(curr)) {
            // Special characters
            return failure();
        }
        else {
            // Literal match
            return success(curr, 1);
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
