/**
 * lexer.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A generic lexer that produces tokens.
 */

#ifndef CPPCMB_LEXER_HPP
#define CPPCMB_LEXER_HPP

#include <cstddef>
#include <iterator>
#include <memory>
#include <string_view>
#include <type_traits>
#include <utility>
#include "detail.hpp"
#include "parsers/combinator.hpp"
#include "parsers/regex.hpp"
#include "reader.hpp"
#include "result.hpp"
#include "token.hpp"

namespace cppcmb {

/**
 * Signal that we want to skip these characters instead of making a token out of
 * them.
 */
struct skip_t {};

inline constexpr auto skip = skip_t();

namespace detail {

// XXX(LPeter1997): This implementation blocks incremental features
template <typename P, typename Tag>
class token_parser : public combinator<token_parser<P, Tag>> {
private:
    P m_Parser;
    Tag m_Tag;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename PFwd>
    constexpr token_parser(PFwd&& p, Tag t)
        : m_Parser(cppcmb_fwd(p)), m_Tag(t) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<maybe<token<Tag>>> {

        using maybe_t = maybe<token<Tag>>;
        using result_t = result<maybe_t>;

        auto t = m_Parser.apply(r);
        if (t.is_success()) {
            std::string_view src = r.source();
            std::size_t len = t.success().matched();
            auto tok = token(src.substr(r.cursor(), len), m_Tag);

            return result_t(
                success(maybe_t(some(std::move(tok))), len),
                t.furthest()
            );
        }
        else {
            return result_t(std::move(t).failure(), t.furthest());
        }
    }
};

template <typename P, typename Tag>
class skip_token_parser : public combinator<skip_token_parser<P, Tag>> {
private:
    P m_Parser;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename PFwd>
    constexpr skip_token_parser(PFwd&& p)
        : m_Parser(cppcmb_fwd(p)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<maybe<token<Tag>>> {

        using maybe_t = maybe<token<Tag>>;
        using result_t = result<maybe_t>;

        auto t = m_Parser.apply(r);
        if (t.is_success()) {
            return result_t(
                success(maybe_t(none()), t.success().matched()),
                t.furthest()
            );
        }
        else {
            return result_t(std::move(t).failure(), t.furthest());
        }
    }
};

// XXX(LPeter1997): Noexcept specifier
template <typename Tag, typename Src, typename TTag>
[[nodiscard]] constexpr auto str_to_token_parser(Src src, TTag t) {
    ((void)t); // Unused warning
    auto p = ::cppcmb::regex(src);
    using parser_type = decltype(p);
    if constexpr (std::is_same_v<TTag, skip_t>) {
        // We want to skip this
        return skip_token_parser<parser_type, Tag>(std::move(p));
    }
    else {
        // Keep it
        static_assert(std::is_same_v<Tag, TTag>);
        return token_parser<parser_type, Tag>(std::move(p), t);
    }
}

/**
 * Trait to find the first not skip-type in the type-list.
 */
template <typename...>
struct first_not_skip;

// If there is none, we signal failure with defining skip_t
template <>
struct first_not_skip<> {
    using type = skip_t;
};

template <typename Head, typename... Tail>
struct first_not_skip<Head, Tail...> {
    using type = std::conditional_t<
        std::is_same_v<Head, skip_t>,
        typename first_not_skip<Tail...>::type,
        Head
    >;
};

template <typename... Ts>
using first_not_skip_t = typename first_not_skip<Ts...>::type;

// XXX(LPeter1997): Noexcept specifier
template <typename... Rs>
[[nodiscard]] constexpr auto make_lexer_parser(Rs&&... rules) {
    using token_type = first_not_skip_t<
        typename remove_cvref_t<Rs>::tag_type...
    >;
    // XXX(LPeter1997): Or we could just allow it
    static_assert(
        !std::is_same_v<token_type, skip_t>,
        "There must be at least one token rule that doesn't skip!"
    );
    // XXX(LPeter1997): We could do a check if the tokenizer succeeds for an
    // empty string. If it does, tell the user it's a BAD idea.
    return (... | str_to_token_parser<token_type>(
        cppcmb_fwd(rules).source(),
        cppcmb_fwd(rules).tag()
    ));
}

} /* namespace detail */

template <typename Lexer, typename Src>
class token_iterator {
public:
    using token_type        = detail::remove_cvref_t<decltype(
        std::declval<Lexer const&>()
            .rule()
            .apply(std::declval<reader<Src> const&>())
            .success()
            .value()
            .some()
            .value()
            .type()
    )>;
    using value_type        = result<token<token_type>>;
    using difference_type   = std::ptrdiff_t;
    using pointer           = value_type const*;
    using reference         = value_type const&;
    using iterator_category = std::forward_iterator_tag;

private:
    Lexer const*              m_Lexer;
    reader<Src>               m_Reader;
    std::optional<value_type> m_Last;

public:
    constexpr token_iterator() noexcept
        : m_Lexer(nullptr), m_Reader(), m_Last(std::nullopt) {
    }

    // XXX(LPeter1997): Noexcept specifier
    constexpr token_iterator(Lexer const& l, Src const& src)
        : m_Lexer(::std::addressof(l)), m_Reader(src) {
        find_token();
    }

    template <typename Src2>
    [[nodiscard]]
    constexpr bool
    operator==(token_iterator<Lexer, Src2> const& o) const noexcept {
        // A null-source in the reader indicates the end
        if (m_Reader.source_ptr() == nullptr) {
            if (o.m_Reader.source_ptr() == nullptr) {
                return true;
            }
            if (o.m_Reader.is_end()) {
                return true;
            }
        }
        if (o.m_Reader.source_ptr() == nullptr) {
            if (m_Reader.is_end()) {
                return true;
            }
        }
        // Both readers have sources
        return m_Reader.source_ptr() == o.m_Reader.source_ptr()
            && m_Reader.cursor()     == o.m_Reader.cursor();
    }

    template <typename Src2>
    [[nodiscard]]
    constexpr bool
    operator!=(token_iterator<Lexer, Src2> const& o) const noexcept {
        return !operator==(o);
    }

    [[nodiscard]] constexpr reference operator*() const noexcept {
        cppcmb_assert(
            "A value must be present for de-referencing!",
            m_Last.has_value()
        );
        return *m_Last;
    }

    [[nodiscard]] constexpr pointer operator->() const noexcept {
        return ::std::addressof(operator*());
    }

    // XXX(LPeter1997): Noexcept specifier
    constexpr token_iterator& operator++() {
        cppcmb_assert(
            "A token iterator without a source can't be incremented!",
            m_Reader.source_ptr() != nullptr
        );
        cppcmb_assert(
            "A token iterator at the end can't be incremented!",
            !m_Reader.is_end()
        );
        cppcmb_assert(
            "Precondition of increment is dereferenceable!",
            m_Last.has_value()
        );
        auto const& last = *m_Last;
        if (last.is_success()) {
            // For success we skip the entire thing
            m_Reader.seek(m_Reader.cursor() + last.success().matched());
        }
        else {
            // XXX(LPeter1997): Is this the best strategy?
            // For failures we skip a single character
            m_Reader.seek(m_Reader.cursor() + 1);
        }
        find_token();
        return *this;
    }

    // XXX(LPeter1997): Noexcept specifier
    constexpr token_iterator operator++(int) {
        auto cpy = *this;
        operator++();
        return cpy;
    }

private:
    // XXX(LPeter1997): Noexcept specifier
    constexpr void find_token() {
        while (true) {
            if (m_Reader.is_end()) {
                return;
            }
            auto res = m_Lexer->rule().apply(m_Reader);
            if (res.is_success()) {
                auto succ = std::move(res).success();
                if (succ.value().is_some()) {
                    // Token, store it
                    m_Last = value_type(
                        success(
                            std::move(succ).value().some().value(),
                            succ.matched()
                        ),
                        res.furthest()
                    );
                    return;
                }
                else {
                    // Skip
                    m_Reader.seek(m_Reader.cursor() + succ.matched());
                }
            }
            else {
                // Error, store it
                m_Last = value_type(std::move(res).failure(), res.furthest());
                return;
            }
        }
    }
};

template <typename Src, typename Tag>
class token_rule {
private:
    Src m_Src;
    Tag m_Tag;

public:
    using tag_type = Tag;

    // XXX(LPeter1997): Noexcept specifier
    constexpr token_rule(Src src, Tag t)
        : m_Src(src), m_Tag(t) {
    }

    // XXX(LPeter1997): Possibly don't need
    cppcmb_getter(source, m_Src)
    cppcmb_getter(tag, m_Tag)
};

#define cppcmb_token(rx, ...) ::cppcmb::token_rule(cppcmb_str(rx), __VA_ARGS__)

template <typename MainRule>
class lexer {
private:
    MainRule m_Rule;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename... Rs>
    constexpr lexer(Rs&&... rules)
        : m_Rule(detail::make_lexer_parser(cppcmb_fwd(rules)...)) {
    }

    [[nodiscard]] constexpr auto const& rule() const noexcept { return m_Rule; }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto begin(Src const& src) const {
        return token_iterator<lexer, Src>(*this, src);
    }

    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] constexpr auto end() const {
        return token_iterator<lexer, std::string_view>();
    }
};

template <typename... Rs>
lexer(Rs&&...)
    -> lexer<decltype(detail::make_lexer_parser(std::declval<Rs&&>()...))>;

} /* namespace cppcmb */

#endif /* CPPCMB_LEXER_HPP */
