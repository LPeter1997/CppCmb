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

template <typename Src, typename Tag>
class token_iterator {
public:
    using value_type        = result<token<Tag>>;
    using difference_type   = std::ptrdiff_t;
    using pointer           = value_type const*;
    using reference         = value_type const&;
    using iterator_category = std::forward_iterator_tag;

private:
    reader<Src>               m_Reader;
    std::optional<value_type> m_Last;

public:
    constexpr token_iterator() noexcept = default;

    constexpr token_iterator(Src const& src) noexcept
        : m_Reader(src) {
    }

    [[nodiscard]]
    constexpr bool operator==(token_iterator const& o) const noexcept {
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

    [[nodiscard]]
    constexpr bool operator!=(token_iterator const& o) const noexcept {
        return !operator==(o);
    }

    [[nodiscard]] constexpr reference operator*() const noexcept {
        cppcmb_assert(m_Last.has_value());
        return *m_Last;
    }

    [[nodiscard]] constexpr pointer operator->() const noexcept {
        return ::std::addressof(operator*());
    }

    // XXX(LPeter1997): Noexcept specifier
    constexpr token_iterator& operator++() {
        // XXX(LPeter1997): Implement
    }

    // XXX(LPeter1997): Noexcept specifier
    constexpr token_iterator operator++(int) {
        auto cpy = *this;
        operator++();
        return cpy;
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

    // XXX(LPeter1997): Noexcept specifier
    constexpr auto next() {
        using token_t = detail::remove_cvref_t<
            decltype(m_Rule.apply(m_Reader).success().value().some().value())
        >;
        using result_t = result<token_t>;

        while (true) {
            if (is_end()) {
                return result_t(failure(), 0);
            }
            auto t = m_Rule.apply(m_Reader);
            if (t.is_success()) {
                auto succ = std::move(t).success();

                m_Reader.seek(m_Reader.cursor() + succ.matched());

                if (succ.value().is_some()) {
                    return result_t(
                        success(
                            std::move(succ).value().some().value(),
                            succ.matched()
                        ),
                        t.furthest()
                    );
                }
                else {
                    continue;
                }
            }
            else {
                return result_t(failure(), t.furthest());
            }
        }
    }

    [[nodiscard]] constexpr auto is_end() const
        cppcmb_return(m_Reader.is_end())
};

template <typename... Rs>
lexer(Rs&&...)
    -> lexer<decltype(detail::make_lexer_parser(std::declval<Rs&&>()...))>;

} /* namespace cppcmb */

#endif /* CPPCMB_LEXER_HPP */
