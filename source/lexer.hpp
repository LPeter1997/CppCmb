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

#include "detail.hpp"
#include "parsers/combinator.hpp"
#include "reader.hpp"
#include "result.hpp"
#include "token.hpp"

namespace cppcmb {

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

        using result_t = result<maybe<token<Tag>>>;

        auto t = m_Parser.apply(r);
        if (t.is_success()) {
            std::string_view src = r.source();
            std::size_t len = t.success().matched();
            auto tok = token(src.substr(r.cursor(), len), m_Tag);

            return result_t(
                success(some(std::move(tok)), len),
                t.furthest()
            );
        }
        else {
            return result_t(std::move(t).failure(), t.furthest());
        }
    }
};

template <typename P, typename Tag>
class skip_token_parser : public combinator<token_parser<>> {
private:
    P m_Parser;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename PFwd>
    constexpr token_parser(PFwd&& p)
        : m_Parser(cppcmb_fwd(p)), m_Tag(t) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<maybe<token<Tag>>> {

        using result_t = result<maybe<token<Tag>>>;

        auto t = m_Parser.apply(r);
        if (t.is_success()) {
            return result_t(
                success(none(), t.success().matched()),
                t.furthest()
            );
        }
        else {
            return result_t(std::move(t).failure(), t.furthest());
        }
    }
};

// XXX(LPeter1997): A function that turns a parser + token into token_parser or
// skip_token_parser

} /* namespace detail */

template <typename Src, typename Tag>
class token_rule {
private:

};

template <typename MainRule>
class lexer {

};

} /* namespace cppcmb */

#endif /* CPPCMB_LEXER_HPP */
