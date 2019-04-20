/**
 * parser.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A wrapper-type, that's not an actual combinator. Simply wraps a combinator
 * and provides memo context and a simpler interface for parsing.
 */

#ifndef CPPCMB_PARSER_HPP
#define CPPCMB_PARSER_HPP

#include <cstddef>
#include "detail.hpp"
#include "memo_context.hpp"
#include "reader.hpp"

namespace cppcmb {

template <typename P>
class parser {
private:
    cppcmb_self_check(parser);

    P            m_Parser;
    memo_context m_Context;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr parser(PFwd&& p)
        : m_Parser(cppcmb_fwd(p)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr decltype(auto) parse(Src const& src) {
        m_Context.clear();
        auto r = reader(src, m_Context);
        return m_Parser.apply(r);
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr decltype(auto)
    reparse(Src const& src,
        std::size_t start, std::size_t rem, std::size_t ins) {

        m_Context.memo().invalidate(start, rem, ins);
        auto r = reader(src, m_Context);
        return m_Parser.apply(r);
    }
};

template <typename PFwd>
parser(PFwd) -> parser<PFwd>;

} /* namespace cppcmb */

#endif /* CPPCMB_PARSER_HPP */
