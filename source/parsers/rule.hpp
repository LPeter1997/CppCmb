/**
 * rule.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A wrapper parser that allows for easier recursion.
 */

#ifndef CPPCMB_PARSERS_RULE_HPP
#define CPPCMB_PARSERS_RULE_HPP

#include "combinator.hpp"

namespace cppcmb {

namespace detail {

// XXX(LPeter1997): Can this be constexpr?
/**
 * This is where all the rules are stored internally.
 * They allow us to do a nice assignment-syntax.
 */
template <typename>
inline /* constexpr */ auto rule_set = 0;

} /* namespace detail */

template <typename Val, typename Tag>
class rule_t : public combinator<rule_t<Val, Tag>> {
public:
    using tag_type = Tag;

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<Val> {
        return cppcmb_parse_rule(*this, r);
    }
};

/**
 * Used to declare rules.
 */
#define cppcmb_decl(name, ...) \
auto const name =              \
::cppcmb::rule_t<__VA_ARGS__, struct cppcmb_unique_id(cppcmb_rule_tag)>()

// XXX(LPeter1997): The use of the inline variable like this is IFNDR...
// We need an alternative solution!
// XXX(LPeter1997): Noexcept specifier
/**
 * Used to define rules.
 * Generates the function that does the indirect-call.
 */
#define cppcmb_def(name)                                                   \
template <typename Src, typename Lazy = typename decltype(name)::tag_type> \
[[nodiscard]] constexpr auto                                               \
cppcmb_parse_rule(decltype(name), ::cppcmb::reader<Src> const& r) {        \
    auto const& p = ::cppcmb::detail::rule_set<Lazy>;                      \
    return p.apply(r);                                                     \
}                                                                          \
template <>                                                                \
inline /* constexpr */ auto                                                \
::cppcmb::detail::rule_set<typename decltype(name)::tag_type>

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_RULE_HPP */
