/**
 * one.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * Consumes a single element from the input, if there is one.
 */

#ifndef CPPCMB_PARSERS_ONE_HPP
#define CPPCMB_PARSERS_ONE_HPP

#include "combinator.hpp"
#include "../result.hpp"

namespace cppcmb {

class one_t : public combinator<one_t> {
public:
    // XXX(LPeter1997): Do we need typename here?
    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<typename reader<Src>::value_type> {

        using result_t = result<typename reader<Src>::value_type>;

        if (r.is_end()) {
            // Nothing to consume
            return result_t(failure(), 0U);
        }
        else {
            return result_t(success(r.current(), 1U), 1U);
        }
    }
};

// Value for 'one' parser
inline constexpr one_t one = one_t();

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_ONE_HPP */
