/**
 * end.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * Matches the end of the input.
 */

#ifndef CPPCMB_PARSERS_END_HPP
#define CPPCMB_PARSERS_END_HPP

#include "combinator.hpp"
#include "../product.hpp"
#include "../result.hpp"

namespace cppcmb {

class end_t : public combinator<end_t> {
public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<product<>> {

        if (r.is_end()) {
            // XXX(LPeter1997): GCC bug
            return result<product<>>(success(product<>(), 0U), 0U);
        }
        return result<product<>>(failure(), 0U);
    }
};

// Value for 'end' parser
inline constexpr end_t end = end_t();

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_END_HPP */
