/**
 * epsilon.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A parser that always succeeds with an empty result.
 */

#ifndef CPPCMB_PARSERS_EPSILON_HPP
#define CPPCMB_PARSERS_EPSILON_HPP

#include "combinator.hpp"
#include "../product.hpp"
#include "../result.hpp"

namespace cppcmb {

class epsilon_t : public combinator<epsilon_t> {
public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& /* r */) const
        -> result<product<>> {

        // XXX(LPeter1997): GCC bug
        return result<product<>>(success(product<>(), 0U), 0U);
    }
};

// Value for 'epsilon' parser
inline constexpr auto epsilon = epsilon_t();

} /* namespace cppcmb */

#endif /* CPPCMB_PARSERS_EPSILON_HPP */
