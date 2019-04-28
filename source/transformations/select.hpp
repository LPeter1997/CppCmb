/**
 * select.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * Selects only the elements from the product with the provided indicies.
 */

#ifndef CPPCMB_TRANSFORMATIONS_SELECT_HPP
#define CPPCMB_TRANSFORMATIONS_SELECT_HPP

// XXX(LPeter1997): There is probably a bug with Clang where selecting nothing
// from product<> fails. The JSON example (other repo right now) shows that at
// line 127

namespace cppcmb {

template <std::size_t... Ns>
class select_t {
public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename... Ts>
    [[nodiscard]] constexpr decltype(auto) operator()(Ts&&... args) const {
        return product_values(
            std::get<Ns>(std::tuple(cppcmb_fwd(args)...))...
        );
    }
};

template <std::size_t... Ns>
inline constexpr auto select = select_t<Ns...>();

} /* namespace cppcmb */

#endif /* CPPCMB_TRANSFORMATIONS_SELECT_HPP */
