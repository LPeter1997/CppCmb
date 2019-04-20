/**
 * filter.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A transformation that only succeeds if a given predicate is true for the
 * input.
 */

#ifndef CPPCMB_TRANSFORMATIONS_FILTER_HPP
#define CPPCMB_TRANSFORMATIONS_FILTER_HPP

#include <cstddef>
#include <type_traits>
#include "../detail.hpp"
#include "../maybe.hpp"
#include "../product.hpp"

namespace cppcmb {

template <typename Pred>
class filter {
private:
    cppcmb_self_check(filter);

    template <typename... Ts>
    using value_t = decltype(product_values(
        std::declval<Ts>()...
    ));

    Pred m_Predicate;

public:
    template <typename PredFwd, cppcmb_requires_t(!is_self_v<PredFwd>)>
    constexpr filter(PredFwd&& pred)
        noexcept(std::is_nothrow_constructible_v<Pred, PredFwd&&>)
        : m_Predicate(cppcmb_fwd(pred)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename... Ts>
    [[nodiscard]] constexpr auto operator()(Ts&&... args) const
        -> maybe<value_t<Ts&&...>> {
        static_assert(
            std::is_invocable_v<Pred, Ts&&...>,
            "The predicate must be invocable with the parser value!"
        );
        using result_t = std::invoke_result_t<Pred, Ts&&...>;
        static_assert(
            std::is_convertible_v<result_t, bool>,
            "The predicate must return a type that is convertible to bool!"
        );

        if (m_Predicate(args...)) {
            return some(product_values(cppcmb_fwd(args)...));
        }
        else {
            return none();
        }
    }
};

template <typename PredFwd>
filter(PredFwd) -> filter<PredFwd>;

} /* namespace cppcmb */

#endif /* CPPCMB_TRANSFORMATIONS_FILTER_HPP */
