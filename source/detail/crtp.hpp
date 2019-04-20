/**
 * crtp.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * Utility for CRTP.
 * See: https://www.fluentcpp.com/2017/05/19/crtp-helper/
 */

#ifndef CPPCMB_DETAIL_CRTP_HPP
#define CPPCMB_DETAIL_CRTP_HPP

namespace cppcmb {
namespace detail {

template <typename Self>
class crtp {
public:
    using self_type = Self;

    [[nodiscard]]
    constexpr self_type& self() & noexcept {
        return static_cast<self_type&>(*this);
    }

    [[nodiscard]]
    constexpr self_type const& self() const& noexcept {
        return static_cast<self_type const&>(*this);
    }
};

} /* namespace detail */
} /* namespace cppcmb */

#endif /* CPPCMB_DETAIL_CRTP_HPP */
