/**
 * reader.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT license.
 *
 * Reader abstraction for the library. Helps reading through the parsed source.
 */

#ifndef CPPCMB_READER_HPP
#define CPPCMB_READER_HPP

#include <iterator>
#include <type_traits>
#include <utility>

namespace cppcmb {

class default_source_adapter {
public:
    // TODO(LPeter1997): noexcept specifier?
    template <typename Source, typename Cursor>
    [[nodiscard]] constexpr auto const& at(Source const& src, Cursor cursor) {
        return src[cursor];
    }

    // TODO(LPeter1997): noexcept specifier?
    template <typename Source>
    [[nodiscard]] constexpr auto length(Source const& src) {
        return std::size(src);
    }
};

template <typename Source, typename Adapter = default_source_adapter>
class reader {
public:
    using source_type = Source;
    using source_adapter = Adapter;
    using cursor_type = std::size_t;
    using const_reference =
        decltype(std::declval<source_adapter const&>().at(std::declval<source_type const&>(), cursor_type{}));
    using reference = const_reference;
    using value_type = std::decay_t<reference>;

    static_assert(std::is_lvalue_reference_v<reference>);

public:
    // TODO(LPeter1997): noexcept specifier?
    constexpr source_adapter() = default;

    // TODO(LPeter1997): noexcept specifier?
    constexpr reader(source_type const* src, cursor_type pos, source_adapter const& adapter = source_adapter())
        : m_Source(src), m_Cursor(pos), m_Adapter(adapter) {
    }

    // TODO(LPeter1997): noexcept specifier?
    constexpr reader(source_type const* src, source_adapter const& adapter = source_adapter())
        : reader(src, 0, adapter) {
    }

private:
    source_type const* m_Source = nullptr;
    cursor_type m_Cursor = 0;
    source_adapter m_Adapter = source_adapter();
};

} /* namespace cppcmb */

#endif /* CPPCMB_READER_HPP */
