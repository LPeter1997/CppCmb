/**
 * reader.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * The reader type that the parsers use to step through the input.
 */

#ifndef CPPCMB_READER_HPP
#define CPPCMB_READER_HPP

#include <cstddef>
#include <memory>
#include "detail.hpp"

namespace cppcmb {

namespace detail {

/**
 * Concept for the reader source.
 * The reader has to have:
 *  - An operator[](std::size_t) that returns the element at the given index
 *  - A .size() member or size(source) that returns the length of the source
 */

template <typename T>
using element_at_t = decltype(std::declval<T>()[std::declval<std::size_t>()]);

template <typename T>
using msize_t = decltype(std::size(std::declval<T>()));

// Readable source concept for the reader
template <typename T>
inline constexpr bool is_reader_source_v =
       is_detected_v<element_at_t, T>
    && is_detected_v<msize_t, T>;

} /* namespace detail */

class memo_context;

template <typename Src>
class reader {
public:
    static_assert(
        detail::is_reader_source_v<Src>,
        "The reader source must have a subscript operator [std::size_t] and a "
        "size() member function!"
    );

private:
    Src const*    m_Source;
    std::size_t   m_Cursor;
    memo_context* m_MemoCtx;

public:
    using value_type = detail::remove_cvref_t<decltype((*m_Source)[m_Cursor])>;

    constexpr reader() noexcept
        : m_Source(nullptr), m_Cursor(0U), m_MemoCtx(nullptr) {
    }

    constexpr reader(Src const& src, std::size_t idx, memo_context* t) noexcept
        : m_Source(::std::addressof(src)), m_Cursor(0U), m_MemoCtx(t) {
        seek(idx);
    }

    constexpr reader(Src const& src, std::size_t idx, memo_context& t) noexcept
        : reader(src, idx, &t) {
    }

    constexpr reader(Src const& src, std::size_t idx = 0U) noexcept
        : reader(src, idx, nullptr) {
    }

    constexpr reader(Src const& src, memo_context& t) noexcept
        : reader(src, 0U, t) {
    }

    // Just to avoid nasty bugs
    reader(Src const&& src, std::size_t idx, memo_context* t) = delete;

    [[nodiscard]] constexpr auto* source_ptr() const noexcept {
        return m_Source;
    }

    [[nodiscard]] constexpr auto const& source() const noexcept {
        cppcmb_assert(
            "A reader without a source can't return a source-reference!",
            source_ptr() != nullptr
        );
        return *source_ptr();
    }

    [[nodiscard]] constexpr auto const& cursor() const noexcept {
        return m_Cursor;
    }

    [[nodiscard]] constexpr bool is_end() const noexcept {
        return cursor() >= std::size(source());
    }

    [[nodiscard]] constexpr auto const& current() const noexcept {
        cppcmb_assert(
            "current() can only be invoked when the cursor is not past the "
            "elements!",
            cursor() < std::size(source())
        );
        return (*m_Source)[cursor()];
    }

    constexpr void seek(std::size_t idx) noexcept {
        cppcmb_assert(
            "seek() argument must be in the bounds of source!",
            idx <= std::size(source())
        );
        m_Cursor = idx;
    }

    constexpr void next() noexcept {
        seek(cursor() + 1);
    }

    [[nodiscard]] constexpr auto* context_ptr() const noexcept {
        return m_MemoCtx;
    }

    [[nodiscard]] constexpr auto& context() const noexcept {
        cppcmb_assert(
            "A memo-context must be assigned before accessing it!",
            m_MemoCtx != nullptr
        );
        return *context_ptr();
    }
};

} /* namespace cppcmb */

#endif /* CPPCMB_READER_HPP */
