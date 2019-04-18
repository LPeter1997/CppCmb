/**
 * cppcmb.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 *
 * Distributed under the MIT License.
 *
 * A simple to use C++17 parser combinator library.
 * Repository and usage: https://github.com/LPeter1997/CppCmb
 */

#ifndef CPPCMB_HPP
#define CPPCMB_HPP

#include <algorithm>
#include <any>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <memory>
#include <optional>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

/**
 * Macros used by the library.
 */
#define cppcmb_cat(x, y) cppcmb_prelude_cat(x, y)

#define cppcmb_unique_id(prefix) cppcmb_cat(prefix, __LINE__)

#define cppcmb_fwd(...) \
::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

#define cppcmb_assert_concept(...) \
static_assert(__VA_ARGS__, "Concept assertion " #__VA_ARGS__ " failed!")

#define cppcmb_requires(...) \
template <cppcmb_requires_t(__VA_ARGS__)>

#define cppcmb_requires_t(...) \
cppcmb_prelude_requires_t1(cppcmb_unique_id(cppcmb_concept_req), __VA_ARGS__)

#define cppcmb_return(...) \
noexcept(noexcept(__VA_ARGS__)) -> decltype(__VA_ARGS__) { return __VA_ARGS__; }

#define cppcmb_assert(msg, ...) \
assert(((void)msg, (__VA_ARGS__)))

#define cppcmb_unreachable() \
cppcmb_assert("Unreachable code!", false)

#define cppcmb_getter(name, ...)                                        \
[[nodiscard]]                                                           \
constexpr auto& name() & noexcept(noexcept(__VA_ARGS__)) {              \
    return __VA_ARGS__;                                                 \
}                                                                       \
[[nodiscard]]                                                           \
constexpr auto const& name() const& noexcept(noexcept(__VA_ARGS__)) {   \
    return __VA_ARGS__;                                                 \
}                                                                       \
[[nodiscard]]                                                           \
constexpr auto&& name() && noexcept(noexcept(__VA_ARGS__)) {            \
    return std::move(__VA_ARGS__);                                      \
}                                                                       \
[[nodiscard]]                                                           \
constexpr auto const&& name() const&& noexcept(noexcept(__VA_ARGS__)) { \
    return std::move(__VA_ARGS__);                                      \
}

#define cppcmb_self_check(type) \
cppcmb_prelude_self_check(type, cppcmb_unique_id(cppcmb_self_type))

#define cppcmb_is_specialization(type)                                 \
template <typename SpecType>                                           \
using is_##type = ::cppcmb::detail::is_specialization<SpecType, type>; \
template <typename SpecType>                                           \
inline constexpr bool is_##type##_v = is_##type<SpecType>::value

/**
 * Macro details.
 */
#define cppcmb_prelude_cat(x, y) x ## y

#define cppcmb_prelude_requires_t1(id, ...) 						\
bool id = false,                                                    \
::std::enable_if_t<id || (__VA_ARGS__), ::std::nullptr_t> = nullptr

#define cppcmb_prelude_self_check(type, id)                       \
template <typename id>                                            \
static constexpr bool is_self_v =                                 \
    ::std::is_same_v<::cppcmb::detail::remove_cvref_t<id>, type>

// XXX(LPeter1997): Probably a good idea to get rid of that return macro...
// For members at least, as it doesn't return by ref.

// XXX(LPeter1997): We could eliminate std::any by having a shared memo-table
// between the "same-origin" packrat parsers.

// XXX(LPeter1997): Clean up public interface. For example memo_table doesn't
// belong there because we wrapped it in memo_context.
// Also the helpers for indirect-recursion...

// XXX(LPeter1997): We could make many and many1 right recursive!
// It would simplify implementation and allow incremental parsing to benefit
// from individual list elements

// XXX(LPeter1997): We can simplify at a lot of places just by returning result
// Instead if dissecting it to success/failure and reconstructing

// XXX(LPeter1997): It looks like direct-left-recursive incremental parsing is
// a bit too strict (invalidates too many things)
// Maybe just the nature of the recursion?

namespace cppcmb {

namespace detail {

/**
 * @see https://en.cppreference.com/w/cpp/experimental/nonesuch
 */
struct nonesuch {
    ~nonesuch()                     = delete;
    nonesuch(nonesuch const&)       = delete;
    void operator=(nonesuch const&) = delete;
};

/**
 * @see https://en.cppreference.com/w/cpp/experimental/is_detected
 */
template <typename Default, typename AlwaysVoid,
    template <typename...> typename Op, typename... Args>
struct detector {
    using value_t = std::false_type;
    using type = Default;
};

template <typename Default,
    template <typename...> typename Op, typename... Args>
struct detector<Default, std::void_t<Op<Args...>>, Op, Args...> {
    using value_t = std::true_type;
    using type = Op<Args...>;
};

template <template <typename...> typename Op, typename... Args>
using is_detected = typename detector<nonesuch, void, Op, Args...>::value_t;

template <template <typename...> typename Op, typename... Args>
using detected_t = typename detector<nonesuch, void, Op, Args...>::type;

template <typename Default,
    template <typename...> typename Op, typename... Args>
using detected_or = detector<Default, void, Op, Args...>;

// Additional utilities

template <template <typename...> typename Op, typename... Args>
inline constexpr auto is_detected_v = is_detected<Op, Args...>::value;

template <typename Default,
    template <typename...> typename Op, typename... Args>
using detected_or_t = typename detected_or<Default, Op, Args...>::type;

template <typename Expected,
    template <typename...> typename Op, typename... Args>
using is_detected_exact = std::is_same<Expected, detected_t<Op, Args...>>;

template <typename Expected,
    template <typename...> typename Op, typename... Args>
inline constexpr auto is_detected_exact_v =
    is_detected_exact<Expected, Op, Args...>::value;

template <typename To, template <typename...> typename Op, typename... Args>
using is_detected_convertible =
    std::is_convertible<detected_t<Op, Args...>, To>;

template <typename To, template <typename...> typename Op, typename... Args>
inline constexpr auto is_detected_convertible_v =
    is_detected_convertible<To, Op, Args...>::value;

/**
 * @see https://en.cppreference.com/w/cpp/types/remove_cvref
 */
template <typename T>
struct remove_cvref : std::remove_cv<std::remove_reference_t<T>> {};

template <typename T>
using remove_cvref_t = typename remove_cvref<T>::type;

/**
 * Check if a type is a specialization of a template.
 */
template <typename, template <typename...> typename>
struct is_specialization : std::false_type {};

template <template <typename...> typename T, typename... Ts>
struct is_specialization<T<Ts...>, T> : std::true_type {};

template <typename T, template <typename...> typename Templ>
inline constexpr bool is_specialization_v = is_specialization<T, Templ>::value;

/**
 * Reader concept checking.
 */
template <typename T>
using element_at_t = decltype(std::declval<T>()[std::declval<std::size_t>()]);

template <typename T>
using msize_t = decltype(std::size(std::declval<T>()));

// Readable source concept for the reader
template <typename T>
inline constexpr bool is_reader_source_v = std::conjunction_v<
    is_detected<element_at_t, T>,
    is_detected<msize_t, T>
>;

} /* namespace detail */

class memo_context;

/**
 * The structure that reads from the source element-wise.
 */
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

    [[nodiscard]] constexpr auto const& source() const noexcept {
        return *m_Source;
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

/**
 * Success "type-constructor". The type that the parser returns when it
 * succeeded.
 */
template <typename T>
class success {
public:
    using value_type = T;

private:
    value_type  m_Value;
    std::size_t m_Matched;

public:
    template <typename TFwd>
    constexpr success(TFwd&& val, std::size_t matched)
        noexcept(std::is_nothrow_constructible_v<value_type, TFwd&&>)
        : m_Value(cppcmb_fwd(val)), m_Matched(matched) {
    }

    cppcmb_getter(value, m_Value)

    [[nodiscard]] constexpr auto const& matched() const noexcept {
        return m_Matched;
    }
};

template <typename TFwd>
success(TFwd, std::size_t) -> success<TFwd>;

/**
 * Failure "type-constructor". The type that the parser returns when it fails.
 */
class failure { };

/**
 * The result type of a parser. It's either a success or a failure type.
 */
template <typename T>
class result {
public:
    using success_type = ::cppcmb::success<T>;
    using failure_type = ::cppcmb::failure;

private:
    using either_type = std::variant<success_type, failure_type>;

    either_type m_Data;
    std::size_t m_Furthest;

public:
    template <typename TFwd>
    constexpr result(TFwd&& val, std::size_t furthest)
        noexcept(std::is_nothrow_constructible_v<either_type, TFwd&&>)
        : m_Data(cppcmb_fwd(val)), m_Furthest(furthest) {
    }

    [[nodiscard]] constexpr bool is_success() const noexcept {
        return std::holds_alternative<success_type>(m_Data);
    }

    [[nodiscard]] constexpr bool is_failure() const noexcept {
        return std::holds_alternative<failure_type>(m_Data);
    }

    cppcmb_getter(success, std::get<success_type>(m_Data))
    cppcmb_getter(failure, std::get<failure_type>(m_Data))

    [[nodiscard]] constexpr auto const& furthest() const noexcept {
        return m_Furthest;
    }
};

/**
 * A tuple-like object that can stores a sequence of results.
 */
template <typename... Ts>
class product {
private:
    using tuple_type = decltype(std::make_tuple(std::declval<Ts>()...));

    cppcmb_self_check(product);

    tuple_type m_Value;

public:
    static constexpr auto index_sequence =
        std::make_index_sequence<sizeof...(Ts)>();

    template <typename T,
        cppcmb_requires_t(sizeof...(Ts) == 1 && !is_self_v<T>)>
    constexpr product(T&& val)
        noexcept(std::is_nothrow_constructible_v<tuple_type, T&&>)
        : m_Value(cppcmb_fwd(val)) {
    }

    template <typename... Us, cppcmb_requires_t(sizeof...(Us) != 1)>
    constexpr product(Us&&... vals)
        noexcept(std::is_nothrow_constructible_v<tuple_type, Us&&...>)
        : m_Value(cppcmb_fwd(vals)...) {
    }

    template <std::size_t Idx>
    [[nodiscard]] constexpr auto& get() & noexcept {
        return std::get<Idx>(m_Value);
    }
    template <std::size_t Idx>
    [[nodiscard]] constexpr auto const& get() const& noexcept {
        return std::get<Idx>(m_Value);
    }
    template <std::size_t Idx>
    [[nodiscard]] constexpr auto&& get() && noexcept {
        return std::get<Idx>(std::move(m_Value));
    }
    template <std::size_t Idx>
    [[nodiscard]] constexpr auto const&& get() const&& noexcept {
        return std::get<Idx>(std::move(m_Value));
    }

    cppcmb_getter(as_tuple, m_Value)
};

template <typename... Ts>
product(Ts...) -> product<Ts...>;

// To fix a GCC bug
template <typename T, typename... Ts>
product(T, Ts...) -> product<T, Ts...>;

namespace detail {

cppcmb_is_specialization(product);

} /* namespace detail */

/**
 * Make products comparable.
 */
template <typename... Ts, typename... Us>
[[nodiscard]] constexpr auto operator==(
    product<Ts...> const& l,
    product<Us...> const& r
) cppcmb_return(l.as_tuple() == r.as_tuple())

template <typename... Ts, typename... Us>
[[nodiscard]] constexpr auto operator!=(
    product<Ts...> const& l,
    product<Us...> const& r
) cppcmb_return(l.as_tuple() != r.as_tuple())

namespace detail {

// Base-case for sizeof...(Ts) != 1
template <typename... Ts>
[[nodiscard]]
constexpr auto product_values_impl(product<Ts...>&& res) noexcept {
    // XXX(LPeter1997): Is this right?
    // XXX(LPeter1997): Is the noexcept specifier right?
    return std::move(res);
}

// Base-case for exactly one element
// XXX(LPeter1997): Noexcept specifier
template <typename T>
[[nodiscard]] constexpr auto product_values_impl(product<T>&& res) {
    return std::move(res).template get<0>();
}

// XXX(LPeter1997): Noexcept specifier
template <typename... Ts, typename Head, typename... Tail>
[[nodiscard]] constexpr auto
product_values_impl(product<Ts...>&& res, Head&& h, Tail&&... t);

// XXX(LPeter1997): Noexcept specifier
template <std::size_t... Is, typename... Ts, typename Head, typename... Tail>
[[nodiscard]] constexpr auto product_values_expand(
    std::index_sequence<Is...>,
    product<Ts...>&& res, Head&& h, Tail&&... t) {

    return product_values_impl(
        std::move(res),
        cppcmb_fwd(h).template get<Is>()...,
        cppcmb_fwd(t)...
    );
}

// XXX(LPeter1997): Noexcept specifier
template <std::size_t... Is, typename... Ts, typename Head, typename... Tail>
[[nodiscard]] constexpr auto product_values_append(
    std::index_sequence<Is...>,
    product<Ts...>&& res, Head&& h, Tail&&... t) {

    return product_values_impl(
        product(std::move(res).template get<Is>()..., cppcmb_fwd(h)),
        cppcmb_fwd(t)...
    );
}

// XXX(LPeter1997): Noexcept specifier
template <typename... Ts, typename Head, typename... Tail>
[[nodiscard]] constexpr auto product_values_head(
    std::true_type,
    product<Ts...>&& res, Head&& h, Tail&&... t) {

    return product_values_expand(
        remove_cvref_t<Head>::index_sequence,
        std::move(res),
        cppcmb_fwd(h),
        cppcmb_fwd(t)...
    );
}

// XXX(LPeter1997): Noexcept specifier
template <typename... Ts, typename Head, typename... Tail>
[[nodiscard]] constexpr auto product_values_head(
    std::false_type,
    product<Ts...>&& res, Head&& h, Tail&&... t) {

    return product_values_append(
        product<Ts...>::index_sequence,
        std::move(res),
        cppcmb_fwd(h),
        cppcmb_fwd(t)...
    );
}

// XXX(LPeter1997): Noexcept specifier
template <typename... Ts, typename Head, typename... Tail>
[[nodiscard]] constexpr auto
product_values_impl(product<Ts...>&& res, Head&& h, Tail&&... t) {
    return product_values_head(
        is_product<remove_cvref_t<Head>>(),
        std::move(res),
        cppcmb_fwd(h),
        cppcmb_fwd(t)...
    );
}

} /* namespace detail */

// XXX(LPeter1997): Noexcept specifier
/**
 * Concatenate products and values.
 */
template <typename... Ts>
[[nodiscard]] constexpr auto product_values(Ts&&... vs) {
    // XXX(LPeter1997): GCC bug
    return detail::product_values_impl(product<>(), cppcmb_fwd(vs)...);
}

template <typename... Ts>
class sum {
private:
    using variant_type = std::variant<Ts...>;

    cppcmb_self_check(sum);

    variant_type m_Value;

public:
    template <typename T, cppcmb_requires_t(!is_self_v<T>)>
    constexpr sum(T&& val)
        noexcept(std::is_nothrow_constructible_v<variant_type, T&&>)
        : m_Value(cppcmb_fwd(val)) {
    }

    template <typename U>
    [[nodiscard]] constexpr auto& get() & {
        return std::get<U>(m_Value);
    }
    template <typename U>
    [[nodiscard]] constexpr auto const& get() const& {
        return std::get<U>(m_Value);
    }
    template <typename U>
    [[nodiscard]] constexpr auto&& get() && {
        return std::get<U>(std::move(m_Value));
    }
    template <typename U>
    [[nodiscard]] constexpr auto const&& get() const&& {
        return std::get<U>(std::move(m_Value));
    }

    cppcmb_getter(as_variant, m_Value)
};

namespace detail {

cppcmb_is_specialization(sum);

} /* namespace detail */

/**
 * Make sums comparable.
 */
template <typename... Ts>
[[nodiscard]] constexpr auto operator==(
    sum<Ts...> const& l,
    sum<Ts...> const& r
) cppcmb_return(l.as_variant() == r.as_variant())

template <typename... Ts>
[[nodiscard]] constexpr auto operator!=(
    sum<Ts...> const& l,
    sum<Ts...> const& r
) cppcmb_return(l.as_variant() != r.as_variant())

namespace detail {

template <typename T, typename... Ts>
inline constexpr bool contains_type_v = (... || std::is_same_v<T, Ts>);

////////////////////////////////////////////////////

template <typename...>
struct sum_values_t_impl;

template <typename T>
struct sum_values_t_impl<sum<T>> {
    using type = T;
};

template <typename... Ts>
struct sum_values_t_impl<sum<Ts...>> {
    using type = sum<Ts...>;
};

template <typename... Ts, typename... Us, typename... Vs>
struct sum_values_t_impl<sum<Ts...>, sum<Us...>, Vs...> {
    using type = typename sum_values_t_impl<sum<Ts...>, Us..., Vs...>::type;
};

template <typename... Ts, typename Head, typename... Tail>
struct sum_values_t_impl<sum<Ts...>, Head, Tail...> {
    using type = std::conditional_t<
        contains_type_v<Head, Ts...>,
        typename sum_values_t_impl<sum<Ts...>, Tail...>::type,
        typename sum_values_t_impl<sum<Ts..., Head>, Tail...>::type
    >;
};

} /* namespace detail */

template <typename... Ts>
using sum_values_t = typename detail::sum_values_t_impl<sum<>, Ts...>::type;

namespace detail {

// XXX(LPeter1997): Noexcept specifier
template <typename RetT, typename T>
constexpr auto sum_values_impl(std::false_type, T&& val) {
    return RetT(cppcmb_fwd(val));
}

// XXX(LPeter1997): Noexcept specifier
template <typename RetT, typename T>
constexpr decltype(auto) sum_values_impl(std::true_type, T&& val) {
    return std::visit(
        [](auto&& v) -> RetT { return RetT(cppcmb_fwd(v)); },
        cppcmb_fwd(val).as_variant()
    );
}

} /* namespace detail */

// XXX(LPeter1997): Noexcept specifier
template <typename... Ts, typename T>
constexpr auto sum_values(T&& val) {
    return detail::sum_values_impl<sum_values_t<Ts...>>(
        detail::is_sum<detail::remove_cvref_t<T>>(),
        cppcmb_fwd(val)
    );
}

namespace detail {

// XXX(LPeter1997): Noexcept specifier
// Arg is a product
template <typename Fn, typename T>
[[nodiscard]] constexpr decltype(auto) apply_value_impl(
    std::true_type,
    std::false_type,
    Fn&& fn, T&& arg) {

    return std::apply(
        cppcmb_fwd(fn), cppcmb_fwd(arg).as_tuple()
    );
}

// XXX(LPeter1997): Noexcept specifier
// Arg is a sum
template <typename Fn, typename T>
[[nodiscard]] constexpr decltype(auto) apply_value_impl(
    std::false_type,
    std::true_type,
    Fn&& fn, T&& arg) {

    return std::visit(
        cppcmb_fwd(fn), cppcmb_fwd(arg).as_variant()
    );
}

// XXX(LPeter1997): Noexcept specifier
// Arg is a single value
template <typename Fn, typename T>
[[nodiscard]] constexpr decltype(auto) apply_value_impl(
    std::false_type,
    std::false_type,
    Fn&& fn, T&& arg) {

    return cppcmb_fwd(fn)(cppcmb_fwd(arg));
}

} /* namespace detail */

// XXX(LPeter1997): Noexcept specifier
// XXX(LPeter1997): Make apply recursive, so that underlying sums or
// products can get unwrapped too
template <typename Fn, typename T>
[[nodiscard]] constexpr decltype(auto) apply_value(Fn&& fn, T&& arg) {
    return detail::apply_value_impl(
        detail::is_product<detail::remove_cvref_t<T>>(),
        detail::is_sum<detail::remove_cvref_t<T>>(),
        cppcmb_fwd(fn),
        cppcmb_fwd(arg)
    );
}

namespace detail {

/**
 * A tag-type for every combinator.
 */
class combinator_base {};

/**
 * CRTP helper.
 * @see https://www.fluentcpp.com/2017/05/19/crtp-helper/
 */
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

/**
 * Check if a type correctly defives from the combinator base.
 */
template <typename T>
using is_combinator = std::is_base_of<detail::combinator_base, T>;

template <typename T>
inline constexpr bool is_combinator_v = is_combinator<T>::value;

// Forward-declare the action combinator
template <typename Cmb, typename Fn>
class action_t;

namespace detail {

// Helpers for operators

template <typename T>
inline constexpr bool is_combinator_cvref_v =
    is_combinator_v<remove_cvref_t<T>>;

template <typename... Ts>
inline constexpr bool all_combinators_cvref_v =
    (... & is_combinator_cvref_v<Ts>);

} /* namespace detail */

/**
 * Every combinator must derive from this (so we can inject the subscript
 * operator).
 */
#define cppcmb_noexcept_subscript(...) \
noexcept(noexcept(action_t(std::declval<__VA_ARGS__>(), cppcmb_fwd(fn))))

template <typename Self>
class combinator : public detail::crtp<Self>,
                   private detail::combinator_base {
public:
    template <typename Fn>
    [[nodiscard]] constexpr auto operator[](Fn&& fn) &
        cppcmb_noexcept_subscript(Self&) {

        return action_t(this->self(), cppcmb_fwd(fn));
    }

    template <typename Fn>
    [[nodiscard]] constexpr auto operator[](Fn&& fn) const&
        cppcmb_noexcept_subscript(Self const&) {

        return action_t(this->self(), cppcmb_fwd(fn));
    }

    template <typename Fn>
    [[nodiscard]] constexpr auto operator[](Fn&& fn) &&
        cppcmb_noexcept_subscript(Self&&) {

        return action_t(std::move(this->self()), cppcmb_fwd(fn));
    }

    template <typename Fn>
    [[nodiscard]] constexpr auto operator[](Fn&& fn) const&&
        cppcmb_noexcept_subscript(Self const&&) {

        return action_t(std::move(this->self()), cppcmb_fwd(fn));
    }
};

#undef cppcmb_noexcept_subscript

namespace detail {

/**
 * Concept check for parser interface.
 */
template <typename T, typename Src>
using apply_t = decltype(
    std::declval<T>().apply(std::declval<reader<Src> const&>())
);

template <typename T, typename Src>
inline constexpr bool has_parser_interface_v = std::conjunction_v<
    is_detected<apply_t, T, Src>,
    is_combinator<T>
>;

} /* namespace detail */

// We will not undef this as users could use it
#define cppcmb_assert_parser(p, src)                  \
static_assert(                                        \
    ::cppcmb::detail::has_parser_interface_v<p, src>, \
    "A parser must be derived from combinator<Self> " \
    " and have a member function apply(reader<Src>)!" \
    " (note: apply has to be const-qualified!)"       \
)

/**
 * Helper to get the parser result.
 */
template <typename P, typename Src>
using parser_result_t = detail::remove_cvref_t<decltype(
    std::declval<P>().apply(std::declval<reader<Src> const&>())
)>;

/**
 * Helper to get the result value of a parse success.
 */
template <typename P, typename Src>
using parser_value_t = detail::remove_cvref_t<decltype(
    std::declval<parser_result_t<P, Src>>().success().value()
)>;

/**
 * Some type-constructor for maybe.
 */
template <typename T>
class some {
private:
    cppcmb_self_check(some);

    T m_Value;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename TFwd, cppcmb_requires_t(!is_self_v<TFwd>)>
    constexpr some(TFwd&& val)
        : m_Value(cppcmb_fwd(val)) {
    }

    cppcmb_getter(value, m_Value)
};

template <typename TFwd>
some(TFwd) -> some<TFwd>;

/**
 * None type-constructor for maybe.
 */
class none {};

/**
 * Generic maybe-type.
 */
template <typename T>
class maybe {
private:
    cppcmb_self_check(maybe);

    using some_type = ::cppcmb::some<T>;
    using none_type = ::cppcmb::none;

    std::variant<some_type, none_type> m_Data;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename TFwd, cppcmb_requires_t(!is_self_v<TFwd>)>
    constexpr maybe(TFwd&& val)
        : m_Data(cppcmb_fwd(val)) {
    }

    [[nodiscard]] constexpr bool is_some() const noexcept {
        return std::holds_alternative<some_type>(m_Data);
    }

    [[nodiscard]] constexpr bool is_none() const noexcept {
        return std::holds_alternative<none_type>(m_Data);
    }

    cppcmb_getter(some, std::get<some_type>(m_Data))
    cppcmb_getter(none, std::get<none_type>(m_Data))
};

namespace detail {

cppcmb_is_specialization(maybe);

} /* namespace detail */

/**
 * Action combinator, applies a function to the result when the sub-parser
 * succeeds.
 */
template <typename P, typename Fn>
class action_t : public combinator<action_t<P, Fn>> {
private:
    P  m_Parser;
    Fn m_Fn;

public:
    // XXX(LPeter1997): Is it right to have such a long noexcept specifier?
    template <typename PFwd, typename FnFwd>
    constexpr action_t(PFwd&& cmb, FnFwd&& fn)
        noexcept(
            std::is_nothrow_constructible_v<P, PFwd&&>
         && std::is_nothrow_constructible_v<Fn, FnFwd&&>
        )
        : m_Parser(cppcmb_fwd(cmb)), m_Fn(cppcmb_fwd(fn)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr decltype(auto) apply(reader<Src> const& src) const {
        cppcmb_assert_parser(P, Src);

        using value_t = parser_value_t<P, Src>;

        using apply_t = decltype(&action_t::apply_fn<value_t&&>);
        // XXX(LPeter1997): Maybe check with invoke result
        // to make sure type-deduction happened?
        static_assert(
            std::is_invocable_v<apply_t, action_t, value_t>,
            "The given action function must be invocable with the parser's "
            "successful value type!"
            " (note: the function's invocation must be const-qualified!)"
        );

        using fn_result_t = std::invoke_result_t<apply_t, action_t, value_t>;
        using dispatch_tag =
            detail::is_maybe<detail::remove_cvref_t<fn_result_t>>;

        return apply_impl<fn_result_t>(src, dispatch_tag());
    }

private:
    template <typename T>
    using maybe_value_t = detail::remove_cvref_t<decltype(
        std::declval<T>().some().value()
    )>;

    // XXX(LPeter1997): Noexcept specifier
    // Action can fail
    template <typename FRes, typename Src>
    [[nodiscard]] constexpr auto apply_impl(
        reader<Src> const& src,
        std::true_type) const -> result<maybe_value_t<FRes>> {

        using result_t = result<maybe_value_t<FRes>>;

        auto inv = m_Parser.apply(src);
        if (inv.is_failure()) {
            // Early failure
            return result_t(std::move(inv).failure(), inv.furthest());
        }

        auto succ = std::move(inv).success();
        // Try to apply the action
        auto act_inv = apply_fn(std::move(succ).value());
        // In any case we will have to decorate the result with the position
        if (act_inv.is_none()) {
            return result_t(failure(), inv.furthest());
        }
        else {
            return result_t(
                success(std::move(act_inv).some().value(), succ.matched()),
                inv.furthest()
            );
        }
    }

    // XXX(LPeter1997): Noexcept specifier
    // Action can't fail
    template <typename FRes, typename Src>
    [[nodiscard]] constexpr auto apply_impl(
        reader<Src> const& src,
        std::false_type) const -> result<FRes> {

        auto inv = m_Parser.apply(src);
        if (inv.is_failure()) {
            // Early failure
            return result<FRes>(
                std::move(inv).failure(),
                inv.furthest()
            );
        }

        auto succ = std::move(inv).success();
        // Apply the action
        auto act_val = apply_fn(std::move(succ).value());
        // Wrap it in a success
        return result<FRes>(
            success(std::move(act_val), succ.matched()),
            inv.furthest()
        );
    }

    // Invoke the function with a value
    template <typename T>
    [[nodiscard]] constexpr decltype(auto) apply_fn(T&& val) const {
        return apply_value(m_Fn, cppcmb_fwd(val));
    }
};

template <typename PFwd, typename FnFwd>
action_t(PFwd, FnFwd) -> action_t<PFwd, FnFwd>;

/**
 * A parser that always succeeds.
 */
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

/**
 * A parser that simply consumes a single element. Succeeds if there is an
 * element to consume.
 */
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

/**
 * A parser that succeeds with an empty result if the input has ended.
 */
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
        else {
            return result<product<>>(failure(), 0U);
        }
    }
};

// Value for 'end' parser
inline constexpr end_t end = end_t();

/**
 * A parser that applies the first, then the second parser if the first one
 * succeeded, then concatenates the results. Only succeeds if both succeeds.
 */
template <typename P1, typename P2>
class seq_t : public combinator<seq_t<P1, P2>> {
private:
    template <typename Src>
    using value_t = decltype(product_values(
        std::declval<parser_value_t<P1, Src>>(),
        std::declval<parser_value_t<P2, Src>>()
    ));

    P1 m_First;
    P2 m_Second;

public:
    template <typename P1Fwd, typename P2Fwd>
    constexpr seq_t(P1Fwd&& p1, P2Fwd&& p2)
        noexcept(
            std::is_nothrow_constructible_v<P1, P1Fwd&&>
         && std::is_nothrow_constructible_v<P2, P2Fwd&&>
        )
        : m_First(cppcmb_fwd(p1)), m_Second(cppcmb_fwd(p2)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<value_t<Src>> {
        cppcmb_assert_parser(P1, Src);
        cppcmb_assert_parser(P2, Src);

        using result_t = result<value_t<Src>>;

        auto p1_inv = m_First.apply(r);
        if (p1_inv.is_failure()) {
            // Early failure, don't continue
            return result_t(std::move(p1_inv).failure(), p1_inv.furthest());
        }
        // Get the success alternative
        auto p1_succ = std::move(p1_inv).success();
        // Create the next reader
        auto r2 = reader(
            r.source(), r.cursor() + p1_succ.matched(), r.context_ptr()
        );
        // Invoke the second parser
        auto p2_inv = m_Second.apply(r2);
        // Max peek distance
        auto max_furthest = std::max(
            p1_inv.furthest(),
            p1_succ.matched() + p2_inv.furthest()
        );
        if (p2_inv.is_failure()) {
            // Second failed, fail on that error
            return result_t(
                std::move(p2_inv).failure(),
                max_furthest
            );
        }
        // Get the success alternative
        auto p2_succ = std::move(p2_inv).success();
        // Combine the values
        return result_t(
            success(
                product_values(
                    std::move(p1_succ).value(),
                    std::move(p2_succ).value()
                ),
                p1_succ.matched() + p2_succ.matched()
            ),
            max_furthest
        );
    }
};

template <typename P1Fwd, typename P2Fwd>
seq_t(P1Fwd, P2Fwd) -> seq_t<P1Fwd, P2Fwd>;

/**
 * Operator for making a sequence.
 */
template <typename P1, typename P2,
    cppcmb_requires_t(detail::all_combinators_cvref_v<P1, P2>)>
[[nodiscard]] constexpr auto operator&(P1&& p1, P2&& p2)
    cppcmb_return(seq_t(cppcmb_fwd(p1), cppcmb_fwd(p2)))

/**
 * A tag-type for a more uniform alternative syntax.
 * This can be put as the first element of an alternative chain so every new
 * line can start with the alternative operator. It's completely ignored.
 * Example:
 * auto parser = pass
 *             | first
 *             | second
 *             ;
 */
struct pass_t {};

inline constexpr auto pass = pass_t();

/**
 * A parser that tries to parse the first alternative. If that fails,
 * applies the second one. Succeeds if at least one succeeds.
 */
template <typename P1, typename P2>
class alt_t : public combinator<alt_t<P1, P2>> {
private:
    template <typename Src>
    using value_t = sum_values_t<
        parser_value_t<P1, Src>,
        parser_value_t<P2, Src>
    >;

    P1 m_First;
    P2 m_Second;

public:
    template <typename P1Fwd, typename P2Fwd>
    constexpr alt_t(P1Fwd&& p1, P2Fwd&& p2)
        noexcept(
            std::is_nothrow_constructible_v<P1, P1Fwd&&>
         && std::is_nothrow_constructible_v<P2, P2Fwd&&>
        )
        : m_First(cppcmb_fwd(p1)), m_Second(cppcmb_fwd(p2)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<value_t<Src>> {
        cppcmb_assert_parser(P1, Src);
        cppcmb_assert_parser(P2, Src);

        using result_t = result<value_t<Src>>;

        // Try to apply the first alternative
        auto p1_inv = m_First.apply(r);
        if (p1_inv.is_success()) {
            auto p1_succ = std::move(p1_inv).success();
            return result_t(
                success(
                    sum_values<value_t<Src>>(std::move(p1_succ).value()),
                    p1_succ.matched()
                ),
                p1_inv.furthest()
            );
        }

        // Try to apply the second alternative
        auto p2_inv = m_Second.apply(r);
        if (p2_inv.is_success()) {
            auto p2_succ = std::move(p2_inv).success();
            return result_t(
                success(
                    sum_values<value_t<Src>>(std::move(p2_succ).value()),
                    p2_succ.matched()
                ),
                std::max(p1_inv.furthest(), p2_inv.furthest())
            );
        }

        // Both failed, return the error which got further
        auto p1_err = std::move(p1_inv).failure();
        auto p2_err = std::move(p2_inv).failure();

        if (p1_inv.furthest() > p2_inv.furthest()) {
            return result_t(std::move(p1_err), p1_inv.furthest());
        }
        else if (p1_inv.furthest() < p2_inv.furthest()) {
            return result_t(std::move(p2_err), p2_inv.furthest());
        }
        else {
            // They got to the same distance, need to merge errors
            // XXX(LPeter1997): Implement, for now we just return the first
            return result_t(std::move(p1_err), p1_inv.furthest());
        }
    }
};

template <typename P1Fwd, typename P2Fwd>
alt_t(P1Fwd, P2Fwd) -> alt_t<P1Fwd, P2Fwd>;

/**
 * Operator for making alternatives.
 */
template <typename P1, typename P2,
    cppcmb_requires_t(detail::all_combinators_cvref_v<P1, P2>)>
[[nodiscard]] constexpr auto operator|(P1&& p1, P2&& p2)
    cppcmb_return(alt_t(cppcmb_fwd(p1), cppcmb_fwd(p2)))

/**
 * Ignore pass.
 */
template <typename P2,
    cppcmb_requires_t(detail::is_combinator_cvref_v<P2>)>
[[nodiscard]] constexpr auto operator|(pass_t, P2&& p2)
    cppcmb_return(cppcmb_fwd(p2))

/**
 * A parser that tries all alternatives and then returns the furthest
 * advanced alternative.
 */
template <typename P1, typename P2>
class eager_alt_t : public combinator<eager_alt_t<P1, P2>> {
private:
    template <typename Src>
    using value_t = sum_values_t<
        parser_value_t<P1, Src>,
        parser_value_t<P2, Src>
    >;

    P1 m_First;
    P2 m_Second;

public:
    template <typename P1Fwd, typename P2Fwd>
    constexpr eager_alt_t(P1Fwd&& p1, P2Fwd&& p2)
        noexcept(
            std::is_nothrow_constructible_v<P1, P1Fwd&&>
         && std::is_nothrow_constructible_v<P2, P2Fwd&&>
        )
        : m_First(cppcmb_fwd(p1)), m_Second(cppcmb_fwd(p2)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<value_t<Src>> {
        cppcmb_assert_parser(P1, Src);
        cppcmb_assert_parser(P2, Src);

        using result_t = result<value_t<Src>>;

        // Try to apply both alternatives
        auto p1_inv = m_First.apply(r);
        auto p2_inv = m_Second.apply(r);

        auto furthest = std::max(p1_inv.furthest(), p2_inv.furthest());

        // Both succeeded
        if (p1_inv.is_success() && p2_inv.is_success()) {
            // Return the one that got further
            // If they both got the same distance, return the first one
            auto p1_succ = std::move(p1_inv).success();
            auto p2_succ = std::move(p2_inv).success();

            if (p1_succ.matched() >= p2_succ.matched()) {
                return result_t(success(
                    sum_values<value_t<Src>>(std::move(p1_succ).value()),
                    p1_succ.matched()
                ), furthest);
            }
            else {
                return result_t(success(
                    sum_values<value_t<Src>>(std::move(p2_succ).value()),
                    p2_succ.matched()
                ), furthest);
            }
        }
        // LHS succeeded
        if (p1_inv.is_success()) {
            auto p1_succ = std::move(p1_inv).success();
            return result_t(success(
                sum_values<value_t<Src>>(std::move(p1_succ).value()),
                p1_succ.matched()
            ), furthest);
        }
        // RHS succeeded
        if (p2_inv.is_success()) {
            auto p2_succ = std::move(p2_inv).success();
            return result_t(success(
                sum_values<value_t<Src>>(std::move(p2_succ).value()),
                p2_succ.matched()
            ), furthest);
        }

        // Both failed, return the error which got further
        auto p1_err = std::move(p1_inv).failure();
        auto p2_err = std::move(p2_inv).failure();

        if (p1_inv.furthest() > p2_inv.furthest()) {
            return result_t(std::move(p1_err), furthest);
        }
        else if (p1_inv.furthest() < p2_inv.furthest()) {
            return result_t(std::move(p2_err), furthest);
        }
        else {
            // They got to the same distance, need to merge errors
            // XXX(LPeter1997): Implement, for now we just return the first
            return result_t(std::move(p1_err), furthest);
        }
    }
};

template <typename P1Fwd, typename P2Fwd>
eager_alt_t(P1Fwd, P2Fwd) -> eager_alt_t<P1Fwd, P2Fwd>;

/**
 * Operator for making eager alternatives.
 */
template <typename P1, typename P2,
    cppcmb_requires_t(detail::all_combinators_cvref_v<P1, P2>)>
[[nodiscard]] constexpr auto operator||(P1&& p1, P2&& p2)
    cppcmb_return(eager_alt_t(cppcmb_fwd(p1), cppcmb_fwd(p2)))

/**
 * Ignore pass.
 */
template <typename P2,
    cppcmb_requires_t(detail::is_combinator_cvref_v<P2>)>
[[nodiscard]] constexpr auto operator||(pass_t, P2&& p2)
    cppcmb_return(cppcmb_fwd(p2))

/**
 * A type-pack that describes a collection except it's type.
 * Used for the many and many1 combinators.
 */
template <
    template <typename...> typename Coll,
    template <typename> typename... Ts
>
struct collect_to_t {
    template <typename T>
    using type = Coll<T, Ts<T>...>;
};

template <
    template <typename...> typename Coll,
    template <typename> typename... Ts
>
inline constexpr auto collect_to = collect_to_t<Coll, Ts...>();

namespace detail {

/**
 * Tag-type for many and many1.
 */
struct many_tag {};

/**
 * SFINAE for many types.
 */
template <typename T>
inline constexpr bool is_many_v = std::is_base_of_v<many_tag, T>;

} /* namespace detail */

/**
 * A parser that applies a parser as many times as it can, before it fails.
 * Collects the results into some container. Always succeeds.
 */
template <typename P, typename To = collect_to_t<std::vector>>
class many_t : public combinator<many_t<P>>,
               private detail::many_tag {
private:
    cppcmb_self_check(many_t);

    template <typename Src>
    using value_t = typename To::template type<parser_value_t<P, Src>>;

    P m_Parser;

public:
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr many_t(PFwd&& p)
        noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
        : m_Parser(cppcmb_fwd(p)) {
    }

    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) &
        cppcmb_return(many_t<P, To2>(m_Parser))

    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) const&
        cppcmb_return(many_t<P, To2>(m_Parser))
    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) &&
        cppcmb_return(many_t<P, To2>(std::move(m_Parser)))
    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) const&&
        cppcmb_return(many_t<P, To2>(std::move(m_Parser)))

    cppcmb_getter(underlying, m_Parser)

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<value_t<Src>> {
        cppcmb_assert_parser(P, Src);

        using result_t = result<value_t<Src>>;

        std::size_t furthest = 0U;
        std::size_t matched = 0U;
        auto coll = value_t<Src>();
        auto rr = r;
        while (true) {
            auto p_inv = m_Parser.apply(rr);
            furthest = std::max(furthest, matched + p_inv.furthest());
            if (p_inv.is_failure()) {
                // Stop applying
                break;
            }
            auto p_succ = std::move(p_inv).success();
            matched += p_succ.matched();
            // Add to collection
            coll.push_back(std::move(p_succ).value());
            // Move reader
            rr.seek(rr.cursor() + p_succ.matched());
        }
        return result_t(success(std::move(coll), matched), furthest);
    }
};

template <typename PFwd>
many_t(PFwd) -> many_t<PFwd>;

/**
 * Operator for making many parser.
 */
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
[[nodiscard]] constexpr auto operator*(P&& p)
    cppcmb_return(many_t(cppcmb_fwd(p)))

/**
 * A parser that applies a parser as many times as it can, before it fails.
 * Only succeeds if the parser could be applied at least once.
 */
template <typename P, typename To = collect_to_t<std::vector>>
class many1_t : public combinator<many1_t<P>>,
                private detail::many_tag {
private:
    cppcmb_self_check(many1_t);

    template <typename Src>
    using value_t = parser_value_t<many_t<P, To>, Src>;

    many_t<P, To> m_Parser;

public:
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr many1_t(PFwd&& p)
        noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
        : m_Parser(many_t<P, To>(cppcmb_fwd(p))) {
    }

    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) &
        cppcmb_return(many1_t<P, To2>(m_Parser.underlying()))
    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) const&
        cppcmb_return(many1_t<P, To2>(m_Parser.underlying()))
    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) &&
        cppcmb_return(many1_t<P, To2>(std::move(m_Parser).underlying()))
    template <typename To2>
    [[nodiscard]] constexpr auto collect_to(To2) const&&
        cppcmb_return(many1_t<P, To2>(std::move(m_Parser).underlying()))

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<value_t<Src>> {
        cppcmb_assert_parser(P, Src);

        using result_t = result<value_t<Src>>;

        auto p_inv = m_Parser.apply(r);

        cppcmb_assert(
            "The underlying 'many' parser must always succeed!",
            p_inv.is_success()
        );

        auto p_succ = std::move(p_inv).success();
        if (p_succ.value().size() > 0) {
            // Succeed
            return result_t(std::move(p_succ), p_inv.furthest());
        }
        else {
            // Fail
            return result_t(failure(), p_inv.furthest());
        }
    }
};

template <typename PFwd>
many1_t(PFwd) -> many1_t<PFwd>;

/**
 * Operator for making many1 parser.
 */
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
[[nodiscard]] constexpr auto operator+(P&& p)
    cppcmb_return(many1_t(cppcmb_fwd(p)))

/**
 * Operator to collect 'many' and 'many1' to a different container.
 */
template <typename P, typename To,
    cppcmb_requires_t(detail::is_many_v<detail::remove_cvref_t<P>>)>
[[nodiscard]] constexpr auto operator>>(P&& p, To to)
    cppcmb_return(cppcmb_fwd(p).collect_to(to))

/**
 * A parser that wraps the underlying parse result into a maybe-type.
 * Always succeeds, but the maybe-type only contains a value if the
 * underlying parser succeeds.
 */
template <typename P>
class opt_t : public combinator<opt_t<P>> {
private:
    cppcmb_self_check(opt_t);

    template <typename Src>
    using value_t = maybe<parser_value_t<P, Src>>;

    P m_Parser;

public:
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr opt_t(PFwd&& p)
        noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
        : m_Parser(cppcmb_fwd(p)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
        -> result<value_t<Src>> {
        cppcmb_assert_parser(P, Src);

        using result_t = result<value_t<Src>>;

        auto p_inv = m_Parser.apply(r);
        if (p_inv.is_failure()) {
            return result_t(
                success(value_t<Src>(none()), 0U),
                p_inv.furthest()
            );
        }
        else {
            auto succ = std::move(p_inv).success();
            return result_t(
                success(
                    value_t<Src>(some(std::move(succ).value())),
                    succ.matched()
                ),
                p_inv.furthest()
            );
        }
    }
};

template <typename PFwd>
opt_t(PFwd) -> opt_t<PFwd>;

/**
 * Operator for making optional parser.
 */
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
[[nodiscard]] constexpr auto operator-(P&& p)
    cppcmb_return(opt_t(cppcmb_fwd(p)))

/**
 * A "wrapper" parser. Does nothing, just calls a cppcmb_parse_rule method
 * with itself. Used to implement recursion.
 */
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

namespace detail {

// XXX(LPeter1997): Can this be constexpr?
/**
 * This is where all the rules are stored internally.
 * They allow us to do a nice assignment-syntax.
 */
template <typename>
inline /* constexpr */ auto rule_set = 0;

template <typename, typename U>
struct second {
    using type = U;
};

} /* namespace detail */

/**
 * A library macro that actually gets published.
 * Used to declare rules.
 */
#define cppcmb_decl(name, ...) \
auto const name =              \
::cppcmb::rule_t<__VA_ARGS__, struct cppcmb_unique_id(cppcmb_rule_tag)>()

// XXX(LPeter1997): Noexcept specifier
/**
 * A library macro that actually gets published.
 * Used to define rules.
 */
#define cppcmb_def(name)                                            \
template <typename Src>                                             \
[[nodiscard]] constexpr auto                                        \
cppcmb_parse_rule(decltype(name), ::cppcmb::reader<Src> const& r) { \
    using tag_type = typename decltype(name)::tag_type;             \
    auto const& p = ::cppcmb::detail::rule_set<                     \
        typename ::cppcmb::detail::second<Src, tag_type>::type      \
    >;                                                              \
    return p.apply(r);                                              \
}                                                                   \
template <>                                                         \
inline /* constexpr */ auto                                         \
::cppcmb::detail::rule_set<typename decltype(name)::tag_type>

namespace detail {

// XXX(LPeter1997): Noexcept specifier
/**
 * Functionality for hashing a pair. Straight from Boost.
 */
template <typename T>
constexpr void hash_combine(std::size_t& seed, T const& v) {
    seed ^= std::hash<T>()(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

struct pair_hasher {
    // XXX(LPeter1997): Noexcept specifier
    template <typename T1, typename T2>
    constexpr auto operator()(std::pair<T1, T2> const& p) const {
        std::size_t seed = 0;
        hash_combine(seed, p.first);
        hash_combine(seed, p.second);
        return seed;
    }
};

/**
 * Memorization table for packrat parsers.
 */
class memo_table {
private:
    // pair<parser identifier, position>
    using key_type = std::pair<std::uintptr_t, std::size_t>;
    // pair<result, furthest>
    using value_type = std::pair<std::any, std::size_t>;

    std::unordered_map<key_type, value_type, pair_hasher> m_Cache;

public:
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]]
    /* constexpr */ std::any* get(std::uintptr_t pid, std::size_t pos) {
        auto it = m_Cache.find({ pid, pos });
        if (it == m_Cache.end()) {
            return nullptr;
        }
        return &it->second.first;
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]]
    constexpr std::any* get(std::uintptr_t pid, reader<Src> const& r) {
        return get(pid, r.cursor());
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename TFwd>
    constexpr auto& put(std::uintptr_t pid, std::size_t pos,
        TFwd&& val, std::size_t furth) {

        using raw_type = remove_cvref_t<TFwd>;
        auto id = std::pair(pid, pos);
        auto& a = (m_Cache[id] = std::pair(cppcmb_fwd(val), furth));
        return std::any_cast<raw_type&>(a.first);
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src, typename TFwd>
    constexpr auto& put(std::uintptr_t pid, reader<Src> const& r,
        TFwd&& val, std::size_t furth) {

        return put(pid, r.cursor(), cppcmb_fwd(val), furth);
    }

    // XXX(LPeter1997): Noexcept specifier
    /* constexpr */ void clear() {
        m_Cache.clear();
    }

    // XXX(LPeter1997): Noexcept specifier
    void invalidate(std::size_t start, std::size_t rem, std::size_t ins) {
        // start: Position of the source we are manipulating
        // rem: Removed length
        // ins: Inserted length

        auto end = start + rem;

        // XXX(LPeter1997): Going through every entry is not very effective
        // we would need some helper structure to search by interval

        for (auto it = m_Cache.begin(); it != m_Cache.end();) {
            auto r_from = it->first.second;
            // XXX(LPeter1997): Solve this
            // Maybe redundantly store it
            auto r_furthest = it->second.second;
            auto r_to = r_from + r_furthest;

            // [f_from; r_to) is the entry's interval
            // Need to check overlap with [start; end)
            // If they overlap, remove entry

            // XXX(LPeter1997): Allow equality?
            if (start > r_to || r_from > end) {
                // No overlap
                ++it;
            }
            else {
                // Overlapping
                it = m_Cache.erase(it);
            }
        }

        // XXX(LPeter1997): THIS IS HORRIBLE FOR PERFORMANCE
        // WE ARE REMOVING THEN PUTTING BACK EVERY ENTRY THAT IS AFTER THE
        // EDIT
        // XXX(LPeter1997): This is a very ineffective implementation right
        // now. It's just to test the algorithm itself
        std::intptr_t diff = std::intptr_t(ins) - std::intptr_t(rem);
        // Collect and erase entries that need to be shifted
        std::vector<std::pair<key_type, value_type>> to_shift;
        for (auto it = m_Cache.begin(); it != m_Cache.end();) {
            auto r_from = it->first.second;
            if (r_from >= start) {
                to_shift.push_back({
                    { it->first.first, it->first.second },
                    std::move(it->second)
                });
                it = m_Cache.erase(it);
            }
            else {
                ++it;
            }
        }
        // Re-insert the entries
        for (auto& [k, v] : to_shift) {
            auto& [p_id, pos] = k;
            m_Cache.insert({ { p_id, pos + diff }, std::move(v) });
        }
        // END OF UNGODLY INEFFICIENT CODE
    }
};

template <typename Self>
class packrat_base : public combinator<Self> {
private:
    std::uintptr_t m_ID;

protected:
    constexpr packrat_base() noexcept
        : m_ID(reinterpret_cast<std::uintptr_t>(this)) {
    }

    [[nodiscard]] constexpr auto const& original_id() const noexcept {
        return m_ID;
    }

    // XXX(LPeter1997): This could be static (or removed)
    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    constexpr auto& context(reader<Src> const& r) const {
        return r.context();
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src, typename TFwd>
    constexpr auto& put_memo(reader<Src> const& r,
        TFwd&& val, std::size_t furth) const {

        auto& table = context(r).memo();
        return table.put(original_id(), r, cppcmb_fwd(val), furth);
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr std::any* get_memo(reader<Src> const& r) const {
        auto& table = context(r).memo();
        return table.get(original_id(), r);
    }
};

} /* namespace detail */

/**
 * A memorizing parser to avoid duplicate application.
 */
template <typename P>
class packrat_t : public detail::packrat_base<packrat_t<P>> {
private:
    cppcmb_self_check(packrat_t);

    P m_Parser;

public:
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr packrat_t(PFwd&& p)
        noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
        : m_Parser(cppcmb_fwd(p)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr decltype(auto) apply(reader<Src> const& r) const {
        cppcmb_assert_parser(P, Src);

        using result_t = parser_result_t<P, Src>;

        auto* entry = this->get_memo(r);
        if (entry == nullptr) {
            auto res = m_Parser.apply(r);
            return this->put_memo(r, std::move(res), res.furthest());
        }
        else {
            return std::any_cast<result_t&>(*entry);
        }
    }
};

template <typename PFwd>
packrat_t(PFwd) -> packrat_t<PFwd>;

/**
 * Wrapper to make any combinator a packrat parser.
 */
template <typename PFwd>
[[nodiscard]] constexpr auto memo(PFwd&& p)
    cppcmb_return(packrat_t(cppcmb_fwd(p)))

namespace detail {

template <typename T, typename Tag>
class tagged_wrapper {
private:
    cppcmb_self_check(tagged_wrapper);

    T m_Value;

public:
    template <typename TFwd>
    constexpr tagged_wrapper(TFwd&& val)
        noexcept(std::is_nothrow_constructible_v<T, TFwd&&>)
        : m_Value(cppcmb_fwd(val)) {
    }

    cppcmb_getter(value, m_Value)
};

} /* namespace detail */

/**
 * A direct left-recursion-capable packrat parser.
 */
template <typename P>
class drec_packrat_t : public detail::packrat_base<drec_packrat_t<P>> {
private:
    cppcmb_self_check(drec_packrat_t);

    template <typename T>
    using base_recursion = detail::tagged_wrapper<T, struct drec_base_tag>;

    template <typename T>
    using in_recursion = detail::tagged_wrapper<T, struct drec_in_tag>;

    P m_Parser;

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]]
    constexpr decltype(auto) grow(
        reader<Src> const& r,
        parser_result_t<P, Src>& old_res
    ) const {

        using in_rec = in_recursion<parser_result_t<P, Src>>;

        if (old_res.is_failure()) {
            return old_res;
        }
        auto& old_succ = old_res.success();

        auto tmp_res = m_Parser.apply(r);
        auto max_furthest = std::max(old_res.furthest(), tmp_res.furthest());
        if (tmp_res.is_success()) {
            auto& tmp_succ = tmp_res.success();
            if (old_succ.matched() < tmp_succ.matched()) {
                // XXX(LPeter1997): The actual result doesn't hold the
                // max_furthest value!

                // We successfully grew the seed
                auto& new_old = this->put_memo(
                    r, in_rec(tmp_res), max_furthest
                ).value();
                return grow(r, new_old);
            }
            else {
                // We need to overwrite max-furthest in the memo-table!
                // That's why we don't simply return old_res
                return this->put_memo(
                    r, in_rec(old_res), max_furthest
                ).value();
            }
        }
        else {
            // We need to overwrite max-furthest in the memo-table!
            // That's why we don't simply return old_res
            // XXX(LPeter1997): The actual result doesn't hold the
            // max_furthest value!
            return this->put_memo(
                r, in_rec(old_res), max_furthest
            ).value();
        }
    }

public:
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr drec_packrat_t(PFwd&& p)
        noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
        : m_Parser(cppcmb_fwd(p)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr decltype(auto) apply(reader<Src> const& r) const {
        cppcmb_assert_parser(P, Src);

        using result_t = parser_result_t<P, Src>;
        using base_rec = std::pair<base_recursion<result_t>, bool>;
        using in_rec = in_recursion<result_t>;

        auto* entry = this->get_memo(r);
        if (entry == nullptr) {
            // Nothing is in the cache yet, write a dummy error
            this->put_memo(r, base_rec(result_t(failure(), 0U), true), 0U);
            // Now invoke the parser
            // If it's recursive, the entry must have changed
            auto tmp_res = m_Parser.apply(r);
            // Refresh entry
            entry = this->get_memo(r);
            cppcmb_assert(
                "The entry cannot be nullptr here!",
                entry != nullptr
            );

            // Check for change
            if (auto* inr = std::any_cast<in_rec>(entry)) {
                // We are in recursion
                auto& res = this->put_memo(
                    r, in_rec(std::move(tmp_res)), tmp_res.furthest()
                ).value();
                return grow(r, res);
            }
            else {
                // Base-thing, no progress
                // Overwrite the base-type to contain the result
                auto* br = std::any_cast<base_rec>(entry);
                cppcmb_assert(
                    "A direct-packrat parser must either enter a "
                    "'base' or 'in'-recursion entry!",
                    br != nullptr
                );
                return this->put_memo(
                    r,
                    base_rec(std::move(tmp_res), false),
                    tmp_res.furthest()
                ).first.value();
            }
        }
        else {
            // Something is in the cache
            if (auto* br_p = std::any_cast<base_rec>(entry)) {
                auto& br = *br_p;
                if (br.second) {
                    // Recursion signal
                    return this->put_memo(
                        r,
                        in_rec(result_t(failure(), 0U)),
                        0U
                    ).value();
                }
                else {
                    return br.first.value();
                }
            }
            else {
                auto* inr = std::any_cast<in_rec>(entry);
                cppcmb_assert(
                    "A direct-packrat parser must either enter a "
                    "'base' or 'in'-recursion entry!",
                    inr != nullptr
                );
                return inr->value();
            }
        }
    }
};

template <typename PFwd>
drec_packrat_t(PFwd) -> drec_packrat_t<PFwd>;

/**
 * Wrapper to make any combinator a direct-left-recursive packrat parser.
 */
template <typename PFwd>
[[nodiscard]] constexpr auto memo_d(PFwd&& p)
    cppcmb_return(drec_packrat_t(cppcmb_fwd(p)))

namespace detail {

class irec_head {
private:
    std::uintptr_t                     m_HeadID;
    std::unordered_set<std::uintptr_t> m_InvolvedIDSet;
    std::unordered_set<std::uintptr_t> m_EvalIDSet;

public:
    // XXX(LPeter1997): Noexcept specifier
    explicit /* constexpr */ irec_head(std::uintptr_t hid)
        : m_HeadID(hid) {
    }

    [[nodiscard]] constexpr std::uintptr_t head_id() const noexcept {
        return m_HeadID;
    }

    cppcmb_getter(involved_set, m_InvolvedIDSet)
    cppcmb_getter(eval_set, m_EvalIDSet)
};

class irec_left_recursive {
private:
    std::any                 m_Seed;
    std::uintptr_t           m_ParserID;
    std::optional<irec_head> m_Head;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename TFwd>
    constexpr irec_left_recursive(TFwd&& seed, std::uintptr_t pid)
        : m_Seed(cppcmb_fwd(seed)), m_ParserID(pid) {
    }

    cppcmb_getter(seed, m_Seed)
    cppcmb_getter(head, m_Head)

    [[nodiscard]] constexpr std::uintptr_t parser_id() const noexcept {
        return m_ParserID;
    }
};

} /* namespace detail */

// XXX(LPeter1997): Remove this function! Maybe add a custom type with
// this function
namespace detail {

template <typename Coll, typename Val, typename It>
constexpr bool contains(Coll const& coll, Val const& v, It& it) {
    it = coll.find(v);
    return it != coll.end();
}

template <typename Coll, typename Val>
constexpr bool contains(Coll const& coll, Val const& v) {
    auto it = coll.end();
    return contains(coll, v, it);
}

} /* namespace detail */

/**
 * An indirect left-recursion-capable packrat parser.
 * More expensive than the direct one.
 */
template <typename P>
class irec_packrat_t : public detail::packrat_base<irec_packrat_t<P>> {
private:
    cppcmb_self_check(irec_packrat_t);

    using head = detail::irec_head;
    using left_recursive = detail::irec_left_recursive;

    P m_Parser;

    // XXX(LPeter1997): decltype(auto) where possible

    // XXX(LPeter1997): Noexcept specifier
    // XXX(LPeter1997): This could be static
    template <typename T>
    constexpr decltype(auto) to_result(std::any& a) const {
        if (auto* r = std::any_cast<std::shared_ptr<left_recursive>>(&a)) {
            return std::any_cast<T&>((*r)->seed());
        }
        else {
            return std::any_cast<T&>(a);
        }
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    /* constexpr */ auto recall(reader<Src> const& r) const
        -> std::optional<std::any> {
        using return_t = parser_result_t<P, Src>;

        auto& heads = r.context().call_heads();

        auto* cached = this->get_memo(r);
        auto* in_heads = heads.get(r);

        if (in_heads == nullptr) {
            if (cached == nullptr) {
                return std::nullopt;
            }
            else {
                return *cached;
            }
        }
        else {
            auto& h = *in_heads;

            if (cached == nullptr && !(
                    this->original_id() == h.head_id()
                || detail::contains(h.involved_set(), this->original_id())
            )) {
                return return_t(failure(), 0U);
            }

            auto it = h.eval_set().cend();
            if (detail::contains(h.eval_set(), this->original_id(), it)) {
                // Remove the rule id from the evaluation id set of the head
                h.eval_set().erase(it);
                auto tmp_res = m_Parser.apply(r);
                *cached = tmp_res;
            }

            return *cached;
        }
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    constexpr void setup_lr(
        reader<Src> const& r,
        left_recursive& rec_detect) const {

        if (!rec_detect.head()) {
            rec_detect.head() = head(this->original_id());
        }
        auto& lr_stack = r.context().call_stack();
        for (
            auto it = lr_stack.begin();
            it != lr_stack.end() && (*it)->parser_id() != this->original_id();
            ++it) {

            (*it)->head() = rec_detect.head();
            rec_detect.head()->involved_set().insert((*it)->parser_id());
        }
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    constexpr auto lr_answer(
        reader<Src> const& r,
        left_recursive& growable) const {

        using return_t = parser_result_t<P, Src>;
        cppcmb_assert(
            "The growable must contain a head!",
            growable.head().has_value()
        );

        auto& h = *growable.head();
        auto& seed = to_result<return_t>(growable.seed());

        if (h.head_id() != this->original_id()) {
            return seed;
        }
        else {
            auto& s = this->put_memo(r, seed, seed.furthest());
            if (s.is_failure()) {
                return s;
            }
            else {
                return grow(r, s, h);
            }
        }
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    constexpr auto grow(
        reader<Src> const& r,
        parser_result_t<P, Src>& old_res,
        head& h) const -> parser_result_t<P, Src> {

        // using return_t = parser_result_t<P, Src>;

        auto& rec_heads = r.context().call_heads();

        rec_heads[r] = &h;
        h.eval_set() = h.involved_set();

        std::size_t old_cur = 0U;
        if (old_res.is_success()) {
            old_cur = old_res.success().matched();
        }

        auto tmp_res = m_Parser.apply(r);
        auto max_furthest = std::max(old_res.furthest(), tmp_res.furthest());
        if (tmp_res.is_success()) {
            auto& tmp_succ = tmp_res.success();
            if (old_cur < tmp_succ.matched()) {
                // XXX(LPeter1997): The actual result doesn't hold the
                // max_furthest value!
                auto& new_old = this->put_memo(
                    r, tmp_res, max_furthest
                );
                return grow(r, new_old, h);
            }
            else {
                // We need to overwrite max-furthest in the memo-table!
                // That's why we don't simply return old_res

                auto it = rec_heads.find(r);
                cppcmb_assert("", it != rec_heads.end());
                rec_heads.erase(it);

                // auto* val = this->get_memo(r);
                // cppcmb_assert("", val != nullptr);

                // return this-> template to_result<return_t>(*val);
                return this->put_memo(r, std::move(old_res), max_furthest);
            }
        }
        else {
            // We need to overwrite max-furthest in the memo-table!
            // That's why we don't simply return old_res

            auto it = rec_heads.find(r);
            cppcmb_assert("", it != rec_heads.end());
            rec_heads.erase(it);

            return this->put_memo(r, std::move(old_res), max_furthest);
        }
    }

public:
    template <typename PFwd, cppcmb_requires_t(!is_self_v<PFwd>)>
    constexpr irec_packrat_t(PFwd&& p)
        noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
        : m_Parser(cppcmb_fwd(p)) {
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]]
    constexpr auto apply(reader<Src> const& r) const {
        cppcmb_assert_parser(P, Src);

        using return_t = parser_result_t<P, Src>;

        auto& lr_stack = r.context().call_stack();

        auto m = recall(r);
        if (!m) {
            auto base = std::make_shared<left_recursive>(
                return_t(failure(), 0U), this->original_id()
            );
            lr_stack.push_front(base);
            this->put_memo(r, base, 0U);
            auto tmp_res = m_Parser.apply(r);
            lr_stack.pop_front();

            if (!base->head()) {
                return this->put_memo(r, tmp_res, tmp_res.furthest());
            }
            else {
                base->seed() = tmp_res;
                return lr_answer(r, *base);
            }
        }
        else {
            auto& entry = *m;
            if (auto* lr =
                std::any_cast<std::shared_ptr<left_recursive>>(&entry)) {

                setup_lr(r, **lr);
                return this-> template to_result<return_t>((*lr)->seed());
            }
            else {
                return this-> template to_result<return_t>(entry);
            }
        }
    }
};

template <typename PFwd>
irec_packrat_t(PFwd) -> irec_packrat_t<PFwd>;

/**
 * Wrapper to make any combinator a direct-left-recursive packrat parser.
 */
template <typename PFwd>
[[nodiscard]] constexpr auto memo_i(PFwd&& p)
    cppcmb_return(irec_packrat_t(cppcmb_fwd(p)))

namespace detail {

/**
 * A type to track call-heads.
 */
class call_head_table {
private:
    std::unordered_map<std::size_t, irec_head*> m_Heads;

public:
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]]
    /* constexpr */ irec_head* get(std::size_t n) const {
        auto it = m_Heads.find(n);
        if (it == m_Heads.end()) {
            return nullptr;
        }
        else {
            return it->second;
        }
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]]
    /* constexpr */ irec_head* get(reader<Src> const& r) const {
        return get(r.cursor());
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    constexpr decltype(auto) operator[](reader<Src> const& r) {
        return m_Heads[r.cursor()];
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto find(reader<Src> const& r) {
        return m_Heads.find(r.cursor());
    }

    // XXX(LPeter1997): Noexcept specifier
    template <typename Src>
    [[nodiscard]] constexpr auto find(reader<Src> const& r) const {
        return m_Heads.find(r.cursor());
    }

    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto begin() { return m_Heads.begin(); }
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto begin() const { return m_Heads.begin(); }
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto end() { return m_Heads.end(); }
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto end() const { return m_Heads.end(); }

    // XXX(LPeter1997): Noexcept specifier
    template <typename It>
    constexpr void erase(It it) {
        m_Heads.erase(it);
    }

    // XXX(LPeter1997): Noexcept specifier
    void clear() {
        m_Heads.clear();
    }
};

class call_stack {
private:
    std::deque<std::shared_ptr<irec_left_recursive>> m_Stack;

public:
    // XXX(LPeter1997): Noexcept specifier
    template <typename TFwd>
    constexpr void push_front(TFwd&& val) {
        m_Stack.push_front(val);
    }

    // XXX(LPeter1997): Noexcept specifier
    /* constexpr */ void pop_front() {
        m_Stack.pop_front();
    }

    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto begin() { return m_Stack.begin(); }
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto begin() const { return m_Stack.begin(); }
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto end() { return m_Stack.end(); }
    // XXX(LPeter1997): Noexcept specifier
    [[nodiscard]] /* constexpr */ auto end() const { return m_Stack.end(); }

    // XXX(LPeter1997): Noexcept specifier
    void clear() {
        m_Stack.clear();
    }
};

} /* namespace detail */

/**
 * A tuple of helper-types that the memorizing parsers can use for
 * memorization and direct/indirect recursion.
 */
class memo_context {
private:
    detail::memo_table      m_MemoTable;
    detail::call_head_table m_RecursionHeads;
    detail::call_stack      m_LrStack;

public:
    cppcmb_getter(memo, m_MemoTable)
    cppcmb_getter(call_heads, m_RecursionHeads)
    cppcmb_getter(call_stack, m_LrStack)

    // XXX(LPeter1997): Noexcept specifier
    void clear() {
        m_MemoTable.clear();
        m_RecursionHeads.clear();
        m_LrStack.clear();
    }
};

/**
 * Here we provide operator%= to turn a parser into packrat parsers.
 */

// Tag-types for the parsers
struct as_self_t {};
struct as_memo_t {};
struct as_memo_d_t {};
struct as_memo_i_t {};

inline constexpr auto as_self = as_self_t();
inline constexpr auto as_memo = as_memo_t();
inline constexpr auto as_memo_d = as_memo_d_t();
inline constexpr auto as_memo_i = as_memo_i_t();

// The operators

// Identity
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
constexpr auto operator%=(P&& parser, as_self_t)
    cppcmb_return(cppcmb_fwd(parser))

// Simple packrat
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
constexpr auto operator%=(P&& parser, as_memo_t)
    cppcmb_return(memo(cppcmb_fwd(parser)))

// Direct-left-recursive packrat
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
constexpr auto operator%=(P&& parser, as_memo_d_t)
    cppcmb_return(memo_d(cppcmb_fwd(parser)))

// Indirect-left-recursive packrat
template <typename P, cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
constexpr auto operator%=(P&& parser, as_memo_i_t)
    cppcmb_return(memo_i(cppcmb_fwd(parser)))

/**
 * Some helper functionalities for the action combinator.
 * Not strictly parsing, but helpful utilities, like dropping elements.
 */

/**
 * Only accepts the input, if it satisfies a given predicate.
 */
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

/**
 * Selects some elements from a product.
 */
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

/**
 * An abstract type to wrap everything into a simple interface.
 */
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

/**
 * Detail macro undefines.
 */
#undef cppcmb_prelude_self_check
#undef cppcmb_prelude_requires_t1
// Sadly we have to leak these...
// #undef cppcmb_prelude_cat

/**
 * Library macro undefines.
 */
#undef cppcmb_is_specialization
#undef cppcmb_self_check
#undef cppcmb_getter
#undef cppcmb_unreachable
#undef cppcmb_assert
#undef cppcmb_return
#undef cppcmb_requires_t
#undef cppcmb_requires
#undef cppcmb_assert_concept
#undef cppcmb_fwd
// Sadly we have to leak these...
//#undef cppcmb_unique_id
//#undef cppcmb_cat

#endif /* CPPCMB_HPP */
