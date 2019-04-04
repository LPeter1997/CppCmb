/**
 * cppcmb.hpp
 *
 * @author Peter Lenkefi
 * @date 2018-09-05
 * @description Generic parser combinators for C++17.
 */

#ifndef CPPCMB_HPP
#define CPPCMB_HPP

#include <any>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <tuple>
#include <type_traits>
#include <unordered_map>
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

#define cppcmb_noexcept(...) \
noexcept(noexcept(__VA_ARGS__))

#define cppcmb_getter(name, ...)                                     \
[[nodiscard]]                                                        \
constexpr auto& name() & cppcmb_noexcept(__VA_ARGS__) {              \
    return __VA_ARGS__;                                              \
}                                                                    \
[[nodiscard]]                                                        \
constexpr auto const& name() const& cppcmb_noexcept(__VA_ARGS__) {   \
    return __VA_ARGS__;                                              \
}                                                                    \
[[nodiscard]]                                                        \
constexpr auto&& name() && cppcmb_noexcept(__VA_ARGS__) {            \
    return std::move(__VA_ARGS__);                                   \
}                                                                    \
[[nodiscard]]                                                        \
constexpr auto const&& name() const&& cppcmb_noexcept(__VA_ARGS__) { \
    return std::move(__VA_ARGS__);                                   \
}

#define cppcmb_self_check(type) \
cppcmb_prelude_self_check(type, cppcmb_unique_id(cppcmb_self_type))

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
        using is_detected = typename
            detector<nonesuch, void, Op, Args...>::value_t;

        template <template <typename...> typename Op, typename... Args>
        using detected_t = typename
            detector<nonesuch, void, Op, Args...>::type;

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
        using is_detected_exact =
            std::is_same<Expected, detected_t<Op, Args...>>;

        template <typename Expected,
            template <typename...> typename Op, typename... Args>
        inline constexpr auto is_detected_exact_v =
            is_detected_exact<Expected, Op, Args...>::value;

        template <typename To, template <typename...> typename Op, typename... Args>
        using is_detected_convertible =
            std::is_convertible<detected_t<Op, Args...>, To>;

        template <typename To, template <typename...> typename Op,
            typename... Args>
        inline constexpr auto is_detected_convertible_v =
            is_detected_convertible<To, Op, Args...>::value;

        /**
         * Reader concept checking.
         */
        template <typename T>
        using element_at_t =
            decltype(std::declval<T>()[std::declval<std::size_t>()]);

        template <typename T>
        using msize_t = decltype(std::size(std::declval<T>()));

        // Readable source concept for the reader
        template <typename T>
        inline constexpr bool is_reader_source_v = std::conjunction_v<
            is_detected<element_at_t, T>,
            is_detected<msize_t, T>
        >;

        /**
         * @see https://en.cppreference.com/w/cpp/types/remove_cvref
         */
        template <typename T>
        struct remove_cvref : std::remove_cv<std::remove_reference_t<T>> {};

        template <typename T>
        using remove_cvref_t = typename remove_cvref<T>::type;

    } /* namespace detail */

    class memo_table;

    /**
     * The structure that reads from the source element-wise.
     */
    template <typename Src>
    class reader {
    public:
        static_assert(
            detail::is_reader_source_v<Src>,
            "The reader source must have a subscript operator [std::size_t] "
            "and a size() member function!"
        );

    private:
        Src const*  m_Source;
        std::size_t m_Cursor;
        memo_table* m_MemoTable;

    public:
        using value_type =
            detail::remove_cvref_t<decltype((*m_Source)[m_Cursor])>;

        constexpr reader(
            Src const& src, std::size_t idx, memo_table* t) noexcept
            : m_Source(::std::addressof(src)), m_Cursor(0U), m_MemoTable(t) {
            seek(idx);
        }

        constexpr reader(
            Src const& src, std::size_t idx, memo_table& t) noexcept
            : reader(src, idx, &t) {
        }

        constexpr reader(
            Src const& src, std::size_t idx = 0U) noexcept
            : reader(src, idx, nullptr) {
        }

        constexpr reader(
            Src const& src, memo_table& t) noexcept
            : reader(src, 0U, t) {
        }

        // Just to avoid nasty bugs
        reader(Src const&& src, std::size_t idx, memo_table* t) = delete;

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

        [[nodiscard]] constexpr auto* memo_ptr() const noexcept {
            return m_MemoTable;
        }

        [[nodiscard]] constexpr auto& memo() const noexcept {
            cppcmb_assert(
                "A memo-table must be assigned before accessing it!",
                m_MemoTable != nullptr
            );
            return *memo_ptr();
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
        cppcmb_self_check(success);

        value_type  m_Value;
        std::size_t m_Remaining;

    public:
        template <typename TFwd,
            cppcmb_requires_t(!is_self_v<TFwd>)>
        constexpr success(TFwd&& val, std::size_t rem = 0)
            noexcept(std::is_nothrow_constructible_v<value_type, TFwd&&>)
            : m_Value(cppcmb_fwd(val)), m_Remaining(rem) {
        }

        cppcmb_getter(value, m_Value)

        [[nodiscard]] constexpr auto const& remaining() const noexcept {
            return m_Remaining;
        }
    };

    template <typename TFwd>
    success(TFwd&&, std::size_t) -> success<detail::remove_cvref_t<TFwd>>;

    template <typename TFwd>
    success(TFwd&&) -> success<detail::remove_cvref_t<TFwd>>;

    /**
     * Failure "type-constructor". The type that the parser returns when it
     * fails.
     */
    class failure {
    private:
        std::size_t m_Furthest;

    public:
        constexpr failure(std::size_t furth = 0) noexcept
            : m_Furthest(furth) {
        }

        [[nodiscard]] constexpr auto const& furthest() const noexcept {
            return m_Furthest;
        }
    };

    namespace detail {

        struct result_base {};

        template <typename T>
        using is_result = std::is_base_of<result_base, T>;

        template <typename T>
        inline constexpr bool is_result_v = is_result<T>::value;

    } /* namespace detail */

    /**
     * The result type of a parser. It's either a success or a failure type.
     */
    template <typename T>
    class result : private detail::result_base {
    public:
        using success_type = ::cppcmb::success<T>;
        using failure_type = ::cppcmb::failure;

    private:
        cppcmb_self_check(result);

        using either_type = std::variant<success_type, failure_type>;

        either_type m_Data;

    public:
        template <typename TFwd,
            cppcmb_requires_t(!is_self_v<TFwd>)>
        constexpr result(TFwd&& val)
            noexcept(std::is_nothrow_constructible_v<either_type, TFwd&&>)
            : m_Data(cppcmb_fwd(val)) {
        }

        [[nodiscard]] constexpr bool is_success() const noexcept {
            return std::holds_alternative<success_type>(m_Data);
        }

        [[nodiscard]] constexpr bool is_failure() const noexcept {
            return std::holds_alternative<failure_type>(m_Data);
        }

        cppcmb_getter(success, std::get<success_type>(m_Data))
        cppcmb_getter(failure, std::get<failure_type>(m_Data))
    };

    namespace detail {

        class product_base {};

        template <typename T>
        using is_product = std::is_base_of<product_base, T>;

        template <typename T>
        inline constexpr bool is_product_v = is_product<T>::value;

    } /* namespace detail */

    /**
     * A tuple-like object that can stores a sequence of results.
     */
    template <typename... Ts>
    class product : private detail::product_base {
    private:
        using tuple_type = decltype(std::make_tuple(std::declval<Ts>()...));

        cppcmb_self_check(product);

        tuple_type m_Value;

    public:
        static constexpr auto index_sequence =
            std::make_index_sequence<sizeof...(Ts)>();

        template <typename T,
            cppcmb_requires_t(!is_self_v<T>)>
        constexpr product(T&& val)
            noexcept(std::is_nothrow_constructible_v<tuple_type, T&&>)
            : m_Value(cppcmb_fwd(val)) {
        }

        template <typename... Us,
            cppcmb_requires_t(sizeof...(Us) != 1)>
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
    product(Ts&&...) -> product<detail::remove_cvref_t<Ts>...>;

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
        template <std::size_t... Is,
            typename... Ts, typename Head, typename... Tail>
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
        template <std::size_t... Is,
            typename... Ts, typename Head, typename... Tail>
        [[nodiscard]] constexpr auto product_values_append(
            std::index_sequence<Is...>,
            product<Ts...>&& res, Head&& h, Tail&&... t) {
            return product_values_impl(
                // XXX(LPeter1997): GCC bug?
                product<Ts..., remove_cvref_t<Head>>(
                    std::move(res).template get<Is>()..., cppcmb_fwd(h)
                ),
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
        // XXX(LPeter1997): Bug in GCC?
        return detail::product_values_impl(product<>(), cppcmb_fwd(vs)...);
    }

    namespace detail {

        class sum_base {};

        template <typename T>
        using is_sum = std::is_base_of<sum_base, T>;

        template <typename T>
        inline constexpr bool is_sum_v = is_sum<T>::value;

    } /* namespace detail */

    template <typename... Ts>
    class sum : private detail::sum_base {
    private:
        using variant_type = std::variant<Ts...>;

        cppcmb_self_check(sum);

        variant_type m_Value;

    public:
        // XXX(LPeter1997): Exception specifier?
        template <typename T,
            cppcmb_requires_t(!is_self_v<T>)>
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
            using type =
                typename sum_values_t_impl<sum<Ts...>, Us..., Vs...>::type;
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
        struct combinator_base {};

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
        std::declval<parser_result_t<P, Src>>()
            .success().value()
    )>;

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
        [[nodiscard]]
        constexpr decltype(auto) apply(reader<Src> const& src) const {
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

            using fn_result_t =
                std::invoke_result_t<apply_t, action_t, value_t>;
            using dispatch_tag =
                detail::is_result<detail::remove_cvref_t<fn_result_t>>;

            return apply_impl<fn_result_t>(src, dispatch_tag());
        }

    private:
        // XXX(LPeter1997): Noexcept specifier
        // Action can fail
        template <typename FRes, typename Src>
        [[nodiscard]] constexpr auto apply_impl(
            reader<Src> const& src,
            std::true_type) const -> FRes {

            auto inv = m_Parser.apply(src);
            if (inv.is_failure()) {
                // Early failure
                return std::move(inv).failure();
            }

            auto succ = std::move(inv).success();
            // Try to apply the action
            auto act_inv = apply_fn(std::move(succ).value());
            // In any case we will have to decorate the result with the position
            if (act_inv.is_failure()) {
                return failure(src.cursor());
            }
            else {
                return success(
                    std::move(act_inv).success().value(),
                    succ.remaining()
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
                return std::move(inv).failure();
            }

            auto succ = std::move(inv).success();
            // Apply the action
            auto act_val = apply_fn(std::move(succ).value());
            // Wrap it in a success
            return success(std::move(act_val), succ.remaining());
        }

        // Invoke the function with a value
        template <typename T>
        [[nodiscard]] constexpr decltype(auto) apply_fn(T&& val) const {
            return apply_value(m_Fn, cppcmb_fwd(val));
        }
    };

    template <typename PFwd, typename FnFwd>
    action_t(PFwd&&, FnFwd&&) -> action_t<
        detail::remove_cvref_t<PFwd>,
        std::decay_t<FnFwd>
    >;

    /**
     * A parser that simply consumes a single element. Succeeds if there is an
     * element to consume.
     */
    class one_t : public combinator<one_t> {
    public:
        // XXX(LPeter1997): Noexcept specifier
        template <typename Src>
        [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
            -> result<typename reader<Src>::value_type> {

            if (r.is_end()) {
                // Nothing to consume
                return failure(r.cursor());
            }
            else {
                return success(r.current(), r.cursor() + 1);
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
                // XXX(LPeter1997): GCC bug?
                return success(product<>(), r.cursor());
            }
            else {
                return failure(r.cursor());
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

            auto p1_inv = m_First.apply(r);
            if (p1_inv.is_failure()) {
                // Early failure, don't continue
                return std::move(p1_inv).failure();
            }
            // Get the success alternative
            auto p1_succ = std::move(p1_inv).success();
            // Create the next reader
            auto r2 = reader(r.source(), p1_succ.remaining(), r.memo_ptr());
            // Invoke the second parser
            auto p2_inv = m_Second.apply(r2);
            if (p2_inv.is_failure()) {
                // Second failed, fail on that error
                return std::move(p2_inv).failure();
            }
            // Get the success alternative
            auto p2_succ = std::move(p2_inv).success();
            // Combine the values
            return success(
                product_values(
                    std::move(p1_succ).value(),
                    std::move(p2_succ).value()
                ),
                p2_succ.remaining()
            );
        }
    };

    template <typename P1Fwd, typename P2Fwd>
    seq_t(P1Fwd&&, P2Fwd&&) -> seq_t<
        detail::remove_cvref_t<P1Fwd>,
        detail::remove_cvref_t<P2Fwd>
    >;

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
        // XXX(LPeter1997): Noexcept specifier
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

            // Try to apply the first alternative
            auto p1_inv = m_First.apply(r);
            if (p1_inv.is_success()) {
                auto p1_succ = std::move(p1_inv).success();
                return success(
                    sum_values<value_t<Src>>(std::move(p1_succ).value()),
                    p1_succ.remaining()
                );
            }

            // Try to apply the second alternative
            auto p2_inv = m_Second.apply(r);
            if (p2_inv.is_success()) {
                auto p2_succ = std::move(p2_inv).success();
                return success(
                    sum_values<value_t<Src>>(std::move(p2_succ).value()),
                    p2_succ.remaining()
                );
            }

            // Both failed, return the error which got further
            auto p1_err = std::move(p1_inv).failure();
            auto p2_err = std::move(p2_inv).failure();

            if (p1_err.furthest() > p2_err.furthest()) {
                return p1_err;
            }
            else if (p1_err.furthest() < p2_err.furthest()) {
                return p2_err;
            }
            else {
                // They got to the same distance, need to merge errors
                // XXX(LPeter1997): Implement, for now we just return the first
                return p1_err;
            }
        }
    };

    template <typename P1Fwd, typename P2Fwd>
    alt_t(P1Fwd&&, P2Fwd&&) -> alt_t<
        detail::remove_cvref_t<P1Fwd>,
        detail::remove_cvref_t<P2Fwd>
    >;

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

            // Try to apply both alternatives
            auto p1_inv = m_First.apply(r);
            auto p2_inv = m_Second.apply(r);

            // Both succeeded
            if (p1_inv.is_success() && p2_inv.is_success()) {
                // Return the one that got further
                // If they both got the same distance, return the first one
                auto p1_succ = std::move(p1_inv).success();
                auto p2_succ = std::move(p2_inv).success();

                if (p1_succ.remaining() >= p2_succ.remaining()) {
                    return success(
                        sum_values<value_t<Src>>(std::move(p1_succ).value()),
                        p1_succ.remaining()
                    );
                }
                else {
                    return success(
                        sum_values<value_t<Src>>(std::move(p2_succ).value()),
                        p2_succ.remaining()
                    );
                }
            }
            // LHS succeeded
            if (p1_inv.is_success()) {
                auto p1_succ = std::move(p1_inv).success();
                return success(
                    sum_values<value_t<Src>>(std::move(p1_succ).value()),
                    p1_succ.remaining()
                );
            }
            // RHS succeeded
            if (p2_inv.is_success()) {
                auto p2_succ = std::move(p2_inv).success();
                return success(
                    sum_values<value_t<Src>>(std::move(p2_succ).value()),
                    p2_succ.remaining()
                );
            }

            // Both failed, return the error which got further
            auto p1_err = std::move(p1_inv).failure();
            auto p2_err = std::move(p2_inv).failure();

            if (p1_err.furthest() > p2_err.furthest()) {
                return p1_err;
            }
            else if (p1_err.furthest() < p2_err.furthest()) {
                return p2_err;
            }
            else {
                // They got to the same distance, need to merge errors
                // XXX(LPeter1997): Implement, for now we just return the first
                return p1_err;
            }
        }
    };

    template <typename P1Fwd, typename P2Fwd>
    eager_alt_t(P1Fwd&&, P2Fwd&&) -> eager_alt_t<
        detail::remove_cvref_t<P1Fwd>,
        detail::remove_cvref_t<P2Fwd>
    >;

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
        template <typename PFwd,
            cppcmb_requires_t(!is_self_v<PFwd>)>
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

            auto coll = value_t<Src>();
            auto rr = r;
            while (true) {
                auto p_inv = m_Parser.apply(rr);
                if (p_inv.is_failure()) {
                    // Stop applying
                    break;
                }
                auto p_succ = std::move(p_inv).success();
                // Add to collection
                coll.push_back(std::move(p_succ).value());
                // Move reader
                rr.seek(p_succ.remaining());
            }
            return success(std::move(coll), rr.cursor());
        }
    };

    template <typename PFwd>
    many_t(PFwd&&) -> many_t<detail::remove_cvref_t<PFwd>>;

    /**
     * Operator for making many parser.
     */
    template <typename P,
        cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
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
        template <typename PFwd,
            cppcmb_requires_t(!is_self_v<PFwd>)>
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

            auto p_inv = m_Parser.apply(r);

            cppcmb_assert(
                "The underlying 'many' parser must always succeed!",
                p_inv.is_success()
            );

            auto p_succ = std::move(p_inv).success();
            if (p_succ.value().size() > 0) {
                // Succeed
                return p_succ;
            }
            else {
                // Fail
                return failure(r.cursor());
            }
        }
    };

    template <typename PFwd>
    many1_t(PFwd&&) -> many1_t<detail::remove_cvref_t<PFwd>>;

    /**
     * Operator for making many1 parser.
     */
    template <typename P,
        cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
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

        // XXX(LPeter1997): Change to own optional-like
        template <typename Src>
        using value_t = std::optional<parser_value_t<P, Src>>;

        P m_Parser;

    public:
        template <typename PFwd,
            cppcmb_requires_t(!is_self_v<PFwd>)>
        constexpr opt_t(PFwd&& p)
            noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
            : m_Parser(cppcmb_fwd(p)) {
        }

        // XXX(LPeter1997): Noexcept specifier
        template <typename Src>
        [[nodiscard]] constexpr auto apply(reader<Src> const& r) const
            -> result<value_t<Src>> {
            cppcmb_assert_parser(P, Src);

            auto p_inv = m_Parser.apply(r);
            if (p_inv.is_failure()) {
                return success(value_t<Src>(), r.cursor());
            }
            else {
                auto succ = std::move(p_inv).success();
                return success(
                    value_t<Src>(std::move(succ).value()),
                    succ.remaining()
                );
            }
        }
    };

    template <typename PFwd>
    opt_t(PFwd&&) -> opt_t<detail::remove_cvref_t<PFwd>>;

    /**
     * Operator for making optional parser.
     */
    template <typename P,
        cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
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

    } /* namespace detail */

    /**
     * Memorization table for packrat parsers.
     */
    class memo_table {
    private:
        // pair<parser identifier, position>
        using key_type = std::pair<std::uintptr_t, std::size_t>;
        using value_type = std::any;

        std::unordered_map<
            key_type,
            value_type,
            detail::pair_hasher
        > m_Cache;

    public:
        // XXX(LPeter1997): Noexcept specifier
        [[nodiscard]]
        /* constexpr */ std::any* get(std::uintptr_t pid, std::size_t pos) {
            auto it = m_Cache.find({ pid, pos });
            if (it == m_Cache.end()) {
                return nullptr;
            }
            return &it->second;
        }

        // XXX(LPeter1997): Noexcept specifier
        template <typename Src>
        [[nodiscard]]
        constexpr std::any* get(std::uintptr_t pid, reader<Src> const& r) {
            return get(pid, r.cursor());
        }

        // XXX(LPeter1997): Noexcept specifier
        template <typename TFwd>
        constexpr auto& put(std::uintptr_t pid, std::size_t pos, TFwd&& val) {
            using raw_type = detail::remove_cvref_t<TFwd>;
            auto id = std::pair(pid, pos);
            auto& a = (m_Cache[id] = cppcmb_fwd(val));
            return std::any_cast<raw_type&>(a);
        }

        // XXX(LPeter1997): Noexcept specifier
        template <typename Src, typename TFwd>
        constexpr auto&
        put(std::uintptr_t pid, reader<Src> const& r, TFwd&& val) {
            return put(pid, r.cursor(), cppcmb_fwd(val));
        }

        // XXX(LPeter1997): Noexcept specifier
        /* constexpr */ void clear() {
            m_Cache.clear();
        }
    };

    namespace detail {

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

            // XXX(LPeter1997): Noexcept specifier
            template <typename Src, typename TFwd>
            constexpr auto& put_memo(reader<Src> const& r, TFwd&& val) const {
                auto& table = r.memo();
                return table.put(original_id(), r, cppcmb_fwd(val));
            }

            // XXX(LPeter1997): Noexcept specifier
            template <typename Src>
            [[nodiscard]]
            constexpr std::any* get_memo(reader<Src> const& r) const {
                auto& table = r.memo();
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
        template <typename PFwd,
            cppcmb_requires_t(!is_self_v<PFwd>)>
        constexpr packrat_t(PFwd&& p)
            noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
            : m_Parser(cppcmb_fwd(p)) {
        }

        // XXX(LPeter1997): Noexcept specifier
        template <typename Src>
        [[nodiscard]]
        constexpr decltype(auto) apply(reader<Src> const& r) const {
            cppcmb_assert_parser(P, Src);

            using apply_t = parser_result_t<P, Src>;

            auto* entry = this->get_memo(r);
            if (entry == nullptr) {
                return this->put_memo(r, m_Parser.apply(r));
            }
            else {
                return std::any_cast<apply_t&>(*entry);
            }
        }
    };

    template <typename PFwd>
    packrat_t(PFwd&&) -> packrat_t<detail::remove_cvref_t<PFwd>>;

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
            if (tmp_res.is_success()) {
                auto& tmp_succ = tmp_res.success();
                if (old_succ.remaining() < tmp_succ.remaining()) {
                    // We successfully grew the seed
                    auto& new_old = this->put_memo(r, in_rec(tmp_res)).value();
                    return grow(r, new_old);
                }
                else {
                    return old_res;
                }
            }
            else {
                return this->put_memo(r, in_rec(old_res)).value();
            }
        }

    public:
        template <typename PFwd,
            cppcmb_requires_t(!is_self_v<PFwd>)>
        constexpr drec_packrat_t(PFwd&& p)
            noexcept(std::is_nothrow_constructible_v<P, PFwd&&>)
            : m_Parser(cppcmb_fwd(p)) {
        }

        // XXX(LPeter1997): Noexcept specifier
        template <typename Src>
        [[nodiscard]]
        constexpr decltype(auto) apply(reader<Src> const& r) const {
            cppcmb_assert_parser(P, Src);

            using apply_t = parser_result_t<P, Src>;
            using base_rec = std::pair<base_recursion<apply_t>, bool>;
            using in_rec = in_recursion<apply_t>;

            auto* entry = this->get_memo(r);
            if (entry == nullptr) {
                // Nothing is in the cache yet, write a dummy error
                this->put_memo(r, base_rec(failure(r.cursor()), true));
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
                    auto& res =
                        this->put_memo(r, in_rec(std::move(tmp_res))).value();
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
                        base_rec(std::move(tmp_res), false)
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
                            in_rec(failure(r.cursor()))
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
    drec_packrat_t(PFwd&&) -> drec_packrat_t<detail::remove_cvref_t<PFwd>>;

    /**
     * Wrapper to make any combinator a direct-left-recursive packrat parser.
     */
    template <typename PFwd>
    [[nodiscard]] constexpr auto memo_d(PFwd&& p)
        cppcmb_return(drec_packrat_t(cppcmb_fwd(p)))

    /**
     * Here we provide operator%= to turn a parser into packrat parsers.
     */

    // Tag-types for the parsers
    struct as_self_t {};
    struct as_memo_t {};
    struct as_memo_d_t {};

    inline constexpr auto as_self = as_self_t();
    inline constexpr auto as_memo = as_memo_t();
    inline constexpr auto as_memo_d = as_memo_d_t();

    // The operators

    // Identity
    template <typename P,
        cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
    constexpr auto operator%=(P&& parser, as_self_t)
        cppcmb_return(cppcmb_fwd(parser))

    // Simple packrat
    template <typename P,
        cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
    constexpr auto operator%=(P&& parser, as_memo_t)
        cppcmb_return(memo(cppcmb_fwd(parser)))

    // Direct-left-recursive packrat
    template <typename P,
        cppcmb_requires_t(detail::is_combinator_cvref_v<P>)>
    constexpr auto operator%=(P&& parser, as_memo_d_t)
        cppcmb_return(memo_d(cppcmb_fwd(parser)))

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
        template <typename PredFwd,
            cppcmb_requires_t(!is_self_v<PredFwd>)>
        constexpr filter(PredFwd&& pred)
            noexcept(std::is_nothrow_constructible_v<Pred, PredFwd&&>)
            : m_Predicate(cppcmb_fwd(pred)) {
        }

        // XXX(LPeter1997): Noexcept specifier
        template <typename... Ts>
        [[nodiscard]] constexpr auto operator()(Ts&&... args) const
            -> result<value_t<Ts&&...>> {
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
                return success(product_values(cppcmb_fwd(args)...));
            }
            else {
                return failure();
            }
        }
    };

    template <typename PredFwd>
    filter(PredFwd&&) -> filter<std::decay_t<PredFwd>>;

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
#undef cppcmb_self_check
#undef cppcmb_getter
#undef cppcmb_noexcept
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
