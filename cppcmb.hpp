/**
 * cppcmb.hpp
 *
 * @author Peter Lenkefi
 * @date 2018-09-05
 * @description Generic parser combinators for C++17.
 */

#ifndef CPPCMB_HPP
#define CPPCMB_HPP

#include <cassert>
#include <cstddef>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>

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
cppcmb_prelude_requires_t1(cppcmb_unique_id(concept_req), __VA_ARGS__)

#define cppcmb_return(...) \
noexcept(noexcept(__VA_ARGS__)) -> decltype(__VA_ARGS__) { return __VA_ARGS__; }

#define cppcmb_assert(msg, ...) \
assert(((void)msg, (__VA_ARGS__)))

/**
 * Macro details.
 */
#define cppcmb_prelude_cat(x, y) x ## y

#define cppcmb_prelude_requires_t1(id, ...) 						\
bool id = false,                                                    \
::std::enable_if_t<id || (__VA_ARGS__), ::std::nullptr_t> = nullptr

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

    public:
        using value_type =
            detail::remove_cvref_t<decltype((*m_Source)[m_Cursor])>;

        constexpr reader(Src const& src) noexcept
            : m_Source(std::addressof(src)), m_Cursor(0U) {
        }

        constexpr reader(Src const& src, std::size_t idx) noexcept
            : reader(src) {
            seek(idx);
        }

        reader(Src const&& src) = delete;

        [[nodiscard]] constexpr auto source() const
            cppcmb_return(*m_Source);

        [[nodiscard]] constexpr auto cursor() const
            cppcmb_return(m_Cursor);

        [[nodiscard]] constexpr auto is_end() const
            cppcmb_return(m_Cursor >= std::size(*m_Source));

        [[nodiscard]] constexpr auto current() const
            cppcmb_return((*m_Source)[m_Cursor]);

        constexpr void seek(std::size_t idx) noexcept {
            cppcmb_assert(
                "seek() argument must be in the bounds of source!",
                idx < std::size(source())
            );
            m_Cursor = idx;
        }

        constexpr void next() noexcept {
            cppcmb_assert(
                "next() must not be called when is_end() is true!",
                !is_end()
            );
            seek(cursor() + 1);
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
        std::size_t m_Remaining;

    public:
        template <typename TFwd>
        constexpr success(TFwd&& val, std::size_t rem = 0)
            noexcept(noexcept(cppcmb_fwd(val)))
            : m_Value(cppcmb_fwd(val)), m_Remaining(rem) {
        }

        [[nodiscard]] constexpr auto value() &
            cppcmb_return(m_Value);
        [[nodiscard]] constexpr auto value() const&
            cppcmb_return(m_Value);
        [[nodiscard]] constexpr auto value() &&
            cppcmb_return(std::move(m_Value));
        [[nodiscard]] constexpr auto value() const&&
            cppcmb_return(std::move(m_Value));

        [[nodiscard]] constexpr auto remaining() const
            cppcmb_return(m_Remaining);
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

        [[nodiscard]] constexpr auto furthest() const
            cppcmb_return(m_Furthest);
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
        using success_type = success<T>;
        using failure_type = failure;

    private:
        template <typename U>
        static constexpr bool is_self_v =
            std::is_same_v<detail::remove_cvref_t<U>, result>;

        std::variant<success_type, failure_type> m_Data;

    public:
        template <typename TFwd, cppcmb_requires_t(!is_self_v<TFwd>)>
        constexpr result(TFwd&& val) noexcept(noexcept(cppcmb_fwd(val)))
            : m_Data(cppcmb_fwd(val)) {
        }

        [[nodiscard]] constexpr auto is_success() const
            cppcmb_return(std::holds_alternative<success_type>(m_Data));

        [[nodiscard]] constexpr auto is_failure() const
            cppcmb_return(std::holds_alternative<failure_type>(m_Data));

        [[nodiscard]] constexpr auto success() &
            cppcmb_return(std::get<success_type>(m_Data));
        [[nodiscard]] constexpr auto success() const&
            cppcmb_return(std::get<success_type>(m_Data));
        [[nodiscard]] constexpr auto success() &&
            cppcmb_return(std::get<success_type>(std::move(m_Data)));
        [[nodiscard]] constexpr auto success() const&&
            cppcmb_return(std::get<success_type>(std::move(m_Data)));

        [[nodiscard]] constexpr auto failure() &
            cppcmb_return(std::get<failure_type>(m_Data));
        [[nodiscard]] constexpr auto failure() const&
            cppcmb_return(std::get<failure_type>(m_Data));
        [[nodiscard]] constexpr auto failure() &&
            cppcmb_return(std::get<failure_type>(std::move(m_Data)));
        [[nodiscard]] constexpr auto failure() const&&
            cppcmb_return(std::get<failure_type>(std::move(m_Data)));
    };

    namespace detail {

        class pack_base {};

        template <typename T>
        using is_pack = std::is_base_of<pack_base, T>;

        template <typename T>
        inline constexpr bool is_pack_v = is_pack<T>::value;

    } /* namespace detail */

    /**
     * A tuple-like object that can stores a sequence of results.
     */
    template <typename... Ts>
    class pack : private detail::pack_base {
    private:
        using tuple_type = decltype(std::make_tuple(std::declval<Ts>()...));

        tuple_type m_Value;

        template <typename U>
        static constexpr bool is_self_v =
            std::is_same_v<detail::remove_cvref_t<U>, pack>;

    public:
        static constexpr auto index_sequence =
            std::make_index_sequence<sizeof...(Ts)>();

        template <typename T, cppcmb_requires_t(!is_self_v<T>)>
        constexpr pack(T&& val)
            : m_Value(std::make_tuple(cppcmb_fwd(val))) {
        }

        template <typename... Us, cppcmb_requires_t(sizeof...(Us) != 1)>
        constexpr pack(Us&&... vals)
            : m_Value(std::make_tuple(cppcmb_fwd(vals)...)) {
        }

        template <std::size_t Idx>
        [[nodiscard]] constexpr auto get() &
            cppcmb_return(std::get<Idx>(m_Value));
        template <std::size_t Idx>
        [[nodiscard]] constexpr auto get() const&
            cppcmb_return(std::get<Idx>(m_Value));
        template <std::size_t Idx>
        [[nodiscard]] constexpr auto get() &&
            cppcmb_return(std::get<Idx>(std::move(m_Value)));
        template <std::size_t Idx>
        [[nodiscard]] constexpr auto get() const&&
            cppcmb_return(std::get<Idx>(std::move(m_Value)));

        [[nodiscard]] constexpr auto as_tuple() &
            cppcmb_return(m_Value);
        [[nodiscard]] constexpr auto as_tuple() const&
            cppcmb_return(m_Value);
        [[nodiscard]] constexpr auto as_tuple() &&
            cppcmb_return(std::move(m_Value));
        [[nodiscard]] constexpr auto as_tuple() const&&
            cppcmb_return(std::move(m_Value));
    };

    template <typename... Ts>
    pack(Ts&&...) -> pack<Ts&&...>;

    namespace detail {

        // Base-case
        template <typename... Ts>
        [[nodiscard]] constexpr auto cat_values_impl(pack<Ts...>&& res)
            cppcmb_return(std::move(res));

        template <typename T>
        [[nodiscard]] constexpr auto cat_values_impl(pack<T>&& res)
            cppcmb_return(std::move(res).template get<0>());

        // XXX(LPeter1997): Exception specifier
        // Recursive-case signature
        template <typename... Ts, typename Head, typename... Tail>
        [[nodiscard]]
        constexpr auto
        cat_values_impl(pack<Ts...>&&, Head&&, Tail&&...);

        // XXX(LPeter1997): Exception specifier
        template <std::size_t... Is,
            typename... Ts, typename Head, typename... Tail>
        [[nodiscard]]
        constexpr auto cat_values_head_disp(
            std::index_sequence<Is...>,
            pack<Ts...>&& res, Head&& h, Tail&&... t) {

            return cat_values_impl(
                std::move(res),
                cppcmb_fwd(h).template get<Is>()...,
                cppcmb_fwd(t)...
            );
        }

        // XXX(LPeter1997): Exception specifier
        // Head is a value pack
        template <typename... Ts, typename Head, typename... Tail>
        [[nodiscard]]
        constexpr auto cat_values_impl_single(
            std::true_type,
            pack<Ts...>&& res, Head&& h, Tail&&... t) {

            using head_t = detail::remove_cvref_t<Head>;
            return cat_values_head_disp(
                head_t::index_sequence,
                std::move(res),
                cppcmb_fwd(h),
                cppcmb_fwd(t)...
            );
        }

        // XXX(LPeter1997): Exception specifier
        template <std::size_t... Is,
            typename... Ts, typename Head, typename... Tail>
        [[nodiscard]]
        constexpr auto cat_values_head_append(
            std::index_sequence<Is...>,
            pack<Ts...>&& res, Head&& h, Tail&&... t) {

            return cat_values_impl(
                pack(
                    std::move(res).template get<Is>()...,
                    cppcmb_fwd(h)
                ),
                cppcmb_fwd(t)...
            );
        }

        // XXX(LPeter1997): Exception specifier
        // Head is not a value pack
        template <typename... Ts, typename Head, typename... Tail>
        [[nodiscard]]
        constexpr auto cat_values_impl_single(
            std::false_type,
            pack<Ts...>&& res, Head&& h, Tail&&... t) {

            return cat_values_head_append(
                pack<Ts...>::index_sequence,
                std::move(res),
                cppcmb_fwd(h),
                cppcmb_fwd(t)...
            );
        }

        // XXX(LPeter1997): Exception specifier
        // Recursive-case
        template <typename... Ts, typename Head, typename... Tail>
        [[nodiscard]]
        constexpr auto
        cat_values_impl(pack<Ts...>&& res, Head&& h, Tail&&... t) {
            using tag_type =
                detail::is_pack<detail::remove_cvref_t<Head>>;

            return cat_values_impl_single(
                tag_type(),
                std::move(res),
                cppcmb_fwd(h),
                cppcmb_fwd(t)...
            );
        }

    } /* namespace detail */

    /**
     * Concatenate value packs and values.
     */
    template <typename... Ts>
    [[nodiscard]]
    constexpr auto cat_values(Ts&&... vs)
        cppcmb_return(
            // XXX(LPeter1997): Bug in GCC?
            detail::cat_values_impl(pack<>(), cppcmb_fwd(vs)...)
        );

    namespace detail {

        // Arg is a pack
        template <typename Fn, typename T>
        [[nodiscard]] constexpr auto apply_values_impl(
            std::true_type,
            Fn&& fn, T&& arg)
            cppcmb_return(std::apply(
                cppcmb_fwd(fn), cppcmb_fwd(arg).as_tuple()
            ));

        // Arg is not a pack
        template <typename Fn, typename T>
        [[nodiscard]] constexpr auto apply_values_impl(
            std::false_type,
            Fn&& fn, T&& arg)
            cppcmb_return(cppcmb_fwd(fn)(cppcmb_fwd(arg)));

    } /* namespace detail */

    template <typename Fn, typename T>
    [[nodiscard]] constexpr auto apply_values(Fn&& fn, T&& arg)
        cppcmb_return(
            detail::apply_values_impl(
                detail::is_pack<detail::remove_cvref_t<T>>(),
                cppcmb_fwd(fn),
                cppcmb_fwd(arg)
            )
        );

    namespace detail {

        /**
         * A tag-type for every combinator.
         * Also provides an ID that is unique for every new combinator but the
         * same for copies. Good for cache-ing results, like the packrat parser.
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
     * Helper to get the result value of a parse success.
     */
    template <typename P, typename Src>
    using parser_value_t = detail::remove_cvref_t<decltype(
        std::declval<P>().apply(std::declval<reader<Src>&>())
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
        template <typename PFwd, typename FnFwd>
        constexpr action_t(PFwd&& cmb, FnFwd&& fn)
            noexcept(noexcept(cppcmb_fwd(cmb)) && noexcept(cppcmb_fwd(fn)))
            : m_Parser(cppcmb_fwd(cmb)), m_Fn(cppcmb_fwd(fn)) {
        }

        // XXX(LPeter1997): Noexcept specifier?
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
        // XXX(LPeter1997): Noexcept specifier?
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

        // XXX(LPeter1997): Noexcept specifier?
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
        [[nodiscard]] constexpr auto apply_fn(T&& val) const
            cppcmb_return(apply_values(m_Fn, cppcmb_fwd(val)));
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
            -> result<pack<>> {

            if (r.is_end()) {
                // XXX(LPeter1997): GCC bug?
                return success(pack<>(), r.cursor());
            }
            else {
                return failure(r.cursor());
            }
        }
    };

    // Value for 'end' parser
    inline constexpr end_t end = end_t();

} /* namespace cppcmb */

/**
 * Detail macro undefines.
 */
#undef cppcmb_prelude_requires_t1
#undef cppcmb_prelude_cat

/**
 * Library macro undefines.
 */
#undef cppcmb_assert
#undef cppcmb_return
#undef cppcmb_requires_t
#undef cppcmb_requires
#undef cppcmb_assert_concept
#undef cppcmb_fwd
#undef cppcmb_unique_id
#undef cppcmb_cat

#endif /* CPPCMB_HPP */
