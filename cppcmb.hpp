/**
 * cppcmb.hpp
 *
 * @author Peter Lenkefi
 * @date 2018-09-05
 * @description Generic parser combinators for C++17.
 */

#ifndef CPPCMB_HPP
#define CPPCMB_HPP

#include <functional>
#include <initializer_list>
#include <iterator>
#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>

namespace cppcmb {

/**
 * Utilities and types used by the combinators.
 */
namespace detail {
	/**
	 * A custom optional wrapper to differentiate std::optional from nullables
	 * returned by a mapping failure.
	 */
	template <typename T>
	struct maybe : private std::optional<T> {
		constexpr maybe(maybe const&) = default;
		constexpr maybe(maybe&&) = default;

		maybe& operator=(maybe const&) = default;
		maybe& operator=(maybe&&) = default;

		maybe(std::optional<T> const&) = delete;
		maybe(std::optional<T>&&) = delete;

		std::optional<T>& operator=(std::optional<T> const&) = delete;
		std::optional<T>& operator=(std::optional<T>&&) = delete;

		using std::optional<T>::optional;
		using std::optional<T>::operator=;
		using std::optional<T>::operator->;
		using std::optional<T>::operator*;
		using std::optional<T>::operator bool;
		using std::optional<T>::has_value;
		using std::optional<T>::value;
		using std::optional<T>::value_or;
	};

	/**
	 * Helper functions to make the failable type. Wrapped in a struct so it can
	 * be inherited without duplication.
	 */
	struct maybe_ctors {
		template <typename T>
		static constexpr auto make_maybe(T&& val) {
			return maybe<std::decay_t<T>>(std::forward<T>(val));
		}

		template <typename T, typename... Args>
		static constexpr auto make_maybe(Args&&... args) {
			return maybe<T>(std::in_place, std::forward<Args>(args)...);
		}

		template <typename T, typename U, typename... Args>
		static constexpr auto
		make_maybe(std::initializer_list<U> il, Args&&... args) {
			return maybe<T>(std::in_place, il, std::forward<Args>(args)...);
		}
	};

	/**
	 * A helper functionality that wraps any value into a tuple if it's not
	 * already a tuple.
	 */
	template <typename T>
	struct as_tuple_impl {
		template <typename TFwd>
		static constexpr auto pass(TFwd&& arg) {
			return std::make_tuple(std::forward<TFwd>(arg));
		}
	};

	template <typename... Ts>
	struct as_tuple_impl<std::tuple<Ts...>> {
		template <typename TFwd>
		static constexpr auto pass(TFwd&& arg) {
			return arg;
		}
	};

	template <typename TFwd>
	constexpr auto as_tuple(TFwd&& arg) {
		return as_tuple_impl<std::decay_t<TFwd>>::pass(std::forward<TFwd>(arg));
	}

	/**
	 * A helper functionality that unwraps a tuple if it can.
	 */
	template <typename T>
	struct unwrap_tuple_impl {
		template <typename TFwd>
		static constexpr auto pass(TFwd&& arg) {
			return arg;
		}
	};

	template <typename T>
	struct unwrap_tuple_impl<std::tuple<T>> {
		template <typename TFwd>
		static constexpr auto pass(TFwd&& arg) {
			return std::get<0>(std::forward<TFwd>(arg));
		}
	};

	template <typename TFwd>
	constexpr auto unwrap_tuple(TFwd&& arg) {
		return unwrap_tuple_impl<std::decay_t<TFwd>>::pass(
			std::forward<TFwd>(arg)
		);
	}

	/**
	 * Identity function.
	 */
	template <typename T>
	constexpr auto identity(T&& arg) {
		return arg;
	}

	/**
	 * A function that constructs a tuple if there are multiple or 0 parameters,
	 * but returns the parameter itself if there is only one.
	 */
	template <typename... Ts>
	constexpr auto tuple_of(Ts&&... args) {
		if constexpr (sizeof...(args) == 1) {
			return identity(std::forward<Ts>(args)...);
		}
		else {
			return std::make_tuple(std::forward<Ts>(args)...);
		}
	}

	/**
	 * Concatenates the arguments into a tuple and unwraps the result if
	 * possible.
	 */
	template <typename... Ts>
	constexpr auto concat(Ts&&... args) {
		return unwrap_tuple(
			std::tuple_cat(as_tuple(std::forward<Ts>(args))...)
		);
	}

	/**
	 * Wraps a free function into a functor type so we can pass it around as a
	 * type. Every non-combinator function (like transformations) has to be
	 * wrapped in this.
	 */
	template <auto Callable>
	struct fn_wrap {
		constexpr fn_wrap() = default;

		template <typename... Ts>
		constexpr auto operator()(Ts&&... args) const {
			return Callable(std::forward<Ts>(args)...);
		}
	};

	/**
	 * We need to forward-declare functionality for the subscript operator.
	 */
	template <typename Combinator, typename Mapper>
	constexpr auto make_subscript_map(Combinator&&, Mapper&&);

	/**
	 * Wraps a free function to act as a combinator function. Every combinator
	 * must be wrapped in this.
	 */
	template <typename TokenIterator, auto Callable>
	struct cmb_wrap {
	private:
		using callable_type = std::decay_t<decltype(Callable)> ;
		static_assert(
			std::is_invocable_v<callable_type, TokenIterator>,
			"A combinator must be able to be invoked with the provided iterator"
			" type!"
		);
		using return_type = std::invoke_result_t<callable_type, TokenIterator>;
		using pair_type = typename return_type::value_type;

	public:
		using data_type = typename pair_type::first_type;
		using iterator_type = typename pair_type::second_type;

		static_assert(
			std::is_same_v<TokenIterator, iterator_type>,
			"The resulting iterator type must match the provided one!"
		);

		constexpr cmb_wrap() = default;

		constexpr auto operator()(TokenIterator it) const {
			return Callable(it);
		}

		template <typename Mapper>
		constexpr auto operator[](Mapper&& m) const {
			return make_subscript_map(*this, std::forward<Mapper>(m));
		}
	};

	/**
	 * Filters the result. If the predicate is true, it succeeds, fails
	 * otherwise.
	 */
	template <typename Predicate>
	struct filter_impl {
		constexpr filter_impl() = default;

		template <typename... Ts>
		constexpr auto operator()(Ts&&... args) const {
			using res_type = decltype(maybe_ctors::make_maybe(
				tuple_of(std::forward<Ts>(args)...)
			));
			if (Predicate()(args...)) {
				return maybe_ctors::make_maybe(
					tuple_of(std::forward<Ts>(args)...)
				);
			}
			return res_type();
		}
	};

	/**
	 * Selector function that returns some elements of the tuple.
	 */
	template <std::size_t... Indicies>
	struct select_impl {
		constexpr select_impl() = default;

		template <typename... Ts>
		constexpr auto operator()(Ts&&... args) const {
			return tuple_of(std::get<Indicies>(
				std::make_tuple(std::forward<Ts>(args)...)
			)...);
		}
	};

	/**
	 * Fold left function.
	 * f(f(f(f(f(..., f), g), h), i), j)
	 */
	template <typename Folder>
	struct foldl_impl {
		constexpr foldl_impl() = default;

		template <typename Init, typename Rest>
		constexpr auto operator()(Init&& first, Rest&& rest) const {
			for (auto it = std::cbegin(rest); it != std::cend(rest); ++it) {
				first = Folder()(std::move(first), *it);
			}
			return first;
		}
	};

	/**
	 * Fold right function.
	 * f(a, f(b, f(c, f(d, f(e, ...)))))
	 */
	template <typename Folder>
	struct foldr_impl {
		constexpr foldr_impl() = default;

		template <typename Init, typename Rest>
		constexpr auto operator()(Init&& rest, Rest&& first) const {
			for (auto it = std::crbegin(rest); it != std::crend(rest); ++it) {
				first = Folder()(*it, std::move(first));
			}
			return first;
		}
	};

	/**
	 * Mechanism for matching maybes. Mapping combinator needs it.
	 */
	template <typename T>
	struct is_maybe : std::false_type {};

	template <typename T>
	struct is_maybe<maybe<T>> : std::true_type {};

	template <typename T>
	inline constexpr bool is_maybe_v = is_maybe<T>::value;

	/**
	 * Mechanism for matching combinators. This is the reason we need all
	 * combinators as cmb_wrap-s.
	 */
	template <typename T>
	struct is_cmb : std::false_type {};

	template <typename TokenIterator, auto Callable>
	struct is_cmb<cmb_wrap<TokenIterator, Callable>> : std::true_type {};

	template <typename T>
	inline constexpr bool is_cmb_v = is_cmb<T>::value;

	/**
	 * SFINAE test for combinators.
	 */
	template <typename T>
	struct enable_if_cmb : public std::enable_if<
		is_cmb_v<std::decay_t<T>>
	> {};

	template <typename T>
	using enable_if_cmb_t = typename enable_if_cmb<T>::type;

	/**
	 * Check if a type is a function pointer.
	 */
	template <typename T>
	struct is_function_ptr : std::bool_constant<
		std::is_pointer_v<T> && std::is_function_v<std::remove_pointer_t<T>>
	> { };

	template <typename T>
	inline constexpr bool is_function_ptr_v = is_function_ptr<T>::value;
} /* namespace detail */

/**
 * A module interface for a template-style grammar definition.
 */
template <typename TokenIterator>
struct combinator_types : private detail::maybe_ctors {
public:
	/**
	 * General result type.
	 */
	template <typename... Data>
	using result_type = std::optional<std::pair<
		decltype(detail::tuple_of(std::declval<Data>()...)),
		TokenIterator
	>>;

	/**
	 * Creates a result value.
	 * Result values are in the form of ((data...), position)?.
	 */
	template <typename Data>
	static constexpr auto make_result(Data&& data, TokenIterator it) {
		return std::make_optional(std::make_pair(
			std::forward<Data>(data), it
		));
	}

private:
	/**
	 * The simplest combinator that succeeds with an empty result.
	 */
	static constexpr auto cmb_succ_fn(TokenIterator it) {
		return make_result(std::make_tuple(), it);
	}

	/**
	 * A combinator that always fails with a given data type.
	 */
	template <typename T>
	static constexpr auto cmb_fail_fn(TokenIterator) {
		return result_type<T>();
	}

	/**
	 * A combinator that returns the current token and advances the position by
	 * one.
	 */
	static constexpr auto cmb_one_fn(TokenIterator it) {
		return make_result(*it, std::next(it));
	}

	/**
	 * Wraps another combinator so that it's return data becomes optional. This
	 * combinator therefore always succeeds.
	 */
	template <typename Combinator>
	static constexpr auto cmb_opt_fn(TokenIterator it) {
		static_assert(
			detail::is_cmb_v<Combinator>,
			"Optional combinator requires a combinator wrapper as argument!"
		);

		if (auto res = Combinator()(it)) {
			return make_result(std::make_optional(res->first), res->second);
		}
		using data_type = typename Combinator::data_type;
		return make_result(std::optional<data_type>(), it);
	}

	/**
	 * Applies combinators in a sequence and concatenates the results if all of
	 * them succeeds. If one fails, the whole sequence fails.
	 */
	template <typename First, typename... Rest>
	static constexpr auto cmb_seq_fn(TokenIterator it) {
		static_assert(
			detail::is_cmb_v<First>,
			"Sequencing combinator requires a combinator wrapper as argument!"
		);

		if constexpr (sizeof...(Rest) == 0) {
			// Only one entry, return that
			return First()(it);
		}
		else {
			auto first = First()(it);
			using result_type = decltype(make_result(
				detail::concat(first->first, cmb_seq_fn<Rest...>(it)->first), it
			));
			if (!first) {
				return result_type();
			}
			auto rest = cmb_seq_fn<Rest...>(first->second);
			if (!rest) {
				return result_type();
			}
			return make_result(
				detail::concat(std::move(first->first), std::move(rest->first)),
				rest->second
			);
		}
	}

	/**
	 * Applies the combinators and returns with the first succeeding one. If
	 * none of them succeeds, the combinator fails.
	 */
	template <typename ResultData, typename First, typename... Rest>
	static constexpr auto cmb_alt_fn(TokenIterator it) {
		static_assert(
			detail::is_cmb_v<First>,
			"Alternative combinator requires a combinator wrapper as argument!"
		);

		if constexpr (sizeof...(Rest) == 0) {
			// Just this one entry is left
			return First()(it);
		}
		else {
			if (auto first = First()(it)) {
				return first;
			}
			return cmb_alt_fn<ResultData, Rest...>(it);
		}
	}

	/**
	 * Repeatedly applies a combinator while it succeeds. Stops on faliure.
	 * Collects result into a collection. Always succeeds.
	 */
	template <template <typename...> typename Collection, typename Combinator>
	static constexpr auto cmb_rep_fn(TokenIterator it) {
		static_assert(
			detail::is_cmb_v<Combinator>,
			"Repeat combinator requires a combinator wrapper as argument!"
		);

		using element_type = typename Combinator::data_type;
		Collection<element_type> result;
		auto out_it = std::back_inserter(result);
		while (true) {
			auto res = Combinator()(it);
			if (!res) {
				return make_result(std::move(result), it);
			}
			// Advance
			*out_it++ = std::move(res->first);
			it = res->second;
		}
	}

	/**
	 * Repeatedly applies a combinator while it succeeds. Stops on faliure.
	 * Collects result into a collection. Succeeds if it collected at least one
	 * element.
	 */
	template <template <typename...> typename Collection, typename Combinator>
	static constexpr auto cmb_rep1_fn(TokenIterator it) {
		auto res = cmb_rep_fn<Collection, Combinator>(it);
		using res_type = decltype(res);
		if (res->first.empty()) {
			// Empty, fail
			return res_type();
		}
		return res;
	}

	/**
	 * Applies a combinator. If it succeeded, the result data is applied to a
	 * transformation function. If the transformation can fail, then it's also
	 * considered when returning (on transformation failure the combinator
	 * fails).
	 */
	template <typename Combinator, typename Mapper>
	static constexpr auto cmb_map_fn(TokenIterator it) {
		static_assert(
			detail::is_cmb_v<Combinator>,
			"Map combinator requires a combinator wrapper as argument!"
		);
		static_assert(
			!detail::is_function_ptr_v<std::decay_t<Mapper>>,
			"Map does not accpet raw function pointers as transformations!"
		);

		using combinator_result = decltype(Combinator()(it));
		using transform_result = decltype(std::apply(
			Mapper(),
			detail::as_tuple(std::declval<combinator_result>()->first)
		));
		if constexpr (detail::is_maybe_v<transform_result>) {
			auto res = Combinator()(it);
			if (res) {
				auto transformed = std::apply(
					Mapper(), detail::as_tuple(res->first)
				);
				if (transformed) {
					return make_result(
						detail::unwrap_tuple(std::move(*transformed)),
						res->second
					);
				}
			}
			// XXX(LPeter1997): Simplify?
			using result_type = decltype(make_result(
				detail::unwrap_tuple(
					*std::apply(Mapper(), detail::as_tuple(res->first))
				),
				it
			));
			return result_type();
		}
		else {
			auto res = Combinator()(it);
			if (res) {
				return make_result(
					detail::unwrap_tuple(
						std::apply(Mapper(), detail::as_tuple(res->first))
					),
					res->second
				);
			}
			// XXX(LPeter1997): Simplify?
			using result_type = decltype(make_result(
				detail::unwrap_tuple(
					std::apply(Mapper(), detail::as_tuple(res->first))
				),
				it
			));
			return result_type();
		}
	}

public:
	template <typename T>
	using maybe = detail::maybe<T>;

	using detail::maybe_ctors::make_maybe;

	template <auto Fn>
	using fn = detail::fn_wrap<Fn>;

	template <auto Fn>
	using cmb = detail::cmb_wrap<TokenIterator, Fn>;

	using succ = cmb<cmb_succ_fn>;

	template <typename T>
	using fail = cmb<cmb_fail_fn<T>>;

	using one = cmb<cmb_one_fn>;

	template <typename Combinator>
	using opt = cmb<cmb_opt_fn<Combinator>>;

	template <typename First, typename... Rest>
	using seq = cmb<cmb_seq_fn<First, Rest...>>;

	template <typename First, typename... Rest>
	using alt = cmb<cmb_alt_fn<typename First::data_type, First, Rest...>>;

	template <template <typename...> typename Collection, typename Combinator>
	using rep = cmb<cmb_rep_fn<Collection, Combinator>>;

	template <template <typename...> typename Collection, typename Combinator>
	using rep1 = cmb<cmb_rep1_fn<Collection, Combinator>>;

	template <typename Combinator, typename Mapper>
	using map = cmb<cmb_map_fn<Combinator, Mapper>>;

	template <typename Predicate>
	using filter = detail::filter_impl<Predicate>;

	template <std::size_t... Indicies>
	using select = detail::select_impl<Indicies...>;

	template <typename Folder>
	using foldl = detail::foldl_impl<Folder>;

	template <typename Folder>
	using foldr = detail::foldr_impl<Folder>;
};

template <typename TokenIterator>
struct combinator_values : private detail::maybe_ctors {
	using types = combinator_types<TokenIterator>;

	template <typename... Data>
	using result_type = typename types::template result_type<Data...>;

	template <typename T>
	using maybe = typename types::template maybe<T>;

	using detail::maybe_ctors::make_maybe;

	template <auto Fn>
	static constexpr auto cmb = typename types::template cmb<Fn>();

	template <auto Fn>
	static constexpr auto fn = typename types::template fn<Fn>();

	static constexpr auto succ = typename types::succ();

	static constexpr auto one = typename types::one();

	template <typename T>
	static constexpr auto fail = typename types::template fail<T>();

	template <typename Combinator>
	static constexpr auto opt(Combinator&&) {
		return typename types::template opt<std::decay_t<Combinator>>();
	}

	template <typename... Combinators>
	static constexpr auto seq(Combinators&&...) {
		return typename types::template seq<std::decay_t<Combinators>...>();
	}

	template <typename First, typename... Rest>
	static constexpr auto alt(First&&, Rest&&...) {
		return typename types::template alt<
			std::decay_t<First>, std::decay_t<Rest>...>();
	}

	template <template <typename...> typename Collection, typename Combinator>
	static constexpr auto rep(Combinator&&) {
		return typename types::template rep<Collection,
			std::decay_t<Combinator>>();
	}

	template <template <typename...> typename Collection, typename Combinator>
	static constexpr auto rep1(Combinator&&) {
		return typename types::template rep1<Collection,
			std::decay_t<Combinator>>();
	}

	template <typename Combinator, typename Mapper>
	static constexpr auto map(Combinator&&, Mapper&&) {
		return typename types::template map<std::decay_t<Combinator>,
			std::decay_t<Mapper>>();
	}

	template <typename Predicate>
	static constexpr auto filter(Predicate&&) {
		return typename types::template filter<std::decay_t<Predicate>>();
	}

	template <std::size_t... Indicies>
	static constexpr auto select =
		typename types::template select<Indicies...>();

	template <typename Folder>
	static constexpr auto foldl(Folder&&) {
		return typename types::template foldl<std::decay_t<Folder>>();
	}

	template <typename Folder>
	static constexpr auto foldr(Folder&&) {
		return typename types::template foldr<std::decay_t<Folder>>();
	}
};

/**
 * We can implement the subscript hack here.
 */
namespace detail {
	template <typename Combinator, typename Mapper>
	constexpr auto make_subscript_map(Combinator&& c, Mapper&& m) {
		using combinator_t = std::decay_t<Combinator>;
		using iter_type = typename combinator_t::iterator_type;

		return combinator_values<iter_type>::map(
			std::forward<Combinator>(c), std::forward<Mapper>(m));
	}
} /* namespace detail */

} /* namespace cppcmb */

/**
 * Operators for nicer syntax.
 */

/**
 * Optional.
 */
template <typename Combinator,
	typename = cppcmb::detail::enable_if_cmb_t<Combinator>>
constexpr auto operator~(Combinator&& c) {
	using combinator_t = std::decay_t<Combinator>;

	using iter_type = typename combinator_t::iterator_type;

	return cppcmb::combinator_values<iter_type>::opt(
		std::forward<Combinator>(c)
	);
}

/**
 * Sequencing.
 */
template <typename Left, typename Right,
	typename = cppcmb::detail::enable_if_cmb_t<Left>,
	typename = cppcmb::detail::enable_if_cmb_t<Right>>
constexpr auto operator&(Left&& l, Right&& r) {
	using left_t = std::decay_t<Left>;
	using right_t = std::decay_t<Right>;

	using iter_type = typename left_t::iterator_type;

	static_assert(std::is_same_v<
		iter_type, typename right_t::iterator_type>,
		"Sequenced iterator types must match!");

	return cppcmb::combinator_values<iter_type>::seq(
		std::forward<Left>(l), std::forward<Right>(r)
	);
}

/**
 * Alternatives.
 */
template <typename Left, typename Right,
	typename = cppcmb::detail::enable_if_cmb_t<Left>,
	typename = cppcmb::detail::enable_if_cmb_t<Right>>
constexpr auto operator|(Left&& l, Right&& r) {
	using left_t = std::decay_t<Left>;
	using right_t = std::decay_t<Right>;

	using iter_type = typename left_t::iterator_type;

	static_assert(std::is_same_v<
		iter_type, typename right_t::iterator_type>,
		"Alternate iterator types must match!");

	return cppcmb::combinator_values<iter_type>::alt(
		std::forward<Left>(l), std::forward<Right>(r)
	);
}

#endif /* CPPCMB_HPP */
