#include "catch.hpp"
#include "../cppcmb.hpp"

using ct = cppcmb::combinator_comptime<std::tuple>;

struct A {};
struct B {};
struct C {};
struct D {};
struct E {};

TEST_CASE("Comptime 'succ' always succeeds, stays in position and returns an empty tuple", "[comptime:succ]") {
	using input = std::tuple<A, B, C>;

	using result = ct::succ<input>;
	REQUIRE(result::success);
	REQUIRE((std::is_same_v<typename result::result, std::tuple<>>));
	REQUIRE((std::is_same_v<typename result::remaining, std::tuple<A, B, C>>));
}

TEST_CASE("Comptime 'fail' always fails", "[comptime:fail]") {
	using input = std::tuple<A, B, C>;

	using result = ct::fail<input>;
	REQUIRE(!result::success);
}

TEST_CASE("Comptime 'one' takes a single element from the current position", "[comptime:one]") {
	using input1 = std::tuple<A, B, C>;

	using result1 = ct::one<input1>;
	REQUIRE(result1::success);
	REQUIRE((std::is_same_v<typename result1::result, A>));
	REQUIRE((std::is_same_v<typename result1::remaining, std::tuple<B, C>>));

	using input2 = typename result1::remaining;
	using result2 = ct::one<input2>;
	REQUIRE(result2::success);
	REQUIRE((std::is_same_v<typename result2::result, B>));
	REQUIRE((std::is_same_v<typename result2::remaining, std::tuple<C>>));
}

template <typename T>
struct match {
	template <typename>
	struct type;

	template <typename... Ts>
	struct type<std::tuple<Ts...>> : ct::fail_result {};

	template <typename... Ts>
	struct type<std::tuple<T, Ts...>>
		: ct::success_result<T, std::tuple<Ts...>> {};
};

TEST_CASE("Comptime 'opt' always succeeds, but does not always consume", "[comptime:opt]") {
	using input = std::tuple<A, B, C>;

	SECTION("on matching the parser advances") {
		using result = ct::opt<match<A>::type>::type<input>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, A>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<B, C>>));
	}
	SECTION("when not matching, still succeed, but no advancement") {
		using result = ct::opt<match<B>::type>::type<input>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, std::tuple<>>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<A, B, C>>));
	}
}

TEST_CASE("Comptime 'seq' succeeds when all it's elements succeed", "[comptime:seq]") {
	using input = std::tuple<A, B, C, D, E>;

	SECTION("it succeeds when it matches all the way") {
		using result = ct::seq<match<A>::type, match<B>::type, match<C>::type, match<D>::type>::type<input>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, std::tuple<A, B, C, D>>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<E>>));
	}
	SECTION("it can fail right at the beginning") {
		using result = ct::seq<match<B>::type, match<B>::type, match<C>::type, match<D>::type>::type<input>;
		REQUIRE(!result::success);
	}
	SECTION("it can fail in the middle") {
		using result = ct::seq<match<A>::type, match<B>::type, match<D>::type, match<D>::type>::type<input>;
		REQUIRE(!result::success);
	}
	SECTION("it can fail at the end") {
		using result = ct::seq<match<A>::type, match<B>::type, match<C>::type, match<E>::type>::type<input>;
		REQUIRE(!result::success);
	}
}

TEST_CASE("Comptime 'alt' tries all the alternatives until one succeeds", "[comptime:alt]") {
	using input = std::tuple<A, B, C, D, E>;

	SECTION("succeeding can occur as the first alternative") {
		using result = ct::alt<match<A>::type, match<B>::type, match<C>::type>::type<input>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, A>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<B, C, D, E>>));
	}
	SECTION("succeeding can occur as some middle alternative") {
		using result = ct::alt<match<B>::type, match<A>::type, match<C>::type>::type<input>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, A>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<B, C, D, E>>));
	}
	SECTION("succeeding can occur as the last alternative") {
		using result = ct::alt<match<B>::type, match<C>::type, match<A>::type>::type<input>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, A>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<B, C, D, E>>));
	}
	SECTION("it fails when no alternatives match") {
		using result = ct::alt<match<B>::type, match<D>::type, match<C>::type>::type<input>;
		REQUIRE(!result::success);
	}
}

TEST_CASE("Comptime 'rep' collects the same result while it's combinator succeeds", "[comptime:rep]") {
	using input1 = std::tuple<E, B, B, B, E>;
	using input2 = std::tuple<B, B, B, E>;

	SECTION("succeeds on zero matches") {
		using result = ct::rep<match<A>::type>::type<input1>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, std::tuple<>>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<E, B, B, B, E>>));
	}
	SECTION("succeeds on a single match") {
		using result = ct::rep<match<E>::type>::type<input1>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, E>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<B, B, B, E>>));
	}
	SECTION("succeeds on multiple matches") {
		using result = ct::rep<match<B>::type>::type<input2>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, std::tuple<B, B, B>>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<E>>));
	}
}

TEST_CASE("Comptime 'rep1' collects the same result while it's combinator succeeds and expects at least one element", "[comptime:rep1]") {
	using input1 = std::tuple<E, B, B, B, E>;
	using input2 = std::tuple<B, B, B, E>;

	SECTION("fails on zero matches") {
		using result = ct::rep1<match<A>::type>::type<input1>;
		REQUIRE(!result::success);
	}
	SECTION("succeeds on a single match") {
		using result = ct::rep1<match<E>::type>::type<input1>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, E>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<B, B, B, E>>));
	}
	SECTION("succeeds on multiple matches") {
		using result = ct::rep1<match<B>::type>::type<input2>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, std::tuple<B, B, B>>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<E>>));
	}
}

template <typename T>
struct transform_to_d {
	using type = D;
};

////

template <typename T>
struct transform_to_d_if_a {
	using type = ct::map_fail;
};

template <>
struct transform_to_d_if_a<A> {
	using type = ct::map_succ<D>;
};

////

template <typename T>
struct transform_to_d_if_b {
	using type = ct::map_fail;
};

template <>
struct transform_to_d_if_b<B> {
	using type = ct::map_succ<D>;
};

TEST_CASE("Comptime 'map' applies a transformation for a successful result", "[comptime:map]") {
	using input = std::tuple<A, B, C>;

	SECTION("failing the inner combinator does not apply mapping") {
		using result = ct::map<match<B>::type, transform_to_d>::type<input>;
		REQUIRE(!result::success);
	}
	SECTION("succeeding the inner combinator applies the mapping") {
		using result = ct::map<match<A>::type, transform_to_d>::type<input>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, D>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<B, C>>));
	}
	SECTION("succeeding the inner combinator applies the mapping, that also succeeds") {
		using result = ct::map<match<A>::type, transform_to_d_if_a>::type<input>;
		REQUIRE(result::success);
		REQUIRE((std::is_same_v<typename result::result, D>));
		REQUIRE((std::is_same_v<typename result::remaining, std::tuple<B, C>>));
	}
	SECTION("succeeding the inner combinator applies the mapping that fails, so the whole thing fails") {
		using result = ct::map<match<A>::type, transform_to_d_if_b>::type<input>;
		REQUIRE(!result::success);
	}
}
