#include <vector>
#include "catch.hpp"
#include "../cppcmb.hpp"

using int_vector = std::vector<int>;
using int_iterator = typename int_vector::const_iterator;
using pt = cppcmb::combinator_types<int_iterator>;
using pv = cppcmb::combinator_values<int_iterator>;

TEST_CASE("'one' takes a single element from the current position", "[one]") {
	int_vector v = { 1, 2, 3 };

	auto result = pv::one(std::cbegin(v));

	REQUIRE(result);
	REQUIRE(std::get<0>(result->first) == 1);
	REQUIRE(std::distance(std::cbegin(v), result->second) == 1);

	auto result2 = pv::one(std::cbegin(v) + 1);

	REQUIRE(result2);
	REQUIRE(std::get<0>(result2->first) == 2);
	REQUIRE(std::distance(std::cbegin(v), result2->second) == 2);
}

template <int Num>
static constexpr bool match_pred(int n) {
	return n == Num;
}

template <int Num>
static constexpr auto match = pv::one[pv::filter(pv::fn<match_pred<Num>>)];

TEST_CASE("A constructed match functionality accepts only the predicate", "[match]") {
	int_vector v = { 1, 2, 3 };

	auto result = match<1>(std::cbegin(v));
	REQUIRE(result);
	REQUIRE(std::get<0>(result->first) == 1);
	REQUIRE(std::distance(std::cbegin(v), result->second) == 1);

	auto result2 = match<2>(std::cbegin(v));
	REQUIRE(!result2);
}

TEST_CASE("'opt' always succeeds, but does not always consume", "[opt]") {
	int_vector v = { 1, 2, 3 };

	SECTION("on matching the parser advances") {
		auto result = pv::opt(match<1>)(std::cbegin(v));

		REQUIRE(result);
		REQUIRE(std::get<0>(result->first));
		REQUIRE(std::distance(std::cbegin(v), result->second) == 1);
	}
	SECTION("when not matching, still succeed, but no advancement") {
		auto result = pv::opt(match<2>)(std::cbegin(v));

		REQUIRE(result);
		REQUIRE(!std::get<0>(result->first));
		REQUIRE(std::distance(std::cbegin(v), result->second) == 0);
	}
}

TEST_CASE("'seq' succeeds when all it's elements succeed", "[seq]") {
	int_vector v = { 1, 2, 3, 4, 5 };

	SECTION("it succeeds when it matches all the way") {
		auto result = (match<1> & match<2> & match<3> & match<4>)(std::cbegin(v));
		REQUIRE(result);
		REQUIRE(result->first == std::make_tuple(1, 2, 3, 4));
		REQUIRE(std::distance(std::cbegin(v), result->second) == 4);
	}
	SECTION("it can fail right at the beginning") {
		auto result = (match<2> & match<2> & match<3> & match<4>)(std::cbegin(v));
		REQUIRE(!result);
	}
	SECTION("it can fail in the middle") {
		auto result = (match<1> & match<2> & match<4> & match<4>)(std::cbegin(v));
		REQUIRE(!result);
	}
	SECTION("it can fail at the end") {
		auto result = (match<1> & match<2> & match<3> & match<5>)(std::cbegin(v));
		REQUIRE(!result);
	}
}
