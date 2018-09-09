#include <vector>
#include "catch.hpp"
#include "../cppcmb.hpp"

using int_vector = std::vector<int>;
using int_iterator = typename int_vector::const_iterator;
using pt = cppcmb::combinator_types<int_iterator>;
using pv = cppcmb::combinator_values<int_iterator>;

TEST_CASE("'succ' always succeeds, stays in position and returns an empty tuple", "[succ]") {
	int_vector v = { 1, 2, 3 };

	auto result = pv::succ(std::cbegin(v));
	REQUIRE(result);
	REQUIRE(result->first == std::make_tuple());
	REQUIRE(std::distance(std::cbegin(v), result->second) == 0);
}

TEST_CASE("'one' takes a single element from the current position", "[one]") {
	int_vector v = { 1, 2, 3 };

	auto result = pv::one(std::cbegin(v));
	REQUIRE(result);
	REQUIRE(result->first == 1);
	REQUIRE(std::distance(std::cbegin(v), result->second) == 1);

	auto result2 = pv::one(std::cbegin(v) + 1);
	REQUIRE(result2);
	REQUIRE(result2->first == 2);
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
	REQUIRE(result->first == 1);
	REQUIRE(std::distance(std::cbegin(v), result->second) == 1);

	auto result2 = match<2>(std::cbegin(v));
	REQUIRE(!result2);
}

TEST_CASE("'opt' always succeeds, but does not always consume", "[opt]") {
	int_vector v = { 1, 2, 3 };

	SECTION("on matching the parser advances") {
		auto result = pv::opt(match<1>)(std::cbegin(v));
		REQUIRE(result);
		REQUIRE(result->first);
		REQUIRE(std::distance(std::cbegin(v), result->second) == 1);
	}
	SECTION("when not matching, still succeed, but no advancement") {
		auto result = pv::opt(match<2>)(std::cbegin(v));
		REQUIRE(result);
		REQUIRE(!result->first);
		REQUIRE(std::distance(std::cbegin(v), result->second) == 0);
	}
	SECTION("on matching the parser advances (with operator)") {
		auto result = (~match<1>)(std::cbegin(v));
		REQUIRE(result);
		REQUIRE(result->first);
		REQUIRE(std::distance(std::cbegin(v), result->second) == 1);
	}
	SECTION("when not matching, still succeed, but no advancement (with operator)") {
		auto result = (~match<2>)(std::cbegin(v));
		REQUIRE(result);
		REQUIRE(!result->first);
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

TEST_CASE("'alt' tries all the alternatives until one succeeds", "[alt]") {
	int_vector v = { 1, 2, 3, 4, 5 };

	SECTION("succeeding can occur as the first alternative") {
		auto result = (match<1> | match<2> | match<3>)(std::cbegin(v));
		REQUIRE(result);
		REQUIRE(result->first == 1);
		REQUIRE(std::distance(std::cbegin(v), result->second) == 1);
	}
	SECTION("succeeding can occur as some middle alternative") {
		auto result = (match<2> | match<1> | match<3>)(std::cbegin(v));
		REQUIRE(result);
		REQUIRE(result->first == 1);
		REQUIRE(std::distance(std::cbegin(v), result->second) == 1);
	}
	SECTION("succeeding can occur as the last alternative") {
		auto result = (match<2> | match<3> | match<1>)(std::cbegin(v));
		REQUIRE(result);
		REQUIRE(result->first == 1);
		REQUIRE(std::distance(std::cbegin(v), result->second) == 1);
	}
	SECTION("it fails when no alternatives match") {
		auto result = (match<2> | match<4> | match<3>)(std::cbegin(v));
		REQUIRE(!result);
	}
}

TEST_CASE("'rep' collects the same result while it's combinator succeeds", "[rep]") {
	int_vector v = { 5, 2, 2, 2, 5 };

	SECTION("succeeds on zero matches") {
		auto result = (pv::rep<std::vector>(match<1>))(std::cbegin(v));
		REQUIRE(result);
		REQUIRE(result->first.size() == 0);
		REQUIRE(result->first == int_vector{});
		REQUIRE(std::distance(std::cbegin(v), result->second) == 0);
	}
	SECTION("succeeds on a single match") {
		auto result = (pv::rep<std::vector>(match<5>))(std::cbegin(v));
		REQUIRE(result);
		REQUIRE(result->first.size() == 1);
		REQUIRE(result->first == int_vector{ 5 });
		REQUIRE(std::distance(std::cbegin(v), result->second) == 1);
	}
	SECTION("succeeds on multiple matches") {
		auto result = (pv::rep<std::vector>(match<2>))(std::cbegin(v) + 1);
		REQUIRE(result);
		REQUIRE(result->first.size() == 3);
		REQUIRE(result->first == int_vector{ 2, 2, 2 });
		REQUIRE(std::distance(std::cbegin(v) + 1, result->second) == 3);
	}
}

TEST_CASE("'rep1' collects the same result while it's combinator succeeds and expects at least one element", "[rep1]") {
	int_vector v = { 5, 2, 2, 2, 5 };

	SECTION("fails on zero matches") {
		auto result = (pv::rep1<std::vector>(match<1>))(std::cbegin(v));
		REQUIRE(!result);
	}
	SECTION("succeeds on a single match") {
		auto result = (pv::rep1<std::vector>(match<5>))(std::cbegin(v));
		REQUIRE(result);
		REQUIRE(result->first.size() == 1);
		REQUIRE(result->first == int_vector{ 5 });
		REQUIRE(std::distance(std::cbegin(v), result->second) == 1);
	}
	SECTION("succeeds on multiple matches") {
		auto result = (pv::rep1<std::vector>(match<2>))(std::cbegin(v) + 1);
		REQUIRE(result);
		REQUIRE(result->first.size() == 3);
		REQUIRE(result->first == int_vector{ 2, 2, 2 });
		REQUIRE(std::distance(std::cbegin(v) + 1, result->second) == 3);
	}
}
