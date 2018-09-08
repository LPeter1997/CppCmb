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
}
