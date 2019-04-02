#include <string_view>
#include <vector>
#include "catch.hpp"
#include "../cppcmb.hpp"

namespace pc = cppcmb;

TEST_CASE("'one' returns a character if there is one", "[one]") {
	auto p = pc::one;

	SECTION("there is a character to consume") {
		std::string_view src = "aaa";
		auto res = p.apply(pc::reader(src));

		REQUIRE(res.is_success());
		auto succ = res.success();
		REQUIRE(succ.remaining() == 1);
		REQUIRE(succ.value() == 'a');
	}

	SECTION("there is no character to consume") {
		std::string_view src = "";
		auto res = p.apply(pc::reader(src));

		REQUIRE(res.is_failure());
		auto f = res.failure();
		REQUIRE(f.furthest() == 0);
	}
}
