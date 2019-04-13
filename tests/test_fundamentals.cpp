#include <string_view>
#include <type_traits>
#include <vector>
#include "catch.hpp"
#include "../cppcmb.hpp"

namespace pc = cppcmb;

template <typename T, typename U>
inline constexpr bool same_type_v =
	std::is_same_v<std::decay_t<T>, std::decay_t<U>>;

TEST_CASE("'one' returns a character if there is one", "[one]") {
	auto p = pc::one;

	SECTION("there is a character to consume") {
		std::string_view src = "aaa";
		auto res = p.apply(pc::reader(src));

		REQUIRE(res.is_success());
		auto succ = res.success();
		REQUIRE(res.furthest() == 1);
		REQUIRE(succ.matched() == 1);
		REQUIRE(succ.value() == 'a');
	}

	SECTION("there is no character to consume") {
		std::string_view src = "";
		auto res = p.apply(pc::reader(src));

		REQUIRE(res.is_failure());
		REQUIRE(res.furthest() == 0);
	}
}

TEST_CASE("'end' succeeds is there is nothing to consume", "[end]") {
	auto p = pc::end;

	SECTION("there is a character to consume") {
		std::string_view src = "aaa";
		auto res = p.apply(pc::reader(src));

		REQUIRE(res.is_failure());
		REQUIRE(res.furthest() == 0);
	}

	SECTION("there is no character to consume") {
		std::string_view src = "";
		auto res = p.apply(pc::reader(src));

		REQUIRE(res.is_success());
		auto succ = res.success();
		REQUIRE(res.furthest() == 0);
		REQUIRE(succ.matched() == 0);
		REQUIRE(same_type_v<decltype(succ.value()), pc::product<>>);
	}
}

template <char Ch>
constexpr bool is_char(char c) noexcept { return c == Ch; }

template <char Ch>
inline constexpr auto match = pc::one[pc::filter(is_char<Ch>)];

TEST_CASE("'match' is user-defined, here we test filtering", "[match]") {
	auto p = match<'a'>;

	SECTION("there is no character to consume") {
		std::string_view src = "";
		auto res = p.apply(pc::reader(src));

		REQUIRE(res.is_failure());
		REQUIRE(res.furthest() == 0);
	}

	SECTION("there is a character to consume and it matches") {
		std::string_view src = "aaa";
		auto res = p.apply(pc::reader(src));

		REQUIRE(res.is_success());
		auto succ = res.success();
		REQUIRE(res.furthest() == 1);
		REQUIRE(succ.value() == 'a');
		REQUIRE(succ.matched() == 1);
	}

	SECTION("there is a character to consume but it doesn't match") {
		std::string_view src = "baa";
		auto res = p.apply(pc::reader(src));

		REQUIRE(res.is_failure());
		REQUIRE(res.furthest() == 1);
	}
}

TEST_CASE("'seq' succeeds if all elements succeed", "[seq]") {
	SECTION("two-element sequence") {
		auto p = match<'a'> & pc::end;

		SECTION("the first part fails") {
			std::string_view src = "baa";
			auto res = p.apply(pc::reader(src));

			REQUIRE(res.is_failure());
			REQUIRE(res.furthest() == 1);
		}

		SECTION("the second part fails") {
			std::string_view src = "aaa";
			auto res = p.apply(pc::reader(src));

			REQUIRE(res.is_failure());
			REQUIRE(res.furthest() == 1);
		}

		SECTION("matches the input") {
			std::string_view src = "a";
			auto res = p.apply(pc::reader(src));

			REQUIRE(res.is_success());
			auto succ = res.success();
			REQUIRE(res.furthest() == 1);
			REQUIRE(succ.value() == 'a');
			REQUIRE(succ.matched() == 1);
		}
	}

	SECTION("three-element sequence") {
		auto p = match<'a'> & match<'b'> & match<'c'>;

		SECTION("the first part fails") {
			std::string_view src = "bbc";
			auto res = p.apply(pc::reader(src));

			REQUIRE(res.is_failure());
			REQUIRE(res.furthest() == 1);
		}

		SECTION("the center part fails") {
			std::string_view src = "acc";
			auto res = p.apply(pc::reader(src));

			REQUIRE(res.is_failure());
			REQUIRE(res.furthest() == 2);
		}

		SECTION("the last part fails") {
			std::string_view src = "abd";
			auto res = p.apply(pc::reader(src));

			REQUIRE(res.is_failure());
			REQUIRE(res.furthest() == 3);
		}

		SECTION("matches the input") {
			std::string_view src = "abc";
			auto res = p.apply(pc::reader(src));

			REQUIRE(res.is_success());
			auto succ = res.success();
			REQUIRE(res.furthest() == 3);
			REQUIRE(same_type_v<decltype(succ.value()), pc::product<char, char, char>>);
			// XXX(LPeter1997): GCC bug?
			REQUIRE(succ.value() == pc::product<char, char, char>('a', 'b', 'c'));
			REQUIRE(succ.matched() == 3);
		}
	}
}
