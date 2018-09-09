#include <cassert>
#include <cctype>
#include <cmath>
#include <vector>
#include "catch.hpp"
#include "../cppcmb.hpp"

struct token {
	enum type {
		num,
		expon,
		add, sub, mul, div, lparen, rparen,
		eps
	};

	type ty;
	std::string val;

	explicit token(type ty)
		: ty(ty) {
	}

	explicit token(type ty, std::string const& val)
		: ty(ty), val(val) {
	}
};

// Hacky lexer /////////////////////////////////////////////////////////////////

std::vector<token> lex(char const* src) {
	std::vector<token> res;
begin:
	switch (*src) {
	case '\0': res.push_back(token(token::eps)); return res;
	case ' ': ++src; goto begin;
	case '^': res.push_back(token(token::expon)); ++src; goto begin;
	case '+': res.push_back(token(token::add)); ++src; goto begin;
	case '-': res.push_back(token(token::sub)); ++src; goto begin;
	case '*': res.push_back(token(token::mul)); ++src; goto begin;
	case '/': res.push_back(token(token::div)); ++src; goto begin;
	case '(': res.push_back(token(token::lparen)); ++src; goto begin;
	case ')': res.push_back(token(token::rparen)); ++src; goto begin;

	default:
		if (std::isdigit(*src)) {
			std::string v = "";
			while (std::isdigit(*src)) {
				v += *src++;
			}
			res.push_back(token(token::num, v));
			goto begin;
		}
		else {
			assert(false && "Unknown!");
		}
	}
	assert(false && "Unreachable!");
	return res;
}

// Actual code from wiki, tests

// For all the tests

using token_iterator = typename std::vector<token>::const_iterator;
using pt = cppcmb::combinator_types<token_iterator>;
using pv = cppcmb::combinator_values<token_iterator>;

// First example, baby parser

TEST_CASE("a baby parser", "[A baby parser]") {
	auto tokens = lex("+12");
	// Template-style
	auto result1 = pt::one()(std::cbegin(tokens));
	// Value-style
	auto result2 = pv::one(std::cbegin(tokens));

	REQUIRE(result1);
	REQUIRE(result2);

	auto& data1 = result1->first;
	auto& data2 = result2->first;

	REQUIRE(data1.ty == token::add);
	REQUIRE(data2.ty == token::add);
}

// Matching example

template <token::type Expected_Type>
constexpr pv::result_type<token> match_combinator(token_iterator it) {
	auto result = pv::one(it);
	if (result) {
		if (result->first.ty == Expected_Type) {
			return result;
		}
	}
	return std::nullopt;
}

// Template-style
template <token::type Expected_Type>
using term_t = pt::wrap<match_combinator<Expected_Type>>;

// Value-style
template <token::type Expected_Type>
static constexpr auto term_v = pv::wrap<match_combinator<Expected_Type>>;

TEST_CASE("matching a token", "[Matching a token]") {
	auto tokens = lex("+12");
	// Template-style
	auto result1 = term_t<token::add>()(std::cbegin(tokens));
	// Value-style
	auto result2 = term_v<token::add>(std::cbegin(tokens));

	REQUIRE(result1);
	REQUIRE(result2);

	auto& data1 = result1->first;
	auto& data2 = result2->first;

	REQUIRE(data1.ty == token::add);
	REQUIRE(data2.ty == token::add);
}

TEST_CASE("sequencing tokens", "[Sequencing tokens]") {
	auto tokens = lex("+12");

	SECTION("using the 'seq' function") {
		// Template-style
		auto result1 = pt::seq<term_t<token::add>, term_t<token::num>>()(std::cbegin(tokens));
		// Value-style
		auto result2 = pv::seq(term_v<token::add>, term_v<token::num>)(std::cbegin(tokens));

		REQUIRE(result1);
		REQUIRE(result2);

		auto& data1 = result1->first;
		auto& data2 = result2->first;

		REQUIRE(std::get<0>(data1).ty == token::add);
		REQUIRE(std::get<1>(data1).val == "12");
		REQUIRE(std::get<0>(data2).ty == token::add);
		REQUIRE(std::get<1>(data2).val == "12");
	}
	SECTION("using the sequencing operator") {
		// Template-style
		auto result1 = (term_t<token::add>() & term_t<token::num>())(std::cbegin(tokens));
		// Value-style
		auto result2 = (term_v<token::add> & term_v<token::num>)(std::cbegin(tokens));

		REQUIRE(result1);
		REQUIRE(result2);

		auto& data1 = result1->first;
		auto& data2 = result2->first;

		REQUIRE(std::get<0>(data1).ty == token::add);
		REQUIRE(std::get<1>(data1).val == "12");
		REQUIRE(std::get<0>(data2).ty == token::add);
		REQUIRE(std::get<1>(data2).val == "12");
	}
}

TEST_CASE("alternatives", "[Alternatives]") {
	auto tokens = lex("-15");

	SECTION("using the 'alt' function") {
		// Template-style
		auto parser1 = pt::seq<
			pt::alt<term_t<token::add>, term_t<token::sub>>,
			term_t<token::num>
		>();
		// Value-style
		auto parser2 = pv::seq(
			pv::alt(term_v<token::add>, term_v<token::sub>),
			term_v<token::num>
		);
		// Template-style
		auto result1 = parser1(std::cbegin(tokens));
		// Value-style
		auto result2 = parser2(std::cbegin(tokens));

		REQUIRE(result1);
		REQUIRE(result2);

		auto& data1 = result1->first;
		auto& data2 = result2->first;

		REQUIRE(std::get<0>(data1).ty == token::sub);
		REQUIRE(std::get<1>(data1).val == "15");
		REQUIRE(std::get<0>(data2).ty == token::sub);
		REQUIRE(std::get<1>(data2).val == "15");
	}
	SECTION("using the alternative operator") {
		auto parser = (term_v<token::add> | term_v<token::sub>) & term_v<token::num>;
		auto result = parser(std::cbegin(tokens));

		REQUIRE(result);

		auto& data = result->first;

		REQUIRE(std::get<0>(data).ty == token::sub);
		REQUIRE(std::get<1>(data).val == "15");
	}
}

// The converter function
int token_to_int(token const& t) {
	return std::stoi(t.val);
}

TEST_CASE("transform token to int", "[Transformations]") {
	// The parser itself
	auto parser = pv::map(term_v<token::num>, pv::fn<token_to_int>);
	auto tokens = lex("231");

	auto result = parser(std::cbegin(tokens));

	REQUIRE(result);
	REQUIRE(result->first == 231);
}

// Mapper
int to_int(std::optional<token> const& sign, token const& num) {
	int value =  std::stoi(num.val);
	if (sign) {
		if (sign->ty == token::sub) {
			value = -value;
		}
	}
	return value;
}

TEST_CASE("transform signed token to int", "[Transformations]") {
	SECTION("using the 'map' function") {
		// The parser itself
		auto parser = pv::map(pv::opt(term_v<token::add> | term_v<token::sub>) & term_v<token::num>, pv::fn<to_int>);
		auto tokens = lex("-123");

		auto result = parser(std::cbegin(tokens));

		REQUIRE(result);
		REQUIRE(result->first == -123);
	}
	SECTION("using the mapping operator") {
		// The parser itself
		auto parser = (pv::opt(term_v<token::add> | term_v<token::sub>) & term_v<token::num>)[pv::fn<to_int>];
		auto tokens = lex("-123");

		auto result = parser(std::cbegin(tokens));

		REQUIRE(result);
		REQUIRE(result->first == -123);
	}
	SECTION("using the optional and mapping operator") {
		// The parser itself
		auto parser = (~(term_v<token::add> | term_v<token::sub>) & term_v<token::num>)[pv::fn<to_int>];
		auto tokens = lex("-123");

		auto result = parser(std::cbegin(tokens));

		REQUIRE(result);
		REQUIRE(result->first == -123);
	}
}

// Expressions /////////////////////////////////////////////////////////////////

struct expr_t {
	virtual double eval() const = 0;
};

struct num_expr_t : public expr_t {
	int val;

	explicit num_expr_t(token const& tok)
		: val(std::stoi(tok.val)) {
	}

	virtual double eval() const override {
		return double(val);
	}
};

struct bin_expr_t : public expr_t {
	token::type op;
	expr_t* left;
	expr_t* right;

	explicit bin_expr_t(token::type op, expr_t* l, expr_t* r)
		: op(op), left(l), right(r) {
	}

	virtual double eval() const override {
		auto l = left->eval();
		auto r = right->eval();

		switch (op) {
		case token::add: return l + r;
		case token::sub: return l - r;
		case token::mul: return l * r;
		case token::div: return l / r;
		case token::expon: return std::pow(l, r);

		default: assert(false);
		}
		return 0.0;
	}
};

expr_t* to_int_expr(token const& num) {
	return new num_expr_t(num);
}

// Left-folding variant
expr_t* to_bin_expr_l(expr_t* left, std::tuple<token, expr_t*> const& right) {
	return new bin_expr_t(std::get<0>(right).ty, left, std::get<1>(right));
}

// Right-folding variant
expr_t* to_bin_expr_r(std::tuple<expr_t*, token> const& left, expr_t* right) {
	return new bin_expr_t(std::get<1>(left).ty, std::get<0>(left), right);
}

pv::result_type<expr_t*> expression(token_iterator it) {
	constexpr auto atomic =
		  (term_v<token::lparen> & pv::wrap<expression> & term_v<token::rparen>)[pv::select<1>]
		| term_v<token::num>[pv::fn<to_int_expr>]
		;
	constexpr auto exponentiation =
		(pv::rep<std::vector>(atomic & term_v<token::expon>) & atomic)[pv::foldr(pv::fn<to_bin_expr_r>)]
		;
	constexpr auto multiplication =
		(exponentiation & pv::rep<std::vector>((term_v<token::mul> | term_v<token::div>) & exponentiation))[pv::foldl(pv::fn<to_bin_expr_l>)]
		;
	constexpr auto addition =
		(multiplication & pv::rep<std::vector>((term_v<token::add> | term_v<token::sub>) & multiplication))[pv::foldl(pv::fn<to_bin_expr_l>)]
		;
	return addition(it);
}

bool is_double_eq(double d1, double d2) {
	return std::abs(d1 - d2) < 0.0001;
}

TEST_CASE("parsing a math expression", "[Advanced parsing and recursion]") {
	auto tokens = lex("2*(1+2)^3+1-2+3");
	auto parser = pv::wrap<expression>;
	auto result = parser(std::cbegin(tokens));

	REQUIRE(result);
	REQUIRE(is_double_eq(result->first->eval(), 56.0));
}
