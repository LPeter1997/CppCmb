#include <cassert>
#include <cmath>
#include <cstdio>
#include <cctype>
#include <string>
#include <vector>
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

// AST /////////////////////////////////////////////////////////////////////////

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

// Actual library usage ////////////////////////////////////////////////////////

using token_iterator = typename std::vector<token>::const_iterator;
using pt = cppcmb::combinator_types<token_iterator>;
using pv = cppcmb::combinator_values<token_iterator>;

// Token matching predicate
template <token::type Expected_Type>
constexpr bool match_predicate(token const& tok) {
	return tok.ty == Expected_Type;
}

// Template-style
template <token::type Expected_Type>
using term_t = pt::map<pt::one, pt::filter<pt::fn<match_predicate<Expected_Type>>>>;

// Value-style
template <token::type Expected_Type>
static constexpr auto term_v = pv::one[pv::filter(pv::fn<match_predicate<Expected_Type>>)];

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

int main() {
	auto tokens = lex("2*(1+2)^3+1-2+3");
	auto parser = pv::wrap<expression>;
	auto result = parser(std::cbegin(tokens));

	if (result) {
		std::puts("Success!");
		std::printf("Evaluates to: %lf\n", result->first->eval());
	}
	else {
		std::puts("Fail!");
	}

	return 0;
}
