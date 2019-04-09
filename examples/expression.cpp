#include <cctype>
#include <cmath>
#include <iostream>
#include <string_view>
#include "../cppcmb.hpp"

namespace pc = cppcmb;

template <char Ch>
bool is_same_char(char c) { return c == Ch; }

template <char Ch>
inline constexpr auto match = pc::one[pc::filter(is_same_char<Ch>)];

int do_op(int x, char ch, int y) {
    switch (ch) {
    case '+': return x + y;
    case '-': return x - y;
    case '*': return x * y;
    case '/': return x / y;
    case '^': return (int)std::pow(x, y); // For simplicity
	default: return 0;
    }
}

int to_num(std::vector<char> const& chs) {
    int n = 0;
    for (auto c : chs) n = n * 10 + (c - '0');
    return n;
}

cppcmb_decl(expr,  int);
cppcmb_decl(mul,   int);
cppcmb_decl(expon, int);
cppcmb_decl(atom,  int);
cppcmb_decl(num,   int);
cppcmb_decl(digit, char);

cppcmb_def(expr) = pc::pass
    | (expr & match<'+'> & mul) [do_op]
    | (expr & match<'-'> & mul) [do_op]
    | mul
    %= pc::as_memo_d;

cppcmb_def(mul) = pc::pass
    | (mul & match<'*'> & expon) [do_op]
    | (mul & match<'/'> & expon) [do_op]
    | expon
    %= pc::as_memo_d;

cppcmb_def(expon) = pc::pass
    | (atom & match<'^'> & expon) [do_op]
    | atom
    %= pc::as_memo_d;

cppcmb_def(atom) = pc::pass
    | (match<'('> & expr & match<')'>) [pc::select<1>]
    | num
    %= pc::as_memo_d;

cppcmb_def(num) =
      (+digit) [to_num]
    ;

cppcmb_def(digit) = pc::one[pc::filter(isdigit)];

int main() {
    auto parser = pc::parser(expr);
    std::string line;

    while (std::getline(std::cin, line)) {
        auto res = parser.parse(line);
        if (res.is_success()) {
            std::cout << "Result = " << res.success().value() << std::endl;
        }
        else {
            std::cout << "Failed to parse expression!" << std::endl;
        }
    }

    return 0;
}
