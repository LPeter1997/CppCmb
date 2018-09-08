![alt text](https://github.com/LPeter1997/CppCmb/blob/master/cppcmb_logo.svg "CppCmb Logo")

# What is CppCmb?

CppCmb is a [single-header](https://github.com/LPeter1997/CppCmb/blob/master/cppcmb.hpp) C++17 monadic parser-combinator library that aims for genericity and performance. It makes no heap allocations and has no runtime cost for building the parser.

# How do I compile/install it?

It's a [single header file](https://github.com/LPeter1997/CppCmb/blob/master/cppcmb.hpp) that you can just drop into your project and use it straight away.

# How do I use it?

See the [wiki](https://github.com/LPeter1997/CppCmb/wiki) for documentation and tutorial. See [examples folder](https://github.com/LPeter1997/CppCmb/tree/master/examples) for usage.

# How can I contribute?

You can open issues, or do a pull-request if you've implemented/fixed something.

# To do:
* Provide some helper to implement terminal matching
* Automatic single-element tuple unwrapping when possible
* Create regression tests
* Add more examples
* Test and make it work on MSVC (currently only workd with GCC, untested on everything else)
* Add static assertions to make the library easier to use (for example not passing function wrapper)
