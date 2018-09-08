cmake_minimum_required(VERSION 3.6 FATAL_ERROR)

project(CppCmb_Tests)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror -pedantic")

set(ALL_SOURCES
	catch.cpp
	test_fundamentals.cpp
)

add_executable(tests ${ALL_SOURCES})
