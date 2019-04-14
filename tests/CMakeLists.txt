cmake_minimum_required(VERSION 3.6 FATAL_ERROR)

project(CppCmb_Tests)

set(CMAKE_CXX_STANDARD 17)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
else()
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror -pedantic")
endif()

set(ALL_SOURCES
	catch.cpp
	test_fundamentals.cpp
)

add_executable(tests ${ALL_SOURCES})
