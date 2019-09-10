cmake_minimum_required(VERSION 3.12 FATAL_ERROR)

project(
    CppCmb
    VERSION 0.0.1
    DESCRIPTION "A simple to use C++17 parser combinator library"
    LANGUAGES CXX)

set(CMAKE_CXX_STANDARD          17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_library(cppcmb INTERFACE)
target_include_directories(cppcmb INTERFACE include)

# TODO: remove
add_subdirectory(build)
