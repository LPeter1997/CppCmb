cmake_minimum_required(VERSION 3.6 FATAL_ERROR)

project(CppCmb_Examples)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror -pedantic")

add_executable(expression expression.cpp)
add_executable(anbn anbn.cpp)
