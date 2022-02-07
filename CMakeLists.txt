# Created By Alex Tsao
cmake_minimum_required(VERSION 3.14)
project(tinylalr)
set(CMAKE_CXX_STANDARD 17)

include_directories(.)

add_executable(test_parser test/test_parser.cpp test/parser.cpp test/parser.h)

add_executable(test_regex test/test_regex.cpp regex.h lexer.h)

add_executable(gen generator.cpp)
