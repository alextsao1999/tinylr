# Created By Alex Tsao
cmake_minimum_required(VERSION 3.14)
project(tinylalr)
set(CMAKE_CXX_STANDARD 17)

include_directories(.)

add_executable(tinylalr test.cpp parser.cpp)

add_executable(generator generator.cpp)
