# Created By Alex Tsao
cmake_minimum_required(VERSION 3.14)
project(tinylalr)
set(CMAKE_CXX_STANDARD 17)

function(add_executable_dirs target property dirs)
    foreach (dir ${dirs} ${ARGN})
        aux_source_directory(${dir} sources)
    endforeach ()
    add_executable(${target} ${sources})
    target_include_directories(${target} ${property} ${dirs} ${ARGN})
endfunction()
add_executable_dirs(${PROJECT_NAME} PRIVATE .)
