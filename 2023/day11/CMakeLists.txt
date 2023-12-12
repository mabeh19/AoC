cmake_minimum_required(VERSION 3.28)

set(CMAKE_BUILD_TYPE Debug)

project(day11)

add_executable(day11 src/day11.cpp)

find_library(FMT fmt)
target_link_libraries(day11 ${FMT})
