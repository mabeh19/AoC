cmake_minimum_required(VERSION 3.27)

project(day12)

add_executable(day12 src/day12.cpp)

find_library(FMT fmt)
target_link_libraries(day12 ${FMT})
