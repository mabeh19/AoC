cmake_minimum_required(VERSION 3.27)

project(day10)

add_executable(day10 src/day10.cpp)

find_library(FMT fmt)

target_link_libraries(day10 ${FMT})
