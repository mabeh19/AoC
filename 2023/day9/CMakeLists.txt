cmake_minimum_required(VERSION 3.27)

project(day9)

add_executable(day9 src/day9.cpp)

find_library(FMT fmt)

target_link_libraries(day9 ${FMT})
