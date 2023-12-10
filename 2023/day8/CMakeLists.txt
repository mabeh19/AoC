cmake_minimum_required(VERSION 3.27)

set(CMAKE_BUILD_TYPE Release)

project(day8)

add_executable(day8 src/day8.cpp)

find_library(FMT fmt)

target_link_libraries(day8 ${FMT})
