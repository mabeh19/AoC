cmake_minimum_required(VERSION 3.27)
project(day16)
add_executable(day16 src/day16.cpp)
find_library(FMT fmt)
target_link_libraries(day16 ${FMT})
add_custom_target(run
  COMMAND "./day16"
  DEPENDS "./day16")
