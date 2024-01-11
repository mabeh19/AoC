cmake_minimum_required(VERSION 3.27)
project(day20)
add_executable(day20 src/day20.cpp)
find_library(fmt FMT)
target_link_libraries(day20 ${FMT})
add_custom_target(run
  COMMAND "./day20"
  DEPENDS "./day20")
