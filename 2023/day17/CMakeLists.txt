cmake_minimum_required(VERSION 3.27)
project(day17)
add_executable(day17 src/day17.cpp)
find_library(FMT fmt)
target_link_libraries(day17 ${FMT})
add_custom_target(run
  COMMAND "./day17"
  DEPENDS "./day17")
