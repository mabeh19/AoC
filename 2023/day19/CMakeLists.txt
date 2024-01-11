cmake_minimum_required(VERSION 3.27)
project(day19)
add_executable(day19 src/day19.cpp)
find_library(fmt FMT)
target_link_libraries(day19 ${FMT})
add_custom_target(run
  COMMAND "./day19"
  DEPENDS "./day19")
