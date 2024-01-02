cmake_minimum_required(VERSION 3.27)
project(day14)
add_executable(day14 src/day14.cpp)
find_library(FMT fmt)
target_link_libraries(day14 ${FMT})
add_custom_target(run 
  COMMAND "./day14"
  DEPENDS "./day14"
)
