cmake_minimum_required(VERSION 3.0)

project(iter VERSION 0.1)

set(CMAKE_CXX_STANDARD 20)

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

include_directories(include)
include_directories(extern/extend/include)

add_compile_options(
  "-Wall" "-Wpedantic" "-Wextra"
  "$<$<CONFIG:RELEASE>:-Ofast>"
)

add_executable(
  test_iter
  test/main.cpp
  )
