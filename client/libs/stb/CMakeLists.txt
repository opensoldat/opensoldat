cmake_minimum_required(VERSION 3.2)

project(stb)

set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
add_library(stb SHARED src/stb.c)