cmake_minimum_required(VERSION 3.18)
enable_language(Fortran)

project(fortran-cmake)
set(EXECUTABLE main)

add_executable(main
        main.f90
        src/math.f90
)
