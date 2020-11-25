cmake_minimum_required(VERSION 3.18)
enable_language(Fortran)

project(fortran-cmake)
set(EXECUTABLE main)

add_executable(main
        main.f90
        src/lib/array.f90
        src/lib/math.f90
        src/lib/main_file.f90
        src/usecase/groverQuantumSearchUsecase.f90
)
