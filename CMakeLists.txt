cmake_minimum_required(VERSION 3.0...4.0)
project(molpro-keyval-fortran LANGUAGES Fortran)


include(FetchContent)
FetchContent_Declare(
        Fortuno
        GIT_REPOSITORY "https://github.com/fortuno-repos/fortuno"
        GIT_TAG "main"
)
FetchContent_MakeAvailable(Fortuno)

FetchContent_Declare(json-fortran
        GIT_REPOSITORY https://github.com/jacobwilliams/json-fortran
        GIT_TAG "9.0.5")
set(JSONFORTRAN_ENABLE_TESTS OFF)
FetchContent_MakeAvailable(json-fortran)

add_library(molpro_input molpro_input.f90)
target_link_libraries(molpro_input PUBLIC jsonfortran-static)

add_executable(test_molpro_input test_molpro_input_main.f90 test_molpro_input.f90)
target_link_libraries(test_molpro_input PRIVATE Fortuno::fortuno_serial molpro_input jsonfortran)

add_executable(run_molpro_input run_molpro_input_main.f90)
target_link_libraries(run_molpro_input PRIVATE molpro_input jsonfortran)


enable_testing()
add_test(NAME test_molpro_input COMMAND test_molpro_input)
file(COPY molpro_input.json DESTINATION .)