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
set (JSONFORTRAN_ENABLE_TESTS OFF)
FetchContent_MakeAvailable(json-fortran)

add_executable(test-molpro-keyval-fortran main.f90 testapp.f90 molpro_input.f90)
target_link_libraries(test-molpro-keyval-fortran PRIVATE Fortuno::fortuno_serial jsonfortran)

enable_testing()
add_test(NAME test-molpro-keyval-fortran COMMAND test-molpro-keyval-fortran)