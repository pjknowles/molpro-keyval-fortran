cmake_minimum_required(VERSION 3.0...4.0)
project(molpro-keyval-fortran LANGUAGES Fortran)

include(FetchContent)
FetchContent_Declare(json-fortran
        GIT_REPOSITORY https://github.com/jacobwilliams/json-fortran
        GIT_TAG "9.0.5")
set(JSONFORTRAN_ENABLE_TESTS OFF)
FetchContent_MakeAvailable(json-fortran)

add_library(molpro_input molpro_input.f90)
target_link_libraries(molpro_input PUBLIC jsonfortran-static)
file(COPY molpro_input.json DESTINATION .)

add_executable(run_molpro_input run_molpro_input_main.f90)
target_link_libraries(run_molpro_input PRIVATE molpro_input)


FetchContent_Declare(
        Fortuno
        GIT_REPOSITORY "https://github.com/fortuno-repos/fortuno"
        GIT_TAG "main"
)
FetchContent_MakeAvailable(Fortuno)
add_executable(test_molpro_input test_molpro_input_main.f90 test_molpro_input.f90)
target_link_libraries(test_molpro_input PRIVATE Fortuno::fortuno_serial molpro_input)
enable_testing()
add_test(NAME test_molpro_input COMMAND test_molpro_input)

#enable_language(CXX)
#set(CMAKE_CXX_STANDARD 20)
#FetchContent_Declare(
#        blaze
#        URL https://github.com/sourcemeta/blaze/archive/refs/tags/v1.0.0.zip
#        DOWNLOAD_EXTRACT_TIMESTAMP NO)
#FetchContent_MakeAvailable(blaze)
#
#target_link_libraries(molpro_input PUBLIC sourcemeta::blaze::evaluator)
##target_link_libraries(my_example PUBLIC sourcemeta::blaze::compiler)

#FetchContent_Declare(json-schema-validator
#GIT_REPOSITORY https://github.com/pboettch/json-schema-validator.git
#)
#FetchContent_MakeAvailable(json-schema-validator)
#target_link_libraries(molpro_input PUBLIC nlohmann_json_schema_validator)

#FetchContent_Declare(valijson
#        GIT_REPOSITORY https://github.com/tristanpenman/valijson.git
#)
#FetchContent_MakeAvailable(valijson)
#target_link_libraries(molpro_input PUBLIC valijson)