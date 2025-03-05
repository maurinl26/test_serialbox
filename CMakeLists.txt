cmake_minimum_required(VERSION 3.22)

project(test_serialbox
    LANGUAGES
    Fortran
    C
)

set(Serialbox_ROOT $ENV{SERIALBOX_ROOT})
set(PPSER_SOURCE_DIR ${CMAKE_SOURCE_DIR}/generated)
message("Serialbox_ROOT set to ${Serialbox_ROOT}")

set(FFLAGS "-lstdc++17 -DSERIALIZE -I${Serialbox_ROOT}/include")

find_package(Serialbox REQUIRED)

# Execute preprocessor
execute_process(
    COMMAND python3 ${Serialbox_ROOT}/python/pp_ser/pp_ser.py ${CMAKE_SOURCE_DIR}/test.F90 --output-dir=${PPSER_SOURCE_DIR}
)

add_executable(test.bin ${PPSER_SOURCE_DIR}/test.F90)

target_link_libraries(test.bin
    PUBLIC
    Serialbox::SerialboxFortranStatic
)

target_compile_definitions(test.bin
    PUBLIC
        SERIALIZE=1
)

