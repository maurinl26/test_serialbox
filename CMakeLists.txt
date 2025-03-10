cmake_minimum_required(VERSION 3.22)

project(test_serialbox
    LANGUAGES
    Fortran
    C
)


set(Serialbox_INSTALL_PATH $ENV{SERIALBOX_INSTALL_PATH})
set(Serialbox_INCLUDE_DIR ${Serialbox_INSTALL_PATH}/include)
set(Serialbox_LIB_DIR ${Serialbox_INSTALL_PATH}/lib)
set(Serialbox_SRC_DIR ${Serialbox_INSTALL_PATH}/src)
set(PPSER_SOURCE ${Serialbox_INSTALL_PATH}/python/pp_ser/pp_ser.py)
message("Serialbox_INSTALL_PATH set to ${Serialbox_INSTALL_PATH}")

set(FFLAGS "-lstdc++ -DSERIALIZE")
find_package(Serialbox REQUIRED)

# Run pp_ser preprocessor
execute_process(
        COMMAND python3 ${PPSER_SOURCE} ${CMAKE_SOURCE_DIR}/test.F90 --output-dir=${CMAKE_SOURCE_DIR}/generated
)


# Executable
add_executable(testSerialbox ${CMAKE_SOURCE_DIR}/generated/test.F90)

target_link_libraries(testSerialbox 
	PUBLIC
	Serialbox::SerialboxFortranShared
)


target_compile_definitions(testSerialbox
    PUBLIC
        SERIALIZE=1
)


