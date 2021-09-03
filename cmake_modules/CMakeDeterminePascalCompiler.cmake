# Determine the compiler to use for Pascal programs
# NOTE, a generator may set CMAKE_Pascal_COMPILER before
# loading this file to force a compiler.
# use environment variable Pascal first if defined by user, next use
# the cmake variable CMAKE_GENERATOR_PASCAL which can be defined by a generator
# as a default compiler

# NOTE: on Darwin cmake >= 2.8.11 until cmake <= 2.8.12.1 will add an incompatible
# -F flag to <FLAGS> so you won't be able to use those versions with this script

if(NOT CMAKE_Pascal_COMPILER)
    # prefer the environment variable FPC
    if($ENV{FPC} MATCHES ".+")
        get_filename_component(CMAKE_Pascal_COMPILER_INIT $ENV{FPC} PROGRAM PROGRAM_ARGS CMAKE_Pascal_FLAGS_ENV_INIT)
        if(CMAKE_Pascal_FLAGS_ENV_INIT)
            set(CMAKE_Pascal_COMPILER_ARG1 "${CMAKE_Pascal_FLAGS_ENV_INIT}" CACHE STRING "First argument to Pascal compiler")
        endif(CMAKE_Pascal_FLAGS_ENV_INIT)
        if(EXISTS ${CMAKE_Pascal_COMPILER_INIT})
        else(EXISTS ${CMAKE_Pascal_COMPILER_INIT})
            message(FATAL_ERROR "Could not find compiler set in environment variable FPC:\n$ENV{FPC}.")
        endif(EXISTS ${CMAKE_Pascal_COMPILER_INIT})
    endif($ENV{FPC} MATCHES ".+")

    # next try prefer the compiler specified by the generator
    if(CMAKE_GENERATOR_PASCAL)
        if(NOT CMAKE_Pascal_COMPILER_INIT)
            set(CMAKE_Pascal_COMPILER_INIT ${CMAKE_GENERATOR_PASCAL})
        endif(NOT CMAKE_Pascal_COMPILER_INIT)
    endif(CMAKE_GENERATOR_PASCAL)

    # finally list compilers to try
    if(CMAKE_Pascal_COMPILER_INIT)
        set(CMAKE_Pascal_COMPILER_LIST ${CMAKE_Pascal_COMPILER_INIT})
    else(CMAKE_Pascal_COMPILER_INIT)
        set(CMAKE_Pascal_COMPILER_LIST fpc)
    endif(CMAKE_Pascal_COMPILER_INIT)

    # Find the compiler.
    find_program(CMAKE_Pascal_COMPILER NAMES ${CMAKE_Pascal_COMPILER_LIST} DOC "Pascal compiler")
    if(CMAKE_Pascal_COMPILER_INIT AND NOT CMAKE_Pascal_COMPILER)
        set(CMAKE_Pascal_COMPILER "${CMAKE_Pascal_COMPILER_INIT}" CACHE FILEPATH "Pascal compiler" FORCE)
    endif(CMAKE_Pascal_COMPILER_INIT AND NOT CMAKE_Pascal_COMPILER)
endif(NOT CMAKE_Pascal_COMPILER)
mark_as_advanced(CMAKE_Pascal_COMPILER)

if(NOT CMAKE_Pascal_COMPILER_VERSION)
    execute_process(COMMAND ${CMAKE_Pascal_COMPILER} -iV
                    OUTPUT_VARIABLE CMAKE_Pascal_COMPILER_VERSION
                    OUTPUT_STRIP_TRAILING_WHITESPACE
                    ) # we assume no error for something so simple
    set(CMAKE_Pascal_COMPILER_ARG1 "-l-")
endif(NOT CMAKE_Pascal_COMPILER_VERSION)
mark_as_advanced(CMAKE_Pascal_COMPILER_VERSION)

get_filename_component(COMPILER_LOCATION "${CMAKE_Pascal_COMPILER}" PATH)

# configure variables set in this file for fast reload later on
if(${CMAKE_VERSION} VERSION_LESS 2.8.10)
    configure_file(${CMAKE_MODULE_PATH}/CMakePascalCompiler.cmake.in
                   "${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/CMakePascalCompiler.cmake"
                   IMMEDIATE )
else(${CMAKE_VERSION} VERSION_LESS 2.8.10)
    configure_file(${CMAKE_MODULE_PATH}/CMakePascalCompiler.cmake.in
                  "${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/${CMAKE_VERSION}/CMakePascalCompiler.cmake"
                   IMMEDIATE )
endif(${CMAKE_VERSION} VERSION_LESS 2.8.10)

set(CMAKE_Pascal_COMPILER_ENV_VAR "FPC")

