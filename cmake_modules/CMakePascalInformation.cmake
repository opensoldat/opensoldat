# This file sets the basic flags for the Pascal language in CMake.
# It also loads the available platform file for the system-compiler
# if it exists.

# in case fpc ever becomes included in cmake
get_filename_component(CMAKE_BASE_NAME ${CMAKE_Pascal_COMPILER} NAME_WE)
set(CMAKE_SYSTEM_AND_Pascal_COMPILER_INFO_FILE
    ${CMAKE_ROOT}/Modules/Platform/${CMAKE_SYSTEM_NAME}-${CMAKE_BASE_NAME}.cmake)
include(Platform/${CMAKE_SYSTEM_NAME}-${CMAKE_BASE_NAME} OPTIONAL)

# This section should actually be in Platform/${CMAKE_SYSTME_NAME}-fpc.cmake
set(CMAKE_Pascal_FLAGS_DEBUG_INIT "-O- -g -gl -gp -gh")
set(CMAKE_Pascal_FLAGS_MINSIZEREL_INIT "-Os -dNDEBUG")
set(CMAKE_Pascal_FLAGS_RELEASE_INIT "-O2 -dNDEBUG")
set(CMAKE_Pascal_FLAGS_RELWITHDEBINFO_INIT "-O2 -g -gl -gp")

# This should be included before the _INIT variables are
# used to initialize the cache. Since the rule variables
# have if blocks on them, users can still define them here.
# But, it should still be after the platform file so changes can
# be made to those values.

if(CMAKE_USER_MAKE_RULES_OVERRIDE)
   include(${CMAKE_USER_MAKE_RULES_OVERRIDE})
endif(CMAKE_USER_MAKE_RULES_OVERRIDE)

if(CMAKE_USER_MAKE_RULES_OVERRIDE_Pascal)
   include(${CMAKE_USER_MAKE_RULES_OVERRIDE_Pascal})
endif(CMAKE_USER_MAKE_RULES_OVERRIDE_Pascal)

# Create a set of shared library variable specific to Pascal
# For 90% of the systems, these are the same flags as the C versions
# so if these are not set just copy the flags from the c version

# No flags supported during linking as a shell script takes care of it
# however to avoid interferences we escape -Wl flags to the Pascal -k
#if(NOT CMAKE_SHARED_LIBRARY_CREATE_Pascal_FLAGS)
#-shared (linux) / -dynamiclib -Wl,-headerpad_max_install_names (darwin)
#    string(REGEX REPLACE "-Wl," "-k" CMAKE_SHARED_LIBRARY_CREATE_Pascal_FLAGS ${CMAKE_SHARED_LIBRARY_CREATE_C_FLAGS})
#endif(NOT CMAKE_SHARED_LIBRARY_CREATE_Pascal_FLAGS)

if(NOT CMAKE_SHARED_LIBRARY_Pascal_FLAGS AND CMAKE_SHARED_LIBRARY_C_FLAGS)
#-fPIC
    string(REGEX REPLACE "-Wl," "-k" CMAKE_SHARED_LIBRARY_Pascal_FLAGS ${CMAKE_SHARED_LIBRARY_C_FLAGS})
endif()

if(NOT CMAKE_SHARED_LIBRARY_LINK_Pascal_FLAGS AND CMAKE_SHARED_LIBRARY_LINK_C_FLAGS)
#-rdynamic (linux) / (empty on darwin)
    string(REGEX REPLACE "-Wl," "-k" CMAKE_SHARED_LIBRARY_LINK_Pascal_FLAGS ${CMAKE_SHARED_LIBRARY_LINK_C_FLAGS})
endif()

if(NOT CMAKE_SHARED_LIBRARY_RUNTIME_Pascal_FLAG)
#-Wl,-rpath,
    set(CMAKE_SHARED_LIBRARY_RUNTIME_Pascal_FLAG "-k-rpath")
endif(NOT CMAKE_SHARED_LIBRARY_RUNTIME_Pascal_FLAG)

if(NOT CMAKE_SHARED_LIBRARY_RUNTIME_Pascal_FLAG_SEP)
    set(CMAKE_SHARED_LIBRARY_RUNTIME_Pascal_FLAG_SEP ${CMAKE_SHARED_LIBRARY_RUNTIME_C_FLAG_SEP})
endif(NOT CMAKE_SHARED_LIBRARY_RUNTIME_Pascal_FLAG_SEP)

if(NOT CMAKE_SHARED_LIBRARY_RPATH_LINK_Pascal_FLAG)
#-Wl,-rpath-link,
    set(CMAKE_SHARED_LIBRARY_RPATH_LINK_Pascal_FLAG "-k-rpath-link")
endif(NOT CMAKE_SHARED_LIBRARY_RPATH_LINK_Pascal_FLAG)

# for most systems a module is the same as a shared library
# so unless the variable CMAKE_MODULE_EXISTS is set just
# copy the values from the LIBRARY variables
if(NOT CMAKE_MODULE_EXISTS)
    set(CMAKE_SHARED_MODULE_Pascal_FLAGS ${CMAKE_SHARED_LIBRARY_Pascal_FLAGS})
    set(CMAKE_SHARED_MODULE_CREATE_Pascal_FLAGS ${CMAKE_SHARED_LIBRARY_CREATE_Pascal_FLAGS})
endif(NOT CMAKE_MODULE_EXISTS)

# repeat for modules
if(NOT CMAKE_SHARED_MODULE_CREATE_Pascal_FLAGS AND CMAKE_SHARED_MODULE_CREATE_C_FLAGS)
# ? (linux) / -bundle -Wl,-headerpad_max_install_names (darwin)
    string(REGEX REPLACE "-Wl," "-k" CMAKE_SHARED_MODULE_CREATE_Pascal_FLAGS ${CMAKE_SHARED_MODULE_CREATE_C_FLAGS})
endif()

if(NOT CMAKE_SHARED_MODULE_Pascal_FLAGS AND CMAKE_SHARED_MODULE_C_FLAGS)
    string(REGEX REPLACE "-Wl," "-k" CMAKE_SHARED_MODULE_Pascal_FLAGS ${CMAKE_SHARED_MODULE_C_FLAGS})
endif()

if(NOT CMAKE_SHARED_MODULE_RUNTIME_Pascal_FLAG)
    set(CMAKE_SHARED_MODULE_RUNTIME_Pascal_FLAG ${CMAKE_SHARED_MODULE_RUNTIME_C_FLAG})
endif(NOT CMAKE_SHARED_MODULE_RUNTIME_Pascal_FLAG)

if(NOT CMAKE_SHARED_MODULE_RUNTIME_Pascal_FLAG_SEP)
    set(CMAKE_SHARED_MODULE_RUNTIME_Pascal_FLAG_SEP ${CMAKE_SHARED_MODULE_RUNTIME_C_FLAG_SEP})
endif(NOT CMAKE_SHARED_MODULE_RUNTIME_Pascal_FLAG_SEP)

# now other system things
if(NOT CMAKE_INCLUDE_FLAG_Pascal)
    #amazing, fpc: -I<x>  Add <x> to include path
    set(CMAKE_INCLUDE_FLAG_Pascal ${CMAKE_INCLUDE_FLAG_C})
endif(NOT CMAKE_INCLUDE_FLAG_Pascal)

if(NOT CMAKE_INCLUDE_FLAG_SEP_Pascal)
    set(CMAKE_INCLUDE_FLAG_SEP_Pascal ${CMAKE_INCLUDE_FLAG_SEP_C})
endif(NOT CMAKE_INCLUDE_FLAG_SEP_Pascal)

if(NOT CMAKE_Pascal_FRAMEWORK_SEARCH_FLAG)
    #however -F won't work, -Ff is Pascal equivalent
    set(CMAKE_Pascal_FRAMEWORK_SEARCH_FLAG "-Ff")
endif(NOT CMAKE_Pascal_FRAMEWORK_SEARCH_FLAG)

# Copy C version of this flag which is normally determined in platform file.
if(NOT CMAKE_SHARED_LIBRARY_SONAME_Pascal_FLAG)
#-soname (linux) / -install-name (dawin)
    set(CMAKE_SHARED_LIBRARY_SONAME_Pascal_FLAG ${CMAKE_SHARED_LIBRARY_SONAME_C_FLAG})
endif(NOT CMAKE_SHARED_LIBRARY_SONAME_Pascal_FLAG)

set(CMAKE_VERBOSE_MAKEFILE FALSE CACHE BOOL "If this value is on, makefiles will be generated without the .SILENT directive, and all commands will be echoed to the console during the make.  This is useful for debugging only. With Visual Studio IDE projects all commands are done without /nologo.")

#set(CMAKE_Pascal_FLAGS "$ENV{FPFLAGS} ${CMAKE_Pascal_FLAGS_INIT} ${CMAKE_Pascal_FLAGS}" CACHE STRING "Flags for Pascal compiler." FORCE)

include(CMakeCommonLanguageInclude)

# now define the following rule variables

# CMAKE_Pascal_CREATE_SHARED_LIBRARY
# CMAKE_Pascal_CREATE_SHARED_MODULE
# CMAKE_Pascal_CREATE_STATIC_LIBRARY
# CMAKE_Pascal_COMPILE_OBJECT
# CMAKE_Pascal_LINK_EXECUTABLE

# variables supplied by the generator at use time
# <TARGET>
# <TARGET_BASE> the target without the suffix
# <OBJECTS>
# <OBJECT>
# <LINK_LIBRARIES>
# <FLAGS>
# <LINK_FLAGS>

# Pascal compiler information
# <CMAKE_Pascal_COMPILER>
# <CMAKE_SHARED_LIBRARY_CREATE_Pascal_FLAGS>
# <CMAKE_SHARED_MODULE_CREATE_Pascal_FLAGS>
# <CMAKE_Pascal_LINK_FLAGS>

# Static library tools
#  NONE!

if(NOT EXECUTABLE_OUTPUT_PATH)
    set (EXECUTABLE_OUTPUT_PATH ${CMAKE_CURRENT_BINARY_DIR})
endif(NOT EXECUTABLE_OUTPUT_PATH)

# create a Pascal shared library
if(NOT CMAKE_Pascal_CREATE_SHARED_LIBRARY)
    if(WIN32)
        file(TO_NATIVE_PATH "${EXECUTABLE_OUTPUT_PATH}/ppas.bat" CMAKE_Pascal_CREATE_SHARED_LIBRARY)
    else(WIN32)
        set(CMAKE_Pascal_CREATE_SHARED_LIBRARY "${EXECUTABLE_OUTPUT_PATH}/ppas.sh")
    endif(WIN32)
# other expandable variables here are <CMAKE_Pascal_COMPILER> <CMAKE_SHARED_LIBRARY_Pascal_FLAGS> <LANGUAGE_COMPILE_FLAGS> <LINK_FLAGS> <CMAKE_SHARED_LIBRARY_CREATE_Pascal_FLAGS> <CMAKE_SHARED_LIBRARY_SONAME_Pascal_FLAG> <TARGET_SONAME> <TARGET> <OBJECTS> <LINK_LIBRARIES>
endif(NOT CMAKE_Pascal_CREATE_SHARED_LIBRARY)

# create an Pascal shared module just copy the shared library rule
if(NOT CMAKE_Pascal_CREATE_SHARED_MODULE)
  set(CMAKE_Pascal_CREATE_SHARED_MODULE ${CMAKE_Pascal_CREATE_SHARED_LIBRARY})
endif(NOT CMAKE_Pascal_CREATE_SHARED_MODULE)

# create an Pascal static library (unsupported)
if(NOT CMAKE_Pascal_CREATE_STATIC_LIBRARY)
  set(CMAKE_Pascal_CREATE_STATIC_LIBRARY
      "echo STATIC LIBRARIES ARE NOT SUPPORTED" "exit")
endif(NOT CMAKE_Pascal_CREATE_STATIC_LIBRARY)

# compile a Pascal file into an object file
if(NOT CMAKE_Pascal_COMPILE_OBJECT)
    if(UNIX)
        #when you have multiple ld installation make sure you get the one bundled with the system C compiler
        include(Platform/${CMAKE_SYSTEM_NAME}-GNU-C.cmake OPTIONAL)
        if(CMAKE_COMPILER_IS_GNUCC)
            get_filename_component(CMAKE_C_COMPILER_DIR ${CMAKE_C_COMPILER} PATH)
            set(CMAKE_Pascal_UNIX_FLAGS "-FD${CMAKE_C_COMPILER_DIR}")
        endif(CMAKE_COMPILER_IS_GNUCC)
        if(APPLE)
            #TODO: take care of CMAKE_INSTALL_NAME_DIR for shared targets
        else(APPLE)
            if(CMAKE_INSTALL_RPATH)
                #need to escape twice because we use a script to link
                #\\\\ is just \\ which escapes '\' in the final script
                #same for $$ which escapes '$' in cmake
                string(REGEX REPLACE "\\$" "\\\\$$" CMAKE_INSTALL_RPATH_ESCAPED ${CMAKE_INSTALL_RPATH})
                #normally this flag is found in <LINK_LIBRARIES> but that's not active here
                set(CMAKE_Pascal_UNIX_FLAGS "${CMAKE_SHARED_LIBRARY_RUNTIME_Pascal_FLAG} -k'${CMAKE_INSTALL_RPATH_ESCAPED}' ${CMAKE_Pascal_UNIX_FLAGS}")
            endif()
        endif(APPLE)
    endif(UNIX)

    #-Cn is mandatory as it's what creates the ppas.* script
    set(CMAKE_Pascal_COMPILE_OBJECT
        "<CMAKE_Pascal_COMPILER> -Cn -FE${EXECUTABLE_OUTPUT_PATH} -FU${CMAKE_CURRENT_BINARY_DIR}/<OBJECT_DIR> ${CMAKE_Pascal_UNIX_FLAGS} <FLAGS> <CMAKE_Pascal_LINK_FLAGS> <SOURCE>")
endif(NOT CMAKE_Pascal_COMPILE_OBJECT)

# link Pascal objects in a single executable
if(NOT CMAKE_Pascal_LINK_EXECUTABLE)
    if(WIN32)
        file(TO_NATIVE_PATH "${EXECUTABLE_OUTPUT_PATH}/ppas.bat" CMAKE_Pascal_LINK_EXECUTABLE)
    else(WIN32)
        set(CMAKE_Pascal_LINK_EXECUTABLE "${EXECUTABLE_OUTPUT_PATH}/ppas.sh")
    endif(WIN32)
# other expandable variables here are <CMAKE_Pascal_LINK_FLAGS> <LINK_FLAGS> <TARGET_BASE> <FLAGS> <LINK_LIBRARIES>
endif(NOT CMAKE_Pascal_LINK_EXECUTABLE)

if(CMAKE_Pascal_STANDARD_LIBRARIES_INIT)
    set(CMAKE_Pascal_STANDARD_LIBRARIES "${CMAKE_Pascal_STANDARD_LIBRARIES_INIT}"
    CACHE STRING "Libraries linked by default (usually handled internally).")
    mark_as_advanced(CMAKE_Pascal_STANDARD_LIBRARIES)
endif(CMAKE_Pascal_STANDARD_LIBRARIES_INIT)

if(NOT CMAKE_NOT_USING_CONFIG_FLAGS)
    set(CMAKE_Pascal_FLAGS_DEBUG "${CMAKE_Pascal_FLAGS_DEBUG_INIT}" CACHE STRING
        "Flags used by the compiler during debug builds.")
    set(CMAKE_Pascal_FLAGS_MINSIZEREL "${CMAKE_Pascal_FLAGS_MINSIZEREL_INIT}" CACHE STRING
        "Flags used by the compiler during release minsize builds.")
    set(CMAKE_Pascal_FLAGS_RELEASE "${CMAKE_Pascal_FLAGS_RELEASE_INIT}" CACHE STRING
        "Flags used by the compiler during release builds (/MD /Ob1 /Oi /Ot /Oy /Gs will produce slightly less optimized but smaller files).")
    set(CMAKE_Pascal_FLAGS_RELWITHDEBINFO "${CMAKE_Pascal_FLAGS_RELWITHDEBINFO_INIT}" CACHE STRING
        "Flags used by the compiler during Release with Debug Info builds.")
endif(NOT CMAKE_NOT_USING_CONFIG_FLAGS)

mark_as_advanced(CMAKE_Pascal_FLAGS CMAKE_Pascal_FLAGS_DEBUG CMAKE_Pascal_FLAGS_MINSIZEREL
                 CMAKE_Pascal_FLAGS_RELEASE CMAKE_Pascal_FLAGS_RELWITHDEBINFO)
set(CMAKE_Pascal_INFORMATION_LOADED 1)

