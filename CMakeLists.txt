project(soldat)

cmake_minimum_required(VERSION 3.14)
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake_modules")
include(${CMAKE_MODULE_PATH}/utils.cmake)

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "No build type selected, default to Debug")
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Build type (default Debug)" FORCE)
endif()

set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
# This value is passed to fpc as -FU flag.
# See https://www.freepascal.org/docs-html/user/userap1.html
set(OBJECT_FILES_DIRECTORY ${PROJECT_BINARY_DIR}/obj)
file(MAKE_DIRECTORY ${OBJECT_FILES_DIRECTORY})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

#set(CMAKE_VERBOSE_MAKEFILE on)

# As of my understanding, this should execute CMakePascalInformation.cmake,
# which sets CMake variables to enable integration with fpc.
enable_language(Pascal)
if(${CMAKE_Pascal_COMPILER_VERSION} VERSION_LESS 3.0)
  message(
    FATAL_ERROR
      "Your FreePascal installation is too old (fpc ${CMAKE_Pascal_COMPILER_VERSION})!"
  )
endif()

# Set fpc flags that will be shared between client and server.
# Note that we also inherit fpc flags from CMakePascalInformation module.
add_flag_append(CMAKE_Pascal_FLAGS "-l -B -MDelphi -Scgi")
add_flag_append(CMAKE_Pascal_FLAGS_DEBUG
                "-O1 -g -gl -l -vewnhiq -dDEBUG -dDEVELOPMENT")
add_flag_append(CMAKE_Pascal_FLAGS_RELEASE
                "-O3 -CX -OoREGVAR -Xs -XX -vewnhiq -dTESTING")

# Hide fgl inlining warnings
if(${CMAKE_Pascal_COMPILER_VERSION} VERSION_GREATER 3.1)
  add_flag_append(CMAKE_Pascal_FLAGS "-vm6058")
endif()
add_flag_append(CMAKE_Pascal_FLAGS "-vm3123 -vm3124")

# Define shared unit search paths
add_flag_prepend(CMAKE_Pascal_FLAGS
                 "-Fi${CMAKE_SOURCE_DIR}/3rdparty/Indy10/Lib/Core")
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fi${CMAKE_SOURCE_DIR}/shared")

add_flag_prepend(CMAKE_Pascal_FLAGS
                 "-Fu${CMAKE_SOURCE_DIR}/3rdparty/Pascal-SDL-2-Headers")
add_flag_prepend(CMAKE_Pascal_FLAGS
                 "-Fu${CMAKE_SOURCE_DIR}/3rdparty/Indy10/Lib/System")
add_flag_prepend(CMAKE_Pascal_FLAGS
                 "-Fu${CMAKE_SOURCE_DIR}/3rdparty/Indy10/Lib/Core")
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/shared")
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/shared/libs")
add_flag_prepend(CMAKE_Pascal_FLAGS
                 "-Fu${CMAKE_SOURCE_DIR}/shared/libs/GameNetworkingSockets")
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/shared/libs/PhysFS")
add_flag_prepend(CMAKE_Pascal_FLAGS
                 "-Fu${CMAKE_SOURCE_DIR}/shared/libs/SteamWrapper")
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/shared/mechanics")
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/shared/network")

# generic folder where our libraries reside
add_flag_append(CMAKE_Pascal_FLAGS "-Fl${LIBRARY_OUTPUT_PATH}")

option(CROSS_LINUX_64 "Cross compile to Linux 64bit" False)
if(CROSS_LINUX_64)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-Tlinux -Px86_64")
endif()

option(CROSS_LINUX_32 "Cross compile to Linux 32bit" False)
if(CROSS_LINUX_32)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-Tlinux -Pi386")
endif()

option(CROSS_WINDOWS_64 "Cross compile to Windows x86_64" False)
if(CROSS_WINDOWS_64)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-Twin64 -Px86_64")
endif()

option(CROSS_WINDOWS_32 "Cross compile to Windows i386" False)
if(CROSS_WINDOWS_32)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-Twin32 -Pi386")
endif()

option(CROSS_DARWIN_64 "Cross compile to macOS x86_64" False)
if(CROSS_DARWIN_64)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-Tdarwin -Px86_64")
endif()

option(CROSS_DARWIN_32 "Cross compile to macOS i386" False)
if(CROSS_DARWIN_32)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-Tdarwin -Pi386")
endif()

if(BUILD_STEAM)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-dSTEAM")
endif()

# Other dependencies shared between client and server.
# Find PhysFS
if(NOT PHYSFS_LIBRARY OR NOT PHYSFS_INCLUDE_DIR)
  find_package(PhysFS REQUIRED)
endif()

option(BUILD_SMOD "Build soldat.smod" True)
if(BUILD_SMOD)
  smod_builder()
endif()

option(BUILD_STEAM "Build steam version" False)
if(BUILD_STEAM)
  # Create steam_appid.txt
  file(WRITE ${PROJECT_BINARY_DIR}/bin/steam_appid.txt "638490")
else()
  # GameNetworkingSockets
  add_subdirectory(shared/libs/GameNetworkingSockets)
endif()

# Install options.
if(APPLE)
  set(target_library_install_dir "../Frameworks/")
  set(CMAKE_INSTALL_PREFIX "Soldat.app/Contents/MacOS/")
  set(CMAKE_INSTALL_RPATH "@executable_path/../Frameworks")
  set(CMAKE_BUILD_WITH_INSTALL_NAME_DIR TRUE)
  set(CMAKE_INSTALL_NAME_DIR "@executable_path/../Frameworks")
endif()

if(UNIX)
  set(CMAKE_INSTALL_RPATH
      "$ORIGIN/../${target_library_install_dir}/:$ORIGIN/:${CMAKE_INSTALL_PREFIX}/${target_library_install_dir}/"
  )
endif()

set(CMAKE_SKIP_BUILD_RPATH FALSE)
set(CMAKE_SKIP_INSTALL_RPATH FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH FALSE)

option(BUILD_CLIENT "Build client" True)
if(BUILD_CLIENT)
  add_subdirectory(client)
endif()

option(BUILD_SERVER "Build server" True)
if(BUILD_SERVER)
  add_subdirectory(server)
endif()
