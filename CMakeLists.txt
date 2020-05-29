project(soldat)

cmake_minimum_required(VERSION 3.14)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake_modules")
include(${CMAKE_MODULE_PATH}/utils.cmake)

set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

#set(CMAKE_VERBOSE_MAKEFILE on)

option(BUILD_SMOD "Build soldat.smod" True)

if(BUILD_SMOD)
  smod_builder()
endif()

# GameNetworkingSockets
add_subdirectory(shared/libs/GameNetworkingSockets)

# Find PhysFS
if(NOT PHYSFS_LIBRARY OR NOT PHYSFS_INCLUDE_DIR)
  find_package(PhysFS REQUIRED)
endif()

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

option(BUILD_STEAM "Build steam version" False)
if(BUILD_STEAM)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-dSTEAM")
endif()

option(BUILD_CLIENT "Build client" True)
if(BUILD_CLIENT)
  add_subdirectory(client)
endif()

option(BUILD_SERVER "Build server" True)
if(BUILD_SERVER)
  add_subdirectory(server)
endif()
