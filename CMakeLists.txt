project(soldat)

option(ADD_ASSETS "Include soldat.smod and font file" True)
option(BUILD_CLIENT "Build client" True)
option(BUILD_SERVER "Build server" True)
option(BUILD_STEAM "Build steam version" False)
option(CROSS_LINUX_64 "Cross compile to Linux 64bit" False)
option(CROSS_LINUX_32 "Cross compile to Linux 32bit" False)
option(CROSS_WINDOWS_64 "Cross compile to Windows x86_64" False)
option(CROSS_WINDOWS_32 "Cross compile to Windows i386" False)
option(CROSS_DARWIN_64 "Cross compile to macOS x86_64" False)
option(CROSS_DARWIN_32 "Cross compile to macOS i386" False)

cmake_minimum_required(VERSION 3.14)
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake_modules")
include(${CMAKE_MODULE_PATH}/utils.cmake)

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "No build type selected, default to Debug")
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Build type (default Debug)" FORCE)
endif()

set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# On Unices other than Mac, use GNUInstallDirs for an easier packaging process.
if (UNIX AND NOT APPLE)
  include(GNUInstallDirs)
  set(target_full_library_install_dir "${CMAKE_INSTALL_FULL_LIBDIR}")
  set(target_library_install_dir "${CMAKE_INSTALL_LIBDIR}")
  set(target_full_binary_install_dir "${CMAKE_INSTALL_FULL_BINDIR}")
  set(target_binary_install_dir "${CMAKE_INSTALL_BINDIR}")
  set(target_full_data_install_dir "${CMAKE_INSTALL_FULL_DATADIR}")
  set(target_data_install_dir "${CMAKE_INSTALL_DATADIR}")
else()
  set(target_full_library_install_dir "${CMAKE_INSTALL_PREFIX}")
  set(target_library_install_dir ".")
  set(target_full_binary_install_dir "${CMAKE_INSTALL_PREFIX}")
  set(target_binary_install_dir ".")
  set(target_full_data_install_dir "${CMAKE_INSTALL_PREFIX}")
  set(target_data_install_dir ".")
endif()

#set(CMAKE_VERBOSE_MAKEFILE on)

# RPATH settings. This needs to be set before executing
# enable_language(Pascal). Those settings will make sure
# our binaries can find dynamic libraries. It's only necessary
# on UNIX, will be ignored elsewhere.
if(UNIX)
  if(APPLE)
    set(target_full_library_install_dir "${CMAKE_INSTALL_PREFIX}/../Frameworks/")
    set(target_library_install_dir "../Frameworks/")
    #set(CMAKE_INSTALL_PREFIX "Soldat.app/Contents/MacOS/")
    set(CMAKE_INSTALL_RPATH "@executable_path/../Frameworks")
    set(CMAKE_BUILD_WITH_INSTALL_NAME_DIR TRUE)
    set(CMAKE_INSTALL_NAME_DIR "@executable_path/../Frameworks")
    set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
  else()
    set(CMAKE_INSTALL_RPATH
      "$ORIGIN/../${target_library_install_dir}/:$ORIGIN/:${target_full_library_install_dir}"
    )
    set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
  endif()

  set(CMAKE_SKIP_BUILD_RPATH FALSE)
  set(CMAKE_SKIP_INSTALL_RPATH FALSE)
  set(CMAKE_INSTALL_RPATH_USE_LINK_PATH FALSE)
endif()

# As of my understanding, this should execute CMakePascalInformation.cmake,
# which sets CMake variables to enable integration with fpc.
# The above RPATH settings will be respected.
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
                "-O1 -g -gl -l -vewnhiq -dDEVELOPMENT")
add_flag_append(CMAKE_Pascal_FLAGS_RELEASE
                "-O3 -CX -OoREGVAR -Xs -XX -vewnhiq -dTESTING")

# Hide fgl inlining warnings
if(${CMAKE_Pascal_COMPILER_VERSION} VERSION_GREATER 3.1)
  add_flag_append(CMAKE_Pascal_FLAGS "-vm6058")
endif()
add_flag_append(CMAKE_Pascal_FLAGS "-vm3123 -vm3124")

# Define shared unit search paths
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fi${CMAKE_SOURCE_DIR}/shared")

add_flag_prepend(CMAKE_Pascal_FLAGS
                 "-Fu${CMAKE_SOURCE_DIR}/3rdparty/Pascal-SDL-2-Headers")
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

if(CROSS_LINUX_64)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-Tlinux -Px86_64")
endif()

if(CROSS_LINUX_32)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-Tlinux -Pi386")
endif()

if(CROSS_WINDOWS_64)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-Twin64 -Px86_64")
endif()

if(CROSS_WINDOWS_32)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-Twin32 -Pi386")
endif()

if(CROSS_DARWIN_64)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-Tdarwin -Px86_64")
endif()

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

if(ADD_ASSETS)
  include(${CMAKE_MODULE_PATH}/SoldatAssets.cmake)
  download_assets()
endif()

if(BUILD_CLIENT OR BUILD_SERVER)
  if(BUILD_STEAM)
    # Create steam_appid.txt
    file(WRITE ${PROJECT_BINARY_DIR}/bin/steam_appid.txt "638490")
  else()
    # GameNetworkingSockets
    add_subdirectory(shared/libs/GameNetworkingSockets)
  endif()
endif()

if(BUILD_CLIENT)
  add_subdirectory(client)
endif()

if(BUILD_SERVER)
  add_subdirectory(server)
endif()

# Install step that does 3 things:
# 1) Cleans binary directory by removing unnecessary files from compilation.
# 2) Resets file permissions (for example removes executable flag from shared
#    libraries on linux).
# 3) Copies cleaned directory to path specified by CMAKE_INSTALL_PREFIX.
#
# It's likely not the best approach, but should get the job done.
# CMake v3.16 has file(GET_RUNTIME_DEPENDENCIES), which would probably be
# more suitable for this. That approach comes with its own set of limitations though.
# For instance, .smod dependency will most likely not be detected. Also, we would
# probably need to filter out some dependencies manually, as we probably don't want
# to provide all .so files on linux - users can get those easily from package managers.
install(
  DIRECTORY ${EXECUTABLE_OUTPUT_PATH}/
  DESTINATION ${target_full_library_install_dir}
  FILES_MATCHING
  PATTERN "*.sh" EXCLUDE
  PATTERN "*.bat" EXCLUDE
  PATTERN "*.res" EXCLUDE
  PATTERN "*.smod" EXCLUDE
  PATTERN "*.dll"
  PATTERN "*.so"
  PATTERN "*.dylib"
  # Ignore subdirectories, just in case... it's not necessary on clean build,
  # only if you ran client/server executables before installing.
  REGEX "configs|demos|logs|maps|mods|screens" EXCLUDE
)

if(ADD_ASSETS)
  install(
    DIRECTORY ${EXECUTABLE_OUTPUT_PATH}/
    # FIXME: should be target_full_data_install_dir, but unsupported by soldat
    # (needs wrapper script or some equivalent global config)
    DESTINATION ${target_full_binary_install_dir}
    FILES_MATCHING
    PATTERN "*.smod"
  )
endif()
