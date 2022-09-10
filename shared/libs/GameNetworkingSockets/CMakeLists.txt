cmake_minimum_required(VERSION 3.14)
include(FetchContent)

MESSAGE(STATUS "***  GameNetworkingSockets -- START --")

#############################################################
## Settings
#############################################################
## 0. Make sure you have installed vcpkg and installed openssl and protobuf
    ## Optionally install bcrypt as well
## 1. vcpkg toolchain file:
    # Set in command line like shown below or with CMakeSettings.json or in cmake-gui
    # e.g.: -DCMAKE_TOOLCHAIN_FILE="C:/vcpkg/scripts/buildsystems/vcpkg.cmake"
## 2. Set the path to your vcpkg installation
SET(VCPKG_ROOT "C:/vcpkg" CACHE PATH "Path to your vcpkg installation." FORCE) 
## 3. Set Directories
# Project Root
SET(PROJECT_ROOT ${CMAKE_CURRENT_LIST_DIR}/build)
# local install - change to your liking
SET(GNS_INSTALL_DIR "${PROJECT_ROOT}/install/${CMAKE_BUILD_TYPE}" CACHE PATH "The base path for your installation" FORCE )
#############################################################
## Project Definition
#############################################################
project(GameNetworkingSockets)

## protocol Buffers Settings
# protobuf recognition is not working with the normal find_package mechanics
# We need to provide everything manually
#IF (WIN32)
#SET(Protobuf_INCLUDE_DIR          "${VCPKG_ROOT}/installed/x64-windows/include")                        # The include directory for protocol buffers
#SET(Protobuf_PROTOC_EXECUTABLE    "${VCPKG_ROOT}/installed/x64-windows/tools/protobuf/protoc.exe")      # The protoc compiler
#SET(Protobuf_LIBRARY              "${VCPKG_ROOT}/installed/x64-windows/lib/libprotobuf.lib")            # The protobuf library
#SET(Protobuf_LIBRARY_DEBUG        "${VCPKG_ROOT}/installed/x64-windows/lib/debug/libprotobufd.lib")     # The protobuf library (debug)
#SET(Protobuf_PROTOC_LIBRARY       "${VCPKG_ROOT}/installed/x64-windows/lib/libprotoc.lib")              # The protoc library
#SET(Protobuf_PROTOC_LIBRARY_DEBUG "${VCPKG_ROOT}/installed/x64-windows/lib/debug/libprotocd.lib")       # The protoc library (debug)
#SET(Protobuf_LITE_LIBRARY         "${VCPKG_ROOT}/installed/x64-windows/lib/libprotobuf-lite.lib")       # The protobuf lite library
#SET(Protobuf_LITE_LIBRARY_DEBUG   "${VCPKG_ROOT}/installed/x64-windows/libdebug/libprotobuf-lited.lib") # The protobuf lite library (debug)
## OpenSSL Recognition with vcpkg works nicely
#ENDIF()
# download settings
SET(GNS_SOURCE_DIR "${PROJECT_ROOT}/gns_download" )
SET(GNS_TAG "v1.4.1" )
SET(GNS_URL "https://github.com/ValveSoftware/GameNetworkingSockets/archive/refs/tags/${GNS_TAG}.tar.gz" )

#############################################################
## Fetch and build GNS
#############################################################

FetchContent_Declare(
    gamenetworkingsockets
    URL ${GNS_URL}
    URL_HASH SHA1=06d71f742b97d997c2d41ade21a3e9c4537acf23
    USES_TERMINAL_DOWNLOAD TRUE
    SOURCE_DIR     ${GNS_SOURCE_DIR}
)

FetchContent_Populate(gamenetworkingsockets)

option(GAMENETWORKINGSOCKETS_BUILD_EXAMPLES OFF)
option(GAMENETWORKINGSOCKETS_BUILD_TESTS OFF)

ADD_SUBDIRECTORY( ${GNS_SOURCE_DIR} EXCLUDE_FROM_ALL)
ADD_DEPENDENCIES(GameNetworkingSockets GameNetworkingSockets)
##############################################################
## Install
##############################################################

# Commented out, because we don't really need this step when installing
# OpenSoldat. Reenable this if you would like to export GNS libraries to a
# custom destination.
#install(TARGETS
#    GameNetworkingSockets

#    DESTINATION ${GNS_INSTALL_DIR}
#    RUNTIME  DESTINATION ${GNS_INSTALL_DIR}/bin
#    LIBRARY  DESTINATION ${GNS_INSTALL_DIR}/lib
#    ARCHIVE  DESTINATION ${GNS_INSTALL_DIR}/lib
#    INCLUDES DESTINATION ${GNS_INSTALL_DIR}/include
#)

MESSAGE(STATUS "***  GameNetworkingSockets -- DONE --")
