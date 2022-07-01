if(APPLE)
  add_flag_append(CMAKE_Pascal_FLAGS "-Fl/usr/local/lib")
  add_flag_append(CMAKE_Pascal_FLAGS "-Fu./macOS/Frameworks")
  add_flag_append(CMAKE_Pascal_FLAGS "-Fl./macOS/Frameworks")
  add_flag_append(CMAKE_Pascal_FLAGS "-Fu./macOS/Frameworks")
  add_flag_append(CMAKE_Pascal_FLAGS "-k-FmacOS/Frameworks")
  add_flag_append(CMAKE_Pascal_FLAGS "-k\"-rpath\" -k\"@executable_path\"")
endif()

add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/server")
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/server/scriptcore")
add_flag_prepend(CMAKE_Pascal_FLAGS
                 "-Fu${CMAKE_SOURCE_DIR}/3rdparty/pascalscript/Source")
if(ADD_FFI_FUZZ)
  add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/server/scriptcore/test")
endif()

add_flag_append(CMAKE_Pascal_FLAGS "-dSERVER")

option(BUILD_SCRIPTCORE "Build ScriptCore" True)
option(BUILD_RCON "Build Rcon" True)

if(BUILD_SCRIPTCORE)
  add_flag_append(CMAKE_Pascal_FLAGS "-dSCRIPT")
  add_flag_append(CMAKE_Pascal_FLAGS "-dPS_USESSUPPORT")
  if(ADD_FFI_FUZZ)
    add_flag_append(CMAKE_Pascal_FLAGS "-dSCRIPT_FFI_FUZZ")
  endif()
endif()

if(BUILD_RCON)
  add_flag_append(CMAKE_Pascal_FLAGS "-dRCON")
endif()

file(GLOB_RECURSE sources CONFIGURE_DEPENDS "${CMAKE_SOURCE_DIR}/server/*"
     "${CMAKE_SOURCE_DIR}/shared/*.pas" "${CMAKE_SOURCE_DIR}/3rdparty/*.pas"
     "${CMAKE_SOURCE_DIR}/3rdparty/*.inc")

set_source_files_properties(soldatserver.lpr PROPERTIES OBJECT_DEPENDS
                                                        "${sources}")

add_executable(soldatserver soldatserver.lpr)

if(ADD_ASSETS)
  add_dependencies(soldatserver soldat_smod)
endif()

if(BUILD_SCRIPTCORE AND (ADD_BFT OR ADD_FFI_FUZZ))
  add_subdirectory(scriptcore/test)
endif()

if(BUILD_STEAM)
  target_link_libraries(soldatserver steam_api)
else()
  target_link_libraries(soldatserver GameNetworkingSockets)
endif()

install(
  PROGRAMS ${EXECUTABLE_OUTPUT_PATH}/$<TARGET_FILE_NAME:soldatserver>
  DESTINATION ${target_binary_install_dir}
)
