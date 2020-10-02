if(APPLE)
  add_flag_append(CMAKE_Pascal_FLAGS "-Fu./macOS/Frameworks")
  add_flag_append(CMAKE_Pascal_FLAGS "-Fl./macOS/Frameworks")
  add_flag_append(CMAKE_Pascal_FLAGS "-Fu./macOS/Frameworks")
  add_flag_append(CMAKE_Pascal_FLAGS "-k-FmacOS/Frameworks")
  add_flag_append(CMAKE_Pascal_FLAGS "-k\"-rpath\" -k\"@executable_path\"")
  add_flag_append(CMAKE_Pascal_FLAGS
                  "-k\"-rpath\" -k\"@executable_path/../Frameworks\"")
endif()

add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/server")
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/server/scriptcore")
add_flag_prepend(CMAKE_Pascal_FLAGS
                 "-Fu${CMAKE_SOURCE_DIR}/3rdparty/pascalscript/Source")

add_flag_append(CMAKE_Pascal_FLAGS "-dSERVER")

option(BUILD_SCRIPTCORE "Build ScriptCore" True)
option(BUILD_RCON "Build Rcon" True)

if(BUILD_SCRIPTCORE)
  add_flag_append(CMAKE_Pascal_FLAGS "-dSCRIPT")
endif()

if(BUILD_RCON)
  add_flag_append(CMAKE_Pascal_FLAGS "-dRCON")
endif()

file(GLOB_RECURSE sources CONFIGURE_DEPENDS "${CMAKE_SOURCE_DIR}/server/*"
     "${CMAKE_SOURCE_DIR}/shared/*.pas" "${CMAKE_SOURCE_DIR}/3rdparty/*.pas")

set_source_files_properties(soldatserver.lpr PROPERTIES OBJECT_DEPENDS
                                                        "${sources}")

add_executable(soldatserver soldatserver.lpr)

if(NOT BUILD_STEAM)
  target_link_libraries(soldatserver GameNetworkingSockets)
endif()

install(
  PROGRAMS ${EXECUTABLE_OUTPUT_PATH}/$<TARGET_FILE_NAME:soldatserver>
  DESTINATION ${CMAKE_INSTALL_PREFIX}
)