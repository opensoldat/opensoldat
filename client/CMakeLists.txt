find_package(SDL2 REQUIRED)
find_package(OpenAL REQUIRED)
find_package(Freetype REQUIRED)
add_subdirectory(libs/stb)

if(APPLE)
  add_flag_append(CMAKE_Pascal_FLAGS "-Fl/usr/local/lib")
  # add_flag_append(CMAKE_Pascal_FLAGS "-Fu./macOS/Frameworks")
  # add_flag_append(CMAKE_Pascal_FLAGS "-Fl./macOS/Frameworks")
  # add_flag_append(CMAKE_Pascal_FLAGS "-Fu./macOS/Frameworks")
  # add_flag_append(CMAKE_Pascal_FLAGS "-k-FmacOS/Frameworks")
  add_flag_append(CMAKE_Pascal_FLAGS "-k\"-framework\" -k\"OpenAL\"")
  add_flag_append(CMAKE_Pascal_FLAGS "-k\"-framework\" -k\"OpenGL\"")
  add_flag_append(CMAKE_Pascal_FLAGS "-k\"-rpath\" -k\"@executable_path\"")
  # add_flag_append(CMAKE_Pascal_FLAGS "-k\"-rpath\"
  # -k\"@executable_path/../Frameworks\"")
  add_flag_append(CMAKE_Pascal_FLAGS "-dOPENGL_FRAMEWORK")
  # set the correct library or framework style depending on the main SDL
  string(FIND "${SDL2_LIBRARY}" "dylib" sdl_framework)
  if(${sdl_framework} GREATER -1)
    add_flag_append(CMAKE_Pascal_FLAGS "-k-lsdl2")
  else()
    add_flag_append(CMAKE_Pascal_FLAGS "-k-framework -kSDL2")
  endif()
endif()

# Include unit search paths
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fl${CMAKE_SOURCE_DIR}/client/libs")
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/client/libs")
add_flag_prepend(CMAKE_Pascal_FLAGS
                 "-Fu${CMAKE_SOURCE_DIR}/client/libs/dglOpenGL")
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/client/libs/stb")
add_flag_prepend(CMAKE_Pascal_FLAGS
                 "-Fu${CMAKE_SOURCE_DIR}/client/libs/FreeType")
add_flag_prepend(CMAKE_Pascal_FLAGS "-Fu${CMAKE_SOURCE_DIR}/client/libs/openal")

# source files
file(GLOB_RECURSE sources CONFIGURE_DEPENDS "${CMAKE_SOURCE_DIR}/client/*"
     "${CMAKE_SOURCE_DIR}/shared/*.pas" "${CMAKE_SOURCE_DIR}/3rdparty/*.pas"
     "${CMAKE_SOURCE_DIR}/3rdparty/*.inc")

set_source_files_properties(opensoldat.lpr PROPERTIES OBJECT_DEPENDS "${sources}")

add_executable(opensoldat opensoldat.lpr)

if(ADD_ASSETS)
  add_dependencies(opensoldat soldat_smod opensoldat_font)
endif()

if(UNIX)
  target_link_libraries(opensoldat stb)
endif()

if(BUILD_STEAM)
  target_link_libraries(opensoldat steam_api)
else()
  target_link_libraries(opensoldat GameNetworkingSockets)
endif()

install(
  PROGRAMS ${EXECUTABLE_OUTPUT_PATH}/$<TARGET_FILE_NAME:opensoldat>
  DESTINATION "${target_binary_install_dir}"
)
if(ADD_ASSETS)
  install(
    FILES "${EXECUTABLE_OUTPUT_PATH}/play-regular.ttf"
    # FIXME: this should be target_data_install_dir, but opensoldat can't discover it there
    # (needs wrapper script or some equivalent global config)
    DESTINATION "${target_binary_install_dir}"
  )
endif()
