enable_language(Pascal)

find_package(SDL2 REQUIRED)
find_package(OpenAL REQUIRED)
find_package(Freetype REQUIRED)
add_subdirectory(libs/stb)

if(${CMAKE_Pascal_COMPILER_VERSION} VERSION_LESS 3.0)
  message(
    FATAL_ERROR
      "Your FreePascal installation is too old (fpc ${CMAKE_Pascal_COMPILER_VERSION})!"
  )
endif()

# Hide inline warning
if(${CMAKE_Pascal_COMPILER_VERSION} VERSION_GREATER 3.1)
  add_flag_append(CMAKE_Pascal_FLAGS "-vm6058")
endif()

# generic folder where our libraries reside
add_flag_append(CMAKE_Pascal_FLAGS "-Fl${LIBRARY_OUTPUT_PATH}")

if(APPLE)
  # add_flag_append(CMAKE_Pascal_FLAGS "-Fu./macOS/Frameworks")
  # add_flag_append(CMAKE_Pascal_FLAGS "-Fl./macOS/Frameworks")
  # add_flag_append(CMAKE_Pascal_FLAGS "-Fu./macOS/Frameworks")
  # add_flag_append(CMAKE_Pascal_FLAGS "-k-FmacOS/Frameworks")
  add_flag_append(CMAKE_Pascal_FLAGS "-k\"-framework\" -k\"OpenAL\"")
  add_flag_append(CMAKE_Pascal_FLAGS "-k\"-framework\" -k\"OpenGL\"")
  # add_flag_append(CMAKE_Pascal_FLAGS "-k\"-rpath\" -k\"@executable_path\"")
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

# Link to PhysFS
get_filename_component(PHYSFS_LIBRARY_DIR ${PHYSFS_LIBRARY} PATH)
add_flag_append(CMAKE_Pascal_FLAGS "-Fl${PHYSFS_LIBRARY_DIR}")

# Link to FreeType get_filename_component(FREETYPE_LIBRARY_DIR
# ${FREETYPE_LIBRARY} PATH) add_flag_append(CMAKE_Pascal_FLAGS
# "-Fl${FREETYPE_LIBRARY_DIR}") Link to OpenAL
add_flag_append(CMAKE_Pascal_FLAGS "-Fl${OPENAL_LIBRARY_DIR}")
get_filename_component(OPENAL_LIBRARY_DIR ${OPENAL_LIBRARY} PATH)

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
     "${CMAKE_SOURCE_DIR}/shared/*.pas" "${CMAKE_SOURCE_DIR}/3rdparty/*.pas")

set_source_files_properties(soldat.lpr PROPERTIES OBJECT_DEPENDS "${sources}")

add_executable(soldat soldat.lpr)

if(BUILD_SMOD)
  add_dependencies(soldat create_smod)
endif()

if(UNIX)
  target_link_libraries(soldat stb)
endif()

if(NOT BUILD_STEAM)
  target_link_libraries(soldat GameNetworkingSockets)
endif()