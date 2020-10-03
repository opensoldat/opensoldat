# Downloads required assets during configure step (i.e. when
# calling cmake). We add custom targets, so that client and server
# can set them as their dependencies, which enables automatic
# copying of asset files during build step (i.e. when calling make).
macro(download_assets)
  include(FetchContent)

  set(BASE_REPOSITORY_URL "https://github.com/rzaba0/base"
      CACHE STRING "We pull soldat.smod and font from here")
  set(BASE_GIT_TAG "v0.1"
      CACHE STRING "Git tag associated with release in base repository")

  set(DOWNLOAD_URL ${BASE_REPOSITORY_URL}/releases/download/${BASE_GIT_TAG})
  message(STATUS "Soldat assets will be downloaded from ${DOWNLOAD_URL}")
  FetchContent_Declare(
    smod
    URL ${DOWNLOAD_URL}/soldat.smod
    DOWNLOAD_NO_EXTRACT TRUE # By default, FetchContent unzips the file. We don't want that.
  )
  FetchContent_Declare(
    font
    URL ${DOWNLOAD_URL}/play-regular.ttf
    DOWNLOAD_NO_EXTRACT TRUE # Once again, we don't want any unzipping.
  )

  message(STATUS "Retrieving soldat.smod")
  FetchContent_MakeAvailable(smod)

  message(STATUS "Retrieving soldat font")
  FetchContent_MakeAvailable(font)

  # Configure targets, so that we can set dependencies
  # on client and server. This will make sure that
  # asset files get copied during build step (make).
  add_custom_target(
    soldat_smod
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${smod_SOURCE_DIR}/soldat.smod
                                                  ${EXECUTABLE_OUTPUT_PATH}/soldat.smod
    COMMENT "Copying soldat.smod to ${EXECUTABLE_OUTPUT_PATH}"
  )
  add_custom_target(
    soldat_font
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${font_SOURCE_DIR}/play-regular.ttf
                                                  ${EXECUTABLE_OUTPUT_PATH}/play-regular.ttf
    COMMENT "Copying font to ${EXECUTABLE_OUTPUT_PATH}"
  )
endmacro()