# Downloads required assets during configure step (i.e. when
# calling cmake). We add custom targets, so that client and server
# can set them as their dependencies, which enables automatic
# copying of asset files during build step (i.e. when calling make).
macro(download_assets)
  include(FetchContent)

  set(BASE_REPOSITORY_URL "https://github.com/soldat/base"
      CACHE STRING "We pull soldat.smod and font from here")
  set(BASE_GIT_TAG "v0.1"
      CACHE STRING "Git tag associated with release in base repository")
  set(SOLDAT_SMOD_SHA1 "d14d872fe0cbfbb919e6f22827c503ac58b30674")
  set(DOWNLOAD_URL ${BASE_REPOSITORY_URL}/releases/download/${BASE_GIT_TAG})
  set(DOWNLOADS_DIR downloads)

  message(STATUS "Soldat assets will be downloaded from ${DOWNLOAD_URL}")
  file(DOWNLOAD
    ${DOWNLOAD_URL}/soldat.smod
    ${DOWNLOADS_DIR}/soldat.smod
    EXPECTED_HASH SHA1=${SOLDAT_SMOD_SHA1}
  )

  file(DOWNLOAD
    ${DOWNLOAD_URL}/play-regular.ttf
    ${DOWNLOADS_DIR}/play-regular.ttf
  )

  # Configure targets, so that we can set dependencies
  # on client and server. This will make sure that
  # asset files get copied during build step (make).
  add_custom_target(
    soldat_smod
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${DOWNLOADS_DIR}/soldat.smod
                                                  ${EXECUTABLE_OUTPUT_PATH}/soldat.smod
    COMMENT "Copying soldat.smod to ${EXECUTABLE_OUTPUT_PATH}"
  )
  add_custom_target(
    soldat_font
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${DOWNLOADS_DIR}/play-regular.ttf
                                                  ${EXECUTABLE_OUTPUT_PATH}/play-regular.ttf
    COMMENT "Copying font to ${EXECUTABLE_OUTPUT_PATH}"
  )
endmacro()
