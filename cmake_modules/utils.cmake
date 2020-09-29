macro(smod_builder)
  include(ExternalProject)

  ExternalProject_Add(
    soldatbase
    PREFIX "dist"
    GIT_REPOSITORY https://github.com/Soldat/base.git
    GIT_TAG master
    GIT_SHALLOW 1
    GIT_PROGRESS 1
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")

  set(SOLDAT_ASSETS_DIST ${CMAKE_BINARY_DIR}/dist/src/soldatbase)

  set(SOLDAT_ASSETS_DIR ${CMAKE_BINARY_DIR}/soldat-base)

  file(MAKE_DIRECTORY ${SOLDAT_ASSETS_DIR})

  add_custom_target(create_smod ALL DEPENDS ${EXECUTABLE_OUTPUT_PATH}/soldat.smod)
  add_dependencies(create_smod soldatbase)
  add_custom_command(
    OUTPUT ${EXECUTABLE_OUTPUT_PATH}/soldat.smod PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory ${SOLDAT_ASSETS_DIR}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${SOLDAT_ASSETS_DIR}/configs/

    COMMAND ${CMAKE_COMMAND} -E copy_directory ${SOLDAT_ASSETS_DIST}/shared/
                                               ${SOLDAT_ASSETS_DIR}/
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${SOLDAT_ASSETS_DIST}/client/configs/
                                               ${SOLDAT_ASSETS_DIR}/configs
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${SOLDAT_ASSETS_DIST}/server/configs/
                                               ${SOLDAT_ASSETS_DIR}/configs
    COMMAND ${CMAKE_COMMAND} -E copy ${SOLDAT_ASSETS_DIST}/client/play-regular.ttf
                                     ${EXECUTABLE_OUTPUT_PATH}

    COMMAND ${CMAKE_COMMAND} -E tar "cf" ${EXECUTABLE_OUTPUT_PATH}/soldat.smod
            --format=zip -- .
    #COMMAND ${CMAKE_COMMAND} -E remove_directory ${SOLDAT_ASSETS_DIR}
    WORKING_DIRECTORY ${SOLDAT_ASSETS_DIR}
    COMMENT "Creating soldat.smod"
    VERBATIM)
endmacro()

macro(add_flag_append _VAR_NAME _FLAG)
set(${_VAR_NAME} "${${_VAR_NAME}} ${_FLAG}")
endmacro(
add_flag_append
_VAR_NAME
_FLAG)

macro(add_flag_prepend _VAR_NAME _FLAG)
set(${_VAR_NAME} "${_FLAG} ${${_VAR_NAME}}")
endmacro(
add_flag_prepend
_VAR_NAME
_FLAG)