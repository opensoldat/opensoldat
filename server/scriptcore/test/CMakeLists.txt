add_custom_target(bft
  COMMAND ${CMAKE_COMMAND} -E copy_directory
    ${CMAKE_CURRENT_SOURCE_DIR}/bft
    ${EXECUTABLE_OUTPUT_PATH}/scripts/bft
  COMMENT "Copying BFT test to ${EXECUTABLE_OUTPUT_PATH}/scripts/bft"
)
