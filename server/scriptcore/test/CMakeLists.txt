if(ADD_BFT)
  add_custom_target(bft ALL
    COMMAND ${CMAKE_COMMAND} -E copy_directory
      ${CMAKE_CURRENT_SOURCE_DIR}/bft
      ${EXECUTABLE_OUTPUT_PATH}/scripts/bft
    COMMENT "Copying BFT test to ${EXECUTABLE_OUTPUT_PATH}/scripts/bft"
  )
endif()

if(ADD_FFI_FUZZ)
  add_custom_target(gen-ffi-fuzz
    COMMAND ${PYTHON3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/gen_ffi_tests.py
    OUTPUT "${CMAKE_CURRENT_SOURCE_DIR}/ffi/ffi.pas" "${CMAKE_CURRENT_SOURCE_DIR}/ScriptFFITests.pas"
    COMMENT "Generating new FFI fuzz tests."
  )

  add_custom_target(ffi-fuzz ALL
    COMMAND ${CMAKE_COMMAND} -E copy_directory
      ${CMAKE_CURRENT_SOURCE_DIR}/ffi
      ${EXECUTABLE_OUTPUT_PATH}/scripts/ffi
    COMMENT "Copying FFI test to ${EXECUTABLE_OUTPUT_PATH}/scripts/ffi"
  )
endif()
