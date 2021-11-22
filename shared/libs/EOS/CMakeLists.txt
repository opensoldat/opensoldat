set(EOS_SDK_DIR "Location of EOS SDK" CACHE STRING "")
set(EOS_ANTICHEAT_TOOLS_DIR "Location of EAC Anti-cheat tools" CACHE STRING "")
set(EOS_CERTS "Location of EAC certs" CACHE STRING "")
set(EOS_DEPLOYMENT_ID "EOS Deployment ID" CACHE STRING "")
set(EOS_PRODUCT_ID "EOS Product ID" CACHE STRING "")
set(EOS_SANDBOX_ID "EOS Sandbox ID" CACHE STRING "")
set(EOS_CLIENT_ID "EOS Client ID for soldat" CACHE STRING "")
set(EOS_CLIENT_SECRET "EOS Client secret for soldat" CACHE STRING "")
set(EOS_SERVER_ID "EOS Client ID for soldatserver" CACHE STRING "")
set(EOS_SERVER_SECRET "EOS Client secret for soldat" CACHE STRING "")

if (UNIX)
  set(EOS_LIBRARY_NAME "libEOSSDK-Linux-Shipping.so")
  set(EOS_EXE_NAME "soldat")
endif()

if(WIN32)
  set(EOS_LIBRARY_NAME "EOSSDK-Win64-Shipping.dll")
  set(EOS_EXE_NAME "soldat_x64.exe")
endif()

if(APPLE)
  set(EOS_LIBRARY_NAME "libEOSSDK-Mac-Shipping.dylib")
  set(EOS_EXE_NAME "soldat_x64")
endif()

configure_file(${CMAKE_SOURCE_DIR}/shared/libs/EOS/utils/Settings.json ${EXECUTABLE_OUTPUT_PATH}/EasyAntiCheat/Settings.json.compiled @ONLY)

add_custom_target(copy_eos_sdk
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
  "${EOS_SDK_DIR}/SDK/Bin/${EOS_LIBRARY_NAME}"
  ${EXECUTABLE_OUTPUT_PATH}
)

add_dependencies(soldat copy_eos_sdk)

add_custom_target(copy_anticheat_bootstrapper
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${EOS_ANTICHEAT_TOOLS_DIR}/dist" ${EXECUTABLE_OUTPUT_PATH}
  COMMAND ${CMAKE_COMMAND} -E copy ${EXECUTABLE_OUTPUT_PATH}/EasyAntiCheat/Settings.json.compiled ${EXECUTABLE_OUTPUT_PATH}/EasyAntiCheat/Settings.json
)

add_dependencies(soldat copy_anticheat_bootstrapper)

if(WIN32)
  file(WRITE ${EXECUTABLE_OUTPUT_PATH}/EAC_install.bat "%~dp0EasyAntiCheat\\EasyAntiCheat_EOS_Setup.exe install -productid ${EOS_PRODUCT_ID}\n")
  file(WRITE ${EXECUTABLE_OUTPUT_PATH}/EAC_uinstall.bat "%~dp0EasyAntiCheat\\EasyAntiCheat_EOS_Setup.exe uninstall -productid ${EOS_PRODUCT_ID}\n")
endif()

add_custom_target(integrity_tool ALL
  COMMAND "${EOS_ANTICHEAT_TOOLS_DIR}/devtools/anticheat_integritytool${CMAKE_EXECUTABLE_SUFFIX}" -inkey "${EOS_CERTS}/base_private.key" -incert "${EOS_CERTS}/base_public.cer" -productid "${EOS_PRODUCT_ID}" -target_game_dir "${EXECUTABLE_OUTPUT_PATH}" "${CMAKE_SOURCE_DIR}/shared/libs/EOS/utils/anticheat_integritytool.cfg"
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMENT "Hashing game files with anticheat_integritytool"
)

install(
  DIRECTORY ${EXECUTABLE_OUTPUT_PATH}/EasyAntiCheat
  DESTINATION ${target_full_binary_install_dir}
  PATTERN "*.bat"
)

if(WIN32)
  install(
    DIRECTORY ${EXECUTABLE_OUTPUT_PATH}/
    DESTINATION ${target_full_binary_install_dir}
    PATTERN "*.bat"
  )
endif()