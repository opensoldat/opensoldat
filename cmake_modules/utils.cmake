macro(add_flag_append _VAR_NAME _FLAG)
  set(${_VAR_NAME} "${${_VAR_NAME}} ${_FLAG}")
endmacro(add_flag_append _VAR_NAME _FLAG)

macro(add_flag_prepend _VAR_NAME _FLAG)
  set(${_VAR_NAME} "${_FLAG} ${${_VAR_NAME}}")
endmacro(add_flag_prepend _VAR_NAME _FLAG)