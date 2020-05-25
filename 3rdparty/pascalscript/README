arm EABI/EABIHF port of pascalscript.

Changes:
    * Implement aligned passing of 64-bit parameters for EABI/EABIHF.
    * Implement passing of floating point parameters in vfp registers for EABIHF
    * Rewrite return value handling to avoid passing pointers to unsafe stack
      space.
    * Use constref on parameters to assembler to ensure passing by reference

Todo/issues:
    * Only lightly tested, far more testing needed.
    * Only EABIHF has been tested so far
    * Handling of floating point return types needs implementing for old abi
      FPA (the usual variant of old ABI).
    * String return code looks dubious, may leak memory. Testing needed.
    * Dynamic array return code was not reimplemented in the new return
      value handling. The old code doesn't look like it ever worked.
