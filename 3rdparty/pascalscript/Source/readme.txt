RemObjects Pascal

Files in this distribution:
help/                  - Help documents (html format) and examples
demo/                  - Test application.
demo_import/           - Test application with class library.
demo_kylix/            - Kylix test application.
ifps3.pas              - The runtime interpreter
ifpscomp.pas           - The compiler
ifps3common.pas        - The Common types and constants used by IFPS3
ifps3lib_std.pas       - The standard library
ifps3lib_stdr.pas      - The standard library (runtime)
ifps3utl.pas           - The utility unit
ifps3_def.inc	       - The include file
readme.txt             - Readme (this file)
license.txt            - License Agreement
ifpidll2runtime.pas    - runtime dll support.
ifpidll2.pas           - Compiler dll loading support.
ifpicall.pas           - Call library used by all runtime calling units.
ifpii_*.pas            - Import libraries (compiler)
ifpiir_*.pas           - Import libraries (runtime)
IFPS3CompExec.pas      - A component wrapper around IFPS3

http://www.carlo-kok.com/

Installing:

The .dpk file was build with Delphi 7, if you don't have Delphi 7, it might
complain about missing packages, you can remove those from the requires list
and remove and compiler directives it will complain about. Install the .dpk
file, and add the IFPS3 directory to your search paths.

