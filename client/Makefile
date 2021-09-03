.PHONY: build all windows linux osx windows_i386 windows_x86_64 linux_i386 linux_x86_64 osx_i386 osx_x86_64 clean

EXECUTABLE = soldat

TRUE = true
MKDIR = mkdir -p
PATHSEP = /

ifeq ($(OS),Windows_NT)
  SYSTEM = windows

  ifdef ComSpec
    TRUE = echo.>NUL
    MKDIR = mkdir
    PATHSEP = \\
  endif

  ifeq ($(PROCESSOR_ARCHITEW6432),AMD64)
    ARCH = x86_64
  else
    ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
      ARCH = x86_64
    endif
    ifeq ($(PROCESSOR_ARCHITECTURE),x86)
      ARCH = i386
    endif
    ifeq ($(PROCESSOR_ARCHITECTURE),ARM64)
      ARCH = arm
    endif
  endif
else
  ifeq ($(shell uname), Linux)
    SYSTEM = linux
  else
    ifeq ($(shell uname), Darwin)
      SYSTEM = osx
    else
      SYSTEM = unknown
    endif
  endif
    UNAME_P := $(shell uname -p)
    ifeq ($(UNAME_P),unknown)
      ARCH = $(shell uname -m)
    endif
    ifeq ($(UNAME_P),x86_64)
      ARCH = x86_64
    endif
    ifneq ($(filter %86,$(UNAME_P)),)
      ARCH = i386
    endif
    ifneq ($(filter arm%,$(UNAME_P)),)
      ARCH = arm
    endif
endif

ifeq ($(mode),release)
  CFLAGS = -O3 -Os -CX -OoREGVAR -Xs -XX -vewibq -vn-h- -dTESTING
  ifeq ($(MAKECMDGOALS),windows_x86_64)
    CFLAGS += -WB -WR
  endif
  ifeq ($(MAKECMDGOALS),windows_i386)
    CFLAGS += -WB -WR
  endif
else
  mode = debug
  CFLAGS = -O1 -g -gl -l -vewnibq -vh- -dDEBUG -dDEVELOPMENT
endif

ifeq ($(steam),on)
  CFLAGS += -dSTEAM
endif

COMPILER = fpc -l -B -MDelphi -Scgi ${CFLAGS} \
-Fi../3rdparty/Indy10/Lib/Core \
-Fi../shared \
-Fl./libs \
-Fu./libs \
-Fu../3rdparty/Pascal-SDL-2-Headers \
-Fu../3rdparty/Indy10/Lib/System \
-Fu../3rdparty/Indy10/Lib/Core \
-Fu../shared \
-Fu../shared/libs \
-Fu../shared/libs/GameNetworkingSockets \
-Fu../shared/libs/PhysFS \
-Fu../shared/libs/SteamWrapper \
-Fu../shared/mechanics \
-Fu../shared/network \
-Fu../server \
-Fu./libs/dglOpenGL \
-Fu./libs/stb \
-Fu./libs/FreeType \
-Fu./libs/openal \
-FE./build/

build: $(SYSTEM)_$(ARCH)

all: windows linux osx

windows: windows_$(ARCH)

linux: linux_$(ARCH)

osx: osx_$(ARCH)

windows_i386:
	$(MKDIR) build$(PATHSEP)i386-win32 || $(TRUE)
	$(COMPILER) -o./build/$(EXECUTABLE).exe -FU./build/i386-win32 -Twin32 -Pi386 $(EXECUTABLE).lpr

windows_x86_64:
	$(MKDIR) build$(PATHSEP)x86_64-win64 || $(TRUE)
	$(COMPILER) -o./build/$(EXECUTABLE)_x64.exe -FU./build/x86_64-win64 -Twin64 -Px86_64 $(EXECUTABLE).lpr

linux_i386:
	$(MKDIR) build$(PATHSEP)i386-linux || $(TRUE)
	$(COMPILER) -o./build/$(EXECUTABLE) -k-rpath -k'\$$ORIGIN/' -FU./build/i386-linux -Tlinux -Pi386 $(EXECUTABLE).lpr

linux_x86_64:
	$(MKDIR) build$(PATHSEP)x86_64-linux || $(TRUE)
	$(COMPILER) -o./build/$(EXECUTABLE)_x64 -k-rpath -k'\$$ORIGIN/' -FU./build/x86_64-linux -Tlinux -Px86_64 $(EXECUTABLE).lpr

osx_i386:
	$(MKDIR) build$(PATHSEP)i386-darwin || $(TRUE)
	$(COMPILER) -o./build/$(EXECUTABLE) -Fu./macOS/Frameworks -Fl./macOS/Frameworks -Fu./libs/macOS -k"-FmacOS/Frameworks" -k"-framework" -k"OpenAL" -k"-framework" -k"OpenGL" -k"-framework" -k"SDL2" -k"-rpath" -k"@executable_path" -k"-rpath" -k"@executable_path/../Frameworks" -dOPENGL_FRAMEWORK  -FU./build/i386-darwin -Tdarwin -Pi386 $(EXECUTABLE).lpr

osx_x86_64:
	$(MKDIR) build$(PATHSEP)x86_64-darwin || $(TRUE)
	$(COMPILER) -o./build/$(EXECUTABLE)_x64 -Fu./macOS/Frameworks -Fl./macOS/Frameworks -Fu./libs/macOS -k"-FmacOS/Frameworks" -k"-framework" -k"OpenAL" -k"-framework" -k"OpenGL" -k"-framework" -k"SDL2" -k"-rpath" -k"@executable_path" -k"-rpath" -k"@executable_path/../Frameworks" -dOPENGL_FRAMEWORK  -FU./build/x86_64-darwin -Tdarwin -Px86_64 $(EXECUTABLE).lpr

clean:
	rm -rf \
build/i386-linux/* \
build/x86_64-linux/* \
build/i386-win32/* \
build/x86_64-win64/* \
build/i386-darwin/* \
build/x86_64-darwin/* \
build/$(EXECUTABLE) \
build/$(EXECUTABLE).exe \
build/$(EXECUTABLE)_x64 \
build/$(EXECUTABLE)_x64.exe
