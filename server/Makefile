.PHONY: build all windows linux osx windows_i386 windows_x86_64 linux_i386 linux_x86_64 osx_i386 osx_x86_64 clean

ifeq ($(OS),Windows_NT)
  SYSTEM = windows
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
  CFLAGS = -O3 -Os -CX -OoREGVAR -Xs -XX -vewibq -vn-h-
  ifeq ($(MAKECMDGOALS),windows_x86_64)
    CFLAGS += -WB -WR
  endif
  ifeq ($(MAKECMDGOALS),windows_i386)
    CFLAGS += -WB -WR
  endif
else
  mode = debug
  CFLAGS = -O1 -g -gl -l -vewnibq -vh- -dDEBUG -dDEVELOPMENT -gs -gw -Ci -Co -Cr -CR -Ct  -gl -gt
endif

ifeq ($(steam),on)
  CFLAGS += -dSTEAM
endif

COMPILER = fpc -l -B -MDelphi -Scgi ${CFLAGS} \
-Fi../3rdparty/Indy10/Lib/System \
-Fi../3rdparty/Indy10/Lib/Core \
-Fi../shared \
-Fl./libs \
-Fu./libs \
-Fu../3rdparty/pascalscript/Source \
-Fu../3rdparty/Indy10/Lib/System \
-Fu../3rdparty/Indy10/Lib/Core \
-Fu../shared \
-Fu../shared/libs \
-Fu../shared/libs/PhysFS \
-Fu../shared/libs/GameNetworkingSockets \
-Fu../shared/libs/SteamWrapper \
-Fu../shared/mechanics \
-Fu../shared/network \
-Fu./scriptcore \
-FE./build/ \
-dSERVER \
-dSCRIPT \
-dRCON \
-dPS_USESSUPPORT \
-vm3123 -vm3124 # hide fgl inlining warnings

build: $(SYSTEM)_$(ARCH)

all: windows linux osx

windows: windows_$(ARCH)

linux: linux_$(ARCH)

osx: osx_$(ARCH)

windows_i386:
	$(COMPILER) -o./build/soldatserver.exe -FU./build/win32 -Twin32 -Pi386 soldatserver.lpr

windows_x86_64:
	$(COMPILER) -o./build/soldatserver_x64.exe -FU./build/win64 -Twin64 -Px86_64 soldatserver.lpr

linux_i386:
	$(COMPILER) -o./build/soldatserver -k-rpath -k'\$$ORIGIN/' -FU./build/linux -Tlinux -Pi386 soldatserver.lpr

linux_x86_64:
	$(COMPILER) -o./build/soldatserver_x64 -k-rpath -k'\$$ORIGIN/' -FU./build/linux -Tlinux -Px86_64 soldatserver.lpr

osx_i386:
	$(COMPILER) -o./build/soldatserver -Fu./client/macOS/Frameworks -Fl../client/macOS/Frameworks -k"-F../client/macOS/Frameworks" -k"-rpath" -k"@executable_path" -k"-rpath" -k"@executable_path/../Frameworks" -FU./build/darwin -Tdarwin -Pi386 soldatserver.lpr

osx_x86_64:
	$(COMPILER) -o./build/soldatserver_x64 -Fu./client/macOS/Frameworks -Fl../client/macOS/Frameworks -k"-F../client/macOS/Frameworks" -k"-rpath" -k"@executable_path" -k"-rpath" -k"@executable_path/../Frameworks" -FU./build/darwin -Tdarwin -Px86_64 soldatserver.lpr

clean:
	rm -rf build/linux/* build/win32/* build/win64/* build/darwin/* build/soldatserver build/soldatserver.exe build/soldatserver_x64 build/soldatserver_x64.exe
