# Get current platform
ifeq ($(shell uname -m), x86_64)
  ARCH  = x86_64
else
  ARCH  = i386
endif

# Variables
GCC     = gcc
MINGW32 = i486-mingw32-gcc
MINGW64 = x86_64-w64-mingw32-gcc
FLAGS   = -O3 -std=c99 -Wall
INCLUDE = -I../../../headers
LIBS    = -ldl
OUTPUT  = ../../../bin/
TMP     = ../tmp/
UNIT    = demo01.c
TARGET  = demo01
TARGETW = demo01.exe
TARGETM = demo01
BUNDLE  = $(OUTPUT)$(TARGETM).app

# Targets
all: clean
	$(GCC) $(UNIT) $(INCLUDE) $(LIBS) -o$(OUTPUT)$(ARCH)/$(TARGET) $(FLAGS)
	strip $(OUTPUT)$(ARCH)/$(TARGET) --strip-unneeded -R .comment -R .note

linux32: clean
	$(GCC) $(UNIT) $(INCLUDE) $(LIBS) -o$(OUTPUT)i386/$(TARGET) $(FLAGS) -m32
	strip $(OUTPUT)i386/$(TARGET) --strip-unneeded -R .comment -R .note

win32: clean
	$(MINGW32) $(UNIT) $(INCLUDE) -o$(OUTPUT)$(ARCH)/$(TARGET) $(FLAGS)
	strip $(OUTPUT)$(ARCH)/$(TARGET) --strip-unneeded -R .comment -R .note

win64: clean
	$(MINGW64) $(UNIT) $(INCLUDE) -o$(OUTPUT)$(ARCH)/$(TARGET) $(FLAGS)
	strip $(OUTPUT)$(ARCH)/$(TARGET) --strip-unneeded -R .comment -R .note

macos: clean
	rm -Rf $(BUNDLE)
	mkdir {$(BUNDLE),$(BUNDLE)/{Contents,Contents/{MacOS,Resources,Frameworks}}}
	$(FPC) $(UNIT) $(INCLUDE) $(LIBS) -FE$(OUTPUT) -FU$(TMP) $(FLAGS) -o$(TARGETM)-386 -TDARWIN -Pi386 -k"-macosx_version_min 10.4"
	$(FPC) $(UNIT) $(INCLUDE) $(LIBSPPC) -FE$(OUTPUT) -FU$(TMP) $(FLAGS) -o$(TARGETM)-ppc -TDARWIN -Ppowerpc -k"-macosx_version_min 10.4"
	lipo -create $(OUTPUT)$(TARGETM)-ppc $(OUTPUT)$(TARGETM)-386 -output $(BUNDLE)/Contents/MacOS/$(TARGETM)
	rm $(OUTPUT)$(TARGETM)-386 $(OUTPUT)$(TARGETM)-ppc

clean:
	rm -f *.*~
	rm -f $(TMP)*.*
