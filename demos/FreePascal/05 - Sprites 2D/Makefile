# Get current platform
ifeq ($(shell uname -m), x86_64)
  ARCH  = x86_64
else
  ARCH  = i386
endif

# Get current OS
ifeq ($(shell uname), Linux)
  OS    = linux
else ifeq ($(shell uname), Darwin)
  OS    = darwin
endif

# Variables
FPC     = fpc
FPC_MAC = /usr/local/bin/ppc
FLAGS   = -O3 -XsX -CX -Sd
INCLUDE = -Fu../../../headers -Fu../../../extra -Fu../../../src
LIBS    = -Fu../../../lib/jpeg/$(ARCH)-$(OS) -Fu../../../lib/ogg/$(ARCH)-$(OS) -Fu../../../lib/zlib/$(ARCH)-$(OS)
OUTPUT  = ../../../bin/
TMP     = ../tmp/
UNIT    = demo05.pas
TARGET  = demo05
TARGETW = demo05.exe
TARGETM = demo05
BUNDLE  = $(OUTPUT)$(TARGETM).app

# Targets
all: clean
	$(FPC) $(UNIT) $(INCLUDE) $(LIBS) -FE$(OUTPUT)$(ARCH)/ -FU$(TMP) $(FLAGS) -o$(TARGET) -TLINUX
	strip $(OUTPUT)$(ARCH)/$(TARGET) --strip-unneeded -R .comment -R .note

win32: clean
	$(FPC) $(UNIT) $(INCLUDE) \
	-Fu../../../lib/jpeg/i386-win32 \
	-Fu../../../lib/msvcrt/i386 \
	-Fu../../../lib/ogg/i386-win32 \
	-Fu../../../lib/zlib/i386-win32 \
	-FE$(OUTPUT)i386/ -FU$(TMP) $(FLAGS) -o$(TARGETW) -TWIN32 -Pi386 -WG

win64: clean
	$(FPC) $(UNIT) $(INCLUDE) \
	-Fu../../../lib/jpeg/x86_64-win64 \
	-Fu../../../lib/msvcrt/x86_64 \
	-Fu../../../lib/ogg/x86_64-win64 \
	-Fu../../../lib/zlib/x86_64-win64 \
	-FE$(OUTPUT)x86_64/ -FU$(TMP) $(FLAGS) -o$(TARGETW) -TWIN64 -Px86_64 -WG

macos: clean
	rm -Rf $(BUNDLE)
	mkdir {$(BUNDLE),$(BUNDLE)/{Contents,Contents/{MacOS,Resources,Frameworks}}}
	$(FPC_MAC)386 $(UNIT) $(INCLUDE) $(LIBS) -FE$(OUTPUT) -FU$(TMP) $(FLAGS) -o$(TARGETM)-386
	$(FPC_MAC)ppc $(UNIT) $(INCLUDE) $(LIBS) -FE$(OUTPUT) -FU$(TMP) $(FLAGS) -o$(TARGETM)-ppc
	lipo -create $(OUTPUT)$(TARGETM)-ppc $(OUTPUT)$(TARGETM)-386 -output $(BUNDLE)/Contents/MacOS/$(TARGETM)
	cp $(OUTPUT)data/font* $(BUNDLE)/Contents/Resources/
	cp $(OUTPUT)data/zengl.png $(BUNDLE)/Contents/Resources/
	cp $(OUTPUT)data/back01.jpg $(BUNDLE)/Contents/Resources/
	cp $(OUTPUT)data/ground.png $(BUNDLE)/Contents/Resources/
	cp $(OUTPUT)data/tux* $(BUNDLE)/Contents/Resources/
	rm $(OUTPUT)$(TARGETM)-386 $(OUTPUT)$(TARGETM)-ppc

clean:
	rm -f *.*~
	rm -f $(TMP)*.*
