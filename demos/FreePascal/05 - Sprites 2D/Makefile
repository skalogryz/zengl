# Get current platform
ifeq ($(shell uname -m), x86_64)
  ARCH  = x86_64
else
  ARCH  = i386
endif

# Variables
FPC     = fpc
FPC_MAC = /usr/local/bin/ppc
FLAGS   = -O3 -XsX -CX -Sd
INCLUDE = -Fu../../../headers/ -Fu../../../extra/ -Fu../../../src/ -Fu../../../lib/ -Fu../../../lib/PasJPEG
OUTPUT  = ../../../bin/
TMP     = ../tmp/
UNIT    = demo05.pas
TARGET  = demo05
TARGETW = demo05.exe
TARGETM = demo05
BUNDLE  = $(OUTPUT)$(TARGETM).app

# Targets
all: clean
	$(FPC) $(UNIT) $(INCLUDE) -FE$(OUTPUT)$(ARCH)/ -FU$(TMP) $(FLAGS) -o$(TARGET) -TLINUX
	strip $(OUTPUT)$(ARCH)/$(TARGET) --strip-unneeded -R .comment -R .note

win32: clean
	$(FPC) $(UNIT) $(INCLUDE) -FE$(OUTPUT)i386/ -FU$(TMP) $(FLAGS) -o$(TARGETW) -TWIN32 -Pi386 -WG

win64: clean
	$(FPC) $(UNIT) $(INCLUDE) -FE$(OUTPUT)x86_64/ -FU$(TMP) $(FLAGS) -o$(TARGETW) -TWIN64 -WG

macos: clean
	rm -Rf $(BUNDLE)
	mkdir {$(BUNDLE),$(BUNDLE)/{Contents,Contents/{MacOS,Resources,Frameworks}}}
	$(FPC_MAC)386 $(UNIT) $(INCLUDE) -FE$(OUTPUT) -FU$(TMP) $(FLAGS) -o$(TARGETM)-386
	$(FPC_MAC)ppc $(UNIT) $(INCLUDE) -FE$(OUTPUT) -FU$(TMP) $(FLAGS) -o$(TARGETM)-ppc
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
