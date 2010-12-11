FPC     = fpc
FPC_MAC = /usr/local/bin/ppc
FLAGS   = -O3 -XsX -CX -Sd
INCLUDE = -Fu../../../headers/ -Fu../../../src/ -Fu../../../extra/ -Fu../../../lib/ -Fu../../../lib/PasJPEG
OUTPUT  = ../../../bin/
TMP     = ../tmp/
UNIT    = demo03.pas
TARGET  = demo03
TARGETW = demo03.exe
TARGETM = demo03
BUNDLE  = $(OUTPUT)$(TARGETM).app

all: clean
	$(FPC) $(UNIT) $(INCLUDE) -FE$(OUTPUT) -FU$(TMP) $(FLAGS) -o$(TARGET) -TLINUX
	strip $(OUTPUT)$(TARGET) --strip-unneeded -R .comment -R .note

win32: clean
	$(FPC) $(UNIT) $(INCLUDE) -FE$(OUTPUT) -FU$(TMP) $(FLAGS) -o$(TARGETW) -TWIN32 -Pi386 -WG

win64: clean
	$(FPC) $(UNIT) $(INCLUDE) -FE$(OUTPUT) -FU$(TMP) $(FLAGS) -o$(TARGETW) -TWIN64 -WG

macos: clean
	rm -Rf $(BUNDLE)
	mkdir {$(BUNDLE),$(BUNDLE)/{Contents,Contents/{MacOS,Resources,Frameworks}}}
	$(FPC_MAC)386 $(UNIT) $(INCLUDE) -FE$(OUTPUT) -FU$(TMP) $(FLAGS) -o$(TARGETM)-386
	$(FPC_MAC)ppc $(UNIT) $(INCLUDE) -FE$(OUTPUT) -FU$(TMP) $(FLAGS) -o$(TARGETM)-ppc
	lipo -create $(OUTPUT)$(TARGETM)-ppc $(OUTPUT)$(TARGETM)-386 -output $(BUNDLE)/Contents/MacOS/$(TARGETM)
	rm $(OUTPUT)$(TARGETM)-386 $(OUTPUT)$(TARGETM)-ppc

clean:
	rm -f *.*~
	rm -f $(TMP)*.*
