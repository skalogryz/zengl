# Variables
FPC     = fpc
FLAGS   = -O3 -XsX -CX -Sd
INCLUDE = -Fu../../../headers -Fu../../../extra -Fu../../../src -Fu../../../src/Direct3D
OUTPUT  = ../../../bin/
TMP     = ../tmp/
UNIT    = demo04.pas
TARGETW = demo04.exe

# Targets
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

clean:
	rm -f *.*~
	rm -f $(TMP)*.*
