# Variables
FPC     = fpc
FLAGS   = -O3 -XsX -CX -Sd
INCLUDE = -Fu../../../headers -Fi../../../headers -Fu../../../extra -Fu../../../src -Fu../../../src/Direct3D
OUTPUT  = ../../../bin/
TMP     = ../tmp/
UNIT    = demo08.pas
TARGETW = demo08.exe

# Targets
win32: clean
	$(FPC) $(UNIT) $(INCLUDE) \
	-Fu../../../lib/jpeg/i386-win32 \
	-Fu../../../lib/msvcrt/i386 \
	-Fu../../../lib/ogg/i386-win32 \
	-Fu../../../lib/theora/i386-win32 \
	-Fu../../../lib/zlib/i386-win32 \
	-Fu../../../lib/zip/i386-win32 \
	-FE$(OUTPUT)i386-win32/ -FU$(TMP) $(FLAGS) -o$(TARGETW) -TWIN32 -Pi386 -WG

win64: clean
	$(FPC) $(UNIT) $(INCLUDE) \
	-Fu../../../lib/jpeg/x86_64-win64 \
	-Fu../../../lib/msvcrt/x86_64 \
	-Fu../../../lib/ogg/x86_64-win64 \
	-Fu../../../lib/theora/x86_64-win64 \
	-Fu../../../lib/zlib/x86_64-win64 \
	-Fu../../../lib/zip/x86_64-win64 \
	-FE$(OUTPUT)x86_64-win64/ -FU$(TMP) $(FLAGS) -o$(TARGETW) -TWIN64 -Px86_64 -WG

clean:
	rm -f *.*~
	rm -f $(TMP)*.*
