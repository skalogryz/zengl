FPC     = fpc
FPC_MAC = /usr/local/lib/fpc/2.2.2/ppc386
FLAGS   = -O3 -XsX -CX -Sd
INCLUDE = -Fu../../src -Fu../../src/PasJPEG
OUTPUT  = ./
TMP     = -FUtmp
TARGET  = FontGen
TARGETW = FontGen.exe
TARGETM = FontGen

all: clean
	$(FPC) FontGen.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGET) -TLINUX
	strip $(OUTPUT)$(TARGET) --strip-unneeded -R .comment -R .note

macos: clean
	$(FPC_MAC) FontGen.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGETM) -Cn
	echo /System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib >> link.res
	echo /System/Library/Frameworks/OpenGL.framework/Libraries/libGLU.dylib >> link.res
	sh ppas.sh
	rm link.res
	rm ppas.sh

win32: clean
	$(FPC) FontGen.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGETW) -TWIN32 -WG

clean:
	rm -f ./src/*.*~
	rm -f ./tmp/*
