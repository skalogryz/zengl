FPC     = fpc
FLAGS   = -O3 -XsX -CX -Sd
INCLUDE = -Fu../Direct3D
OUTPUT  = ./
TMP     = -FU../tmp
TARGET  = ZenGL.dll

win32: clean
	$(FPC) ../ZenGL.pas $(INCLUDE) \
	-Fu../../lib/jpeg/i386-win32 \
	-Fu../../lib/msvcrt/i386 \
	-Fu../../lib/ogg/i386-win32 \
	-Fu../../lib/theora/i386-win32 \
	-Fu../../lib/zlib/i386-win32 \
	-Fu../../lib/zip/i386-win32 \
	$(TMP) $(FLAGS) -o$(OUTPUT)$(TARGET) -TWIN32 -Pi386 -WG

clean:
	rm -f *.*~
	rm -f -R ../tmp/*
