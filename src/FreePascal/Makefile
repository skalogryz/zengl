FPC     = fpc
DEFINES = #-dUSE_EXPORT_C -Cccdecl
FLAGS   = -O3 -XsX -CX -Sd $(DEFINES)
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

win64: clean
	$(FPC) ../ZenGL.pas $(INCLUDE) \
	-Fu../../lib/jpeg/x86_64-win64 \
	-Fu../../lib/msvcrt/x86_64 \
	-Fu../../lib/ogg/x86_64-win64 \
	-Fu../../lib/theora/x86_64-win64 \
	-Fu../../lib/zlib/x86_64-win64 \
	-Fu../../lib/zip/x86_64-win64 \
	$(TMP) $(FLAGS) -o$(OUTPUT)$(TARGET) -TWIN64 -Px86_64 -WG

clean:
	rm -f *.*~
	rm -f -R ./tmp/*
