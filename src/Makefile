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
DEFINES = #-dUSE_EXPORT_C -Cccdecl
FLAGS   = -O3 -XsX -CX -Sd $(DEFINES)
LIBS    = -Fu../lib/jpeg/$(ARCH)-$(OS) -Fu../lib/ogg/$(ARCH)-$(OS) -Fu../lib/zlib/$(ARCH)-$(OS)
LIBSPPC = -Fu../lib/jpeg/powerpc-$(OS) -Fu../lib/ogg/powerpc-$(OS) -Fu../lib/zlib/powerpc-$(OS)
OUTPUT  = ./
DESTDIR = /usr
TMP     = -FUtmp
TARGET  = libZenGL.so
TARGETW = ZenGL.dll
TARGETM = libZenGL.dylib

# Targets
all: clean
	$(FPC) ZenGL.pas $(LIBS) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGET) -TLINUX -P$(ARCH)
	strip $(OUTPUT)$(TARGET) --strip-unneeded -R .comment -R .note

linux32: clean
	$(FPC) ZenGL.pas -Fu../lib/jpeg/i386-linux -Fu../lib/ogg/i386-linux -Fu../lib/zlib/i386-linux -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGET) -TLINUX -Pi386 -Xd
	strip $(OUTPUT)$(TARGET) --strip-unneeded -R .comment -R .note

win32: clean
	$(FPC) ZenGL.pas -Fu../lib/jpeg/i386-win32 -Fu../lib/msvcrt/i386 -Fu../lib/ogg/i386-win32 -Fu../lib/zlib/i386-win32 $(TMP) $(FLAGS) -o$(TARGETW) -TWIN32 -Pi386 -WG

win64: clean
	$(FPC) ZenGL.pas -Fu../lib/jpeg/x86_64-win64 -Fu../lib/msvcrt/x86_64 -Fu../lib/ogg/x86_64-win64 -Fu../lib/zlib/x86_64-win64 $(TMP) $(FLAGS) -o$(TARGETW) -TWIN64 -Px86_64 -WG

wince: clean
	$(FPC) ZenGL.pas -Fu../lib/jpeg/arm-wince -Fu../lib/msvcrt/wince -Fu../lib/ogg/arm-wince -Fu../lib/zlib/arm-wince $(TMP) $(FLAGS) -o$(TARGETW) -TWINCE -Parm -WG

macos: clean
	$(FPC) ZenGL.pas $(LIBS) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGETM) -TDARWIN -Pi386 -k"-no_order_inits" -k"-no_order_data" -k"-macosx_version_min 10.4"

macos_uni: clean
	$(FPC) ZenGL.pas $(LIBS) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGETM)-386 -TDARWIN -Pi386 -k"-no_order_inits" -k"-no_order_data" -k"-macosx_version_min 10.4"
	$(FPC) ZenGL.pas $(LIBSPPC) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGETM)-ppc -TDARWIN -Ppowerpc -k"-no_order_inits" -k"-no_order_data" -k"-macosx_version_min 10.4"
	lipo -create $(OUTPUT)$(TARGETM)-ppc $(OUTPUT)$(TARGETM)-386 -output $(OUTPUT)$(TARGETM)
	rm $(OUTPUT)$(TARGETM)-386 $(OUTPUT)$(TARGETM)-ppc

clean:
	rm -f *.*~
	rm -f -R ./tmp/*

install:
	install -m 644 $(OUTPUT)$(TARGET) $(DESTDIR)/lib

uninstall:
	rm -f $(DESTDIR)/lib/$(TARGET)
