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
INCLUDE = -Fu../lib/jpeg/$(ARCH)-$(OS) -Fu../lib/ogg/$(ARCH)-$(OS) -Fu../lib/zlib/$(ARCH)-$(OS)
OUTPUT  = ./
DESTDIR = /usr
TMP     = -FUtmp
TARGET  = libZenGL.so
TARGETW = ZenGL.dll
TARGETM = libZenGL.dylib

# Targets
all: clean
	$(FPC) ZenGL.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGET) -TLINUX -P$(ARCH)
	strip $(OUTPUT)$(TARGET) --strip-unneeded -R .comment -R .note

win32: clean
	$(FPC) ZenGL.pas -Fu../lib/jpeg/i386-win32 -Fu../lib/msvcrt/i386 -Fu../lib/ogg/i386-win32 -Fu../lib/zlib/i386-win32 $(TMP) $(FLAGS) -o$(TARGETW) -TWIN32 -Pi386 -WG

win64: clean
	$(FPC) ZenGL.pas -Fu../lib/jpeg/x86_64-win64 -Fu../lib/msvcrt/x86_64 -Fu../lib/ogg/x86_64-win64 -Fu../lib/zlib/x86_64-win64 $(TMP) $(FLAGS) -o$(TARGETW) -TWIN64 -Px86_64 -WG

macos: clean
	$(FPC_MAC)386 ZenGL.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGETM)-386 -k"-no_order_inits" -k"-no_order_data"
	$(FPC_MAC)ppc ZenGL.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGETM)-ppc -k"-no_order_inits" -k"-no_order_data"
	lipo -create $(OUTPUT)$(TARGETM)-ppc $(OUTPUT)$(TARGETM)-386 -output $(OUTPUT)$(TARGETM)
	rm $(OUTPUT)$(TARGETM)-386 $(OUTPUT)$(TARGETM)-ppc

clean:
	rm -f *.*~
	rm -f -R ./tmp/*

install:
	install -m 644 $(OUTPUT)$(TARGET) $(DESTDIR)/lib

uninstall:
	rm -f $(DESTDIR)/lib/$(TARGET)
