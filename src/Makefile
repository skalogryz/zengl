FPC     = fpc
FPC_MAC = /usr/local/bin/ppc
FLAGS   = -O3 -XdsX -CX -Sd
INCLUDE =  -Fu../lib -Fu../lib/ogg -Fu../lib/zlib -Fu../lib/PasJPEG
OUTPUT  = ./
DESTDIR = /usr
TMP     = -FUtmp
TARGET  = libZenGL.so
TARGETW = ZenGL.dll
TARGETM = libZenGL.dylib

all: clean
	$(FPC) ZenGL.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGET) -TLINUX
	strip $(OUTPUT)$(TARGET) --strip-unneeded -R .comment -R .note

macos: clean
	$(FPC_MAC)386 ZenGL.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGETM)-386 -k"-no_order_inits" -k"-no_order_data"
	$(FPC_MAC)ppc ZenGL.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGETM)-ppc -k"-no_order_inits" -k"-no_order_data"
	lipo -create $(OUTPUT)$(TARGETM)-ppc $(OUTPUT)$(TARGETM)-386 -output $(OUTPUT)$(TARGETM)
	rm $(OUTPUT)$(TARGETM)-386 $(OUTPUT)$(TARGETM)-ppc

win32: clean
	$(FPC) ZenGL.pas $(INCLUDE) $(TMP) $(FLAGS) -o$(TARGETW) -TWIN32 -Pi386 -WG

win64: clean
	$(FPC) ZenGL.pas $(INCLUDE) $(TMP) $(FLAGS) -o$(TARGETW) -TWIN64 -Px86_64 -WG

clean:
	rm -f *.*~
	rm -f -R ./tmp/*

install:
	install -m 644 $(OUTPUT)$(TARGET) $(DESTDIR)/lib

uninstall:
	rm -f $(DESTDIR)/lib/$(TARGET)
