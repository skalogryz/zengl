FPC     = fpc
FPC_MAC = /usr/local/lib/fpc/2.2.2/ppc386
FLAGS   = -O3 -XdsX -CX -Sd
INCLUDE = -FuPasJPEG
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
	$(FPC_MAC) ZenGL.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGETM)

win32: clean
	$(FPC) ZenGL.pas $(INCLUDE) $(TMP) $(FLAGS) -o$(TARGETW) -TWIN32 -WG

clean:
	rm -f *.*~
	rm -f -R ./tmp/*

install:
	install -m 644 $(OUTPUT)$(TARGET) $(DESTDIR)/lib

uninstall:
	rm -f $(DESTDIR)/lib/$(TARGET)
