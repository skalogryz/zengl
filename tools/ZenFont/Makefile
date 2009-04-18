FPC     = fpc
FLAGS   = -O3 -XsX -CX -Sd
INCLUDE = -Fu../../src -Fu../../extra
OUTPUT  = ./
TMP     = -FUtmp
TARGET  = ZenFont
TARGETW = ZenFont.exe

all: clean
	$(FPC) ZenFont.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGET) -TLINUX
	strip $(OUTPUT)$(TARGET) --strip-unneeded -R .comment -R .note

win32: clean
	$(FPC) ZenFont.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGETW) -TWIN32 -WG

clean:
	rm -f *.*~
	rm -f ./tmp/*
