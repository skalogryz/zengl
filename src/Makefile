VERSION = 0.0.35
FPC     = fpc
FPC_WIN =
FPC_MAC = /usr/local/lib/fpc/2.2.2/ppc386
FLAGS   = -O3 -XdsX -CX #-a
#
INCLUDE = -Fuzgl_units \
-Fuzgl_units/system \
-Fuzgl_units/input \
-Fuzgl_units/textures \
-Fuzgl_units/gui \
-Fuzgl_units/sound \
-Fuzgl_units/collision \
-Fusys_units \
-Fusys_units/gl \
-Fusys_units/pasjpeg \
-Fusys_units/paszlib \
-Fusys_units/openal \
-Fusys_units/directsound
#
OUTPUT  = ./
DESTDIR = /usr
TMP     = -FUtmp
TARGET  = libZenGL.so
TARGETW = ZenGL.dll
TARGETM = libZenGL.dylib

all: clean
	$(FPC) zengl.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGET) -TLINUX #-Aas
	strip $(OUTPUT)$(TARGET) --strip-unneeded -R .comment -R .note

macos: clean
	$(FPC_MAC) zengl.pas $(INCLUDE) -FE$(OUTPUT) $(TMP) $(FLAGS) -o$(TARGETM) -Cn
	echo /System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib >> link.res
	echo /System/Library/Frameworks/OpenGL.framework/Libraries/libGLU.dylib >> link.res
	sh ppas.sh
	rm link.res
	rm ppas.sh

pkg: clean
	sh ./pkg/makepkg.sh

win32: clean
	$(FPC) zengl.pas $(INCLUDE) $(TMP) $(FLAGS) -o$(TARGETW) -TWIN32 -WG

inwin32: cleanw
	$(FPC_WIN) zengl.pas $(INCLUDE) $(TMP) $(FLAGS) -o$(TARGETW) -TWIN32 -WG

clean:
	rm -f *.*~
	rm -f -R ./tmp/*
	rm -f ./sys_units/*.*~
	rm -f ./zgl_units/*.*~
	rm -f ./zgl_units/system/*.*~
	rm -f ./zgl_units/input/*.*~
	rm -f ./zgl_units/textures/*.*~
	rm -f ./zgl_units/sound/*.*~
	rm -f ./zgl_units/collision/*.*~

cleanw:
	del /Q *.*~
	del /Q tmp\*
	del /Q sys_units\*.*~
	del /Q zgl_units\*.*~
	del /Q zgl_units\system\*.*~
	del /Q zgl_units\input\*.*~
	del /Q zgl_units\textures\*.*~
	del /Q zgl_units\sound\*.*~
	del /Q zgl_units\collision\*.*~

install:
	install -m 644 $(OUTPUT)$(TARGET) $(DESTDIR)/lib

uninstall:
	rm -f $(DESTDIR)/lib/$(TARGET)
