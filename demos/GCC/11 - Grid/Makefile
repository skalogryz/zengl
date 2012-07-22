# Variables
GCC     = gcc
MINGW32 = i486-mingw32-gcc
MINGW64 = x86_64-w64-mingw32-gcc
FLAGS   = -O3 -std=c99 -Wall
INCLUDE = -I../../../headers
LIBS    = -ldl -lm
OUTPUT  = ../../../bin/
TMP     = ../tmp/
UNIT    = demo11.c
TARGET  = demo11
TARGETW = demo11.exe

# Targets
win32: clean
	$(MINGW32) $(UNIT) $(INCLUDE) -o$(OUTPUT)i386-win32/$(TARGETW) $(FLAGS)
	strip $(OUTPUT)i386-win32/$(TARGETW) --strip-unneeded -R .comment -R .note

win64: clean
	$(MINGW64) $(UNIT) $(INCLUDE) -o$(OUTPUT)x86_64-win64/$(TARGETW) $(FLAGS)
	strip $(OUTPUT)x86_64-win64/$(TARGETW) --strip-unneeded -R .comment -R .note

clean:
	rm -f *.*~
	rm -f $(TMP)*.*
