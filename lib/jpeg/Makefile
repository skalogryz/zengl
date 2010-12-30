all:
	gcc wrapper.c -c -oi386-linux/wrapper.o -O2 -s -ffast-math -std=c99 -march=i686 -m32
	gcc wrapper.c -c -ox86_64-linux/wrapper.o -O2 -s -ffast-math -std=c99 -m64 -fPIC
	i486-mingw32-gcc wrapper.c -c -oi386-win32/wrapper.o -O2 -s -ffast-math -std=c99 -march=i686
	x86_64-w64-mingw32-gcc wrapper.c -c -ox86_64-win64/wrapper.o -O2 -s -ffast-math -std=c99
