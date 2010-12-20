all:
	gcc wrapper.c -c -olinux_i386/wrapper.o -O2 -s -ffast-math -std=c99 -march=i686 -m32
	gcc wrapper.c -c -olinux_x86_64/wrapper.o -O2 -s -ffast-math -std=c99 -m64 -fPIC
	i486-mingw32-gcc wrapper.c -c -owin32/wrapper.o -O2 -s -ffast-math -std=c99 -march=i686
	x86_64-w64-mingw32-gcc wrapper.c -c -owin64/wrapper.o -O2 -s -ffast-math -std=c99
