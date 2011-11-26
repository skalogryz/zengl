ARM_GCC=/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/arm-apple-darwin10-gcc-4.2.1

linux:
	gcc zlib_helper.c -c -oi386-linux/zlib_helper.o -O2 -s -ffast-math -std=c99 -march=i686 -m32
	gcc zlib_helper.c -c -ox86_64-linux/zlib_helper.o -O2 -s -ffast-math -std=c99 -m64 -fPIC

android:
	arm-none-eabi-gcc zlib_helper.c -c -oarm-linux/zlib_helper.o -O2 -s -Xassembler "-meabi=5" -ffast-math -std=c99 -I/home/andru/Desktop/android-ndk-r6b/platforms/android-8/arch-arm/usr/include

win32:
	i486-mingw32-gcc zlib_helper.c -c -oi386-win32/zlib_helper.o -O2 -s -ffast-math -std=c99 -march=i686

win64:
	x86_64-w64-mingw32-gcc zlib_helper.c -c -ox86_64-win64/zlib_helper.o -O2 -s -ffast-math -std=c99

wince:
	arm-wince-cegcc-gcc zlib_helper.c -c -oarm-wince/zlib_helper.o -I./ -O2 -s -ffast-math

macos:
	gcc zlib_helper.c -c -oi386-darwin/zlib_helper.o -O2 -s -ffast-math -std=c99 -march=i686 -m32
	powerpc-apple-darwin10-gcc-4.2.1 zlib_helper.c -c -opowerpc-darwin/zlib_helper.o -O2 -s -ffast-math -std=c99

ios:
	$(ARM_GCC) zlib_helper.c -c -oarm-darwin/zlib_helper.o -O2 -s -ffast-math -std=c99
	gcc zlib_helper.c -c -oi386-iphonesim/zlib_helper.o -O2 -s -ffast-math -std=c99 -march=i686 -m32