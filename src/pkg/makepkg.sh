#!/bin/sh

VERSION=0.0.25

cd ./pkg/
rm -f *.deb
rm -f *.rpm
rm -f *.tar.gz

#archlinux
cp ../libZenGL.so ./arch/usr/lib
cd ./arch
tar -czvf ../zengl-"$VERSION"-i686.pkg.tar.gz usr .PKGINFO
rm -f ./usr/lib/libZenGL.so
cd ..

#deb32
cp ../libZenGL.so ./deb32/usr/lib
dpkg --build ./deb32 libzengl-"$VERSION"_i386.deb
rm -f ./deb32/usr/lib/libZenGL.so

#deb64
cp ../libZenGL.so ./deb64/usr/lib32
dpkg --build ./deb64 libzengl-"$VERSION"_amd64.deb
rm -f ./deb64/usr/lib32/libZenGL.so

#rpm
sudo rpmbuild -bb ./rpm/libzengl.spec --target i686
cp /usr/src/rpm/RPMS/i686/libzengl-"$VERSION"-1.i686.rpm ./libzengl-"$VERSION"-1.i686.rpm

#tar.gz
cp ../libZenGL.so ./tar.gz/usr/lib
cd ./tar.gz
tar -czvf ../libzengl-"$VERSION".tar.gz usr
rm -f ./usr/lib/libZenGL.so
cd ..
