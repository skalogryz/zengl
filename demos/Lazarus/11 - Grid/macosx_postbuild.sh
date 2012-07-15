# copy resources
cp ../../../bin/data/back04.jpg ../../../bin/demo11.app/Contents/Resources/
cp ../../../bin/data/font* ../../../bin/demo11.app/Contents/Resources/
# make Info.plist and copy icon
cp -f demo11_macosx.plist ../../../bin/demo11.app/Contents/Info.plist
cp ../../../bin/data/zengl.icns ../../../bin/demo11.app/Contents/Resources/demo11.icns