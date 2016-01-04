**ZenGL** - cross-platform game development library, designed to provide necessary functionality for rendering 2D-graphics, handling input, sound output, etc.

  * **License:** [zlib](http://zengl.org/license.html)
  * **Supported OS:** GNU/Linux, Windows, MacOS X, iOS, Android 2.1+
  * **Supported compilers:** [FreePascal](http://freepascal.org), [Delphi](http://www.embarcadero.com/products/delphi)
  * **Graphics API:** OpenGL, OpenGL ES 1.x, Direct3D 8/9
  * **Sound API:** OpenAL, DirectSound

  * **Main**
    * can be used as so/dll/dylib or statically compiled with your application
    * rendering to own or any other prepared window
    * logging
    * resource loading from files, memory and **zip** archives
    * multithreaded resource loading
    * easy way to add support for new resource format
  * **Configuration of**
    * antialiasing, screen resolution, refresh rate and vertical synchronization
    * aspect correction
    * title, position and size of window
    * cursor visibility in window space
  * **Input**
    * handling keyboard, mouse and joystick input
    * handling of Unicode text input
    * possibility to restrict the input to the Latin alphabet
  * **Textures**
    * supports **tga**, **png** and **jpg**
    * correct work with NPOT textures
    * control the filter parameters
    * masking
    * _render targets_ for rendering into texture
  * **Text**
    * textured Unicode-font
    * rendering UTF-8 text
    * rendering text with alignment and other options like size, color and count of symbols
  * **2D-subsystem**
    * _batch render_ for high-speed rendering
    * rendering different primitives
    * sprite engine
    * rendering static and animated sprites and tiles
    * rendering distortion grid
    * rendering sprites with new texture coordinates(with the pixel dimension and the usual 0..1)
    * control the blend mode and color mix mode
    * control the color and alpha of vertices of sprites and primitives
    * additional sprite transformations(flipping, zooming, vertices offset)
    * fast clipping of invisible sprites
    * 2D camera with ability to zoom and rotate the scene
  * **Sound**
    * works through OpenAL or DirectSound, depends on configuration or OS
    * correct work without soundcard
    * supports **wav** and **ogg** as sound samples
    * playing **ogg** files in separate thread
    * control the volume and playback speed
    * moving sound sources in 3D space
  * **Video**
    * decoding video frames into texture
    * supports **theora** codec in **ogv** container
  * **Mathematic**
    * basic set of additional math functions
    * triangulation functions
    * basic set of collision functions
  * **Additional**
    * reading and writing ini-files
    * functions for work with files and memory

![![](http://zengl.org/screens/screen01s.jpg)](http://zengl.org/screens/screen01.jpg) ![![](http://zengl.org/screens/screen02s.jpg)](http://zengl.org/screens/screen02.jpg) ![![](http://zengl.org/screens/screen03s.jpg)](http://zengl.org/screens/screen03.jpg) ![![](http://zengl.org/screens/screen04s.png)](http://zengl.org/screens/screen04.png)