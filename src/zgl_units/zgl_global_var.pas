{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru.2x4.ru
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_global_var;

{$I define.inc}

interface
uses
  {$IFDEF LINUX}
  GLX, X, XLib, XUtil, xf86vmode, UnixType,
  openal,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  DirectSound,
  {$ENDIF}
  {$IFDEF DARWIN}
  AGL, MacOSAll,
  openal,
  {$ENDIF}
  zgl_const,
  zgl_types,
  zgl_gui_types;
  
procedure zero;

var
  // Managers
  managerTimer   : zglTTimerManager;
  managerTexture : zglTTextureManager;
  managerFont    : zglTFontManager;
  managerRTarget : zglTRenderTargetManager;
  managerSound   : zglTSoundManager;
  managerGUI     : zglTGUIManager;

  // timers
  CanKillTimers : Boolean;
  TimersToKill  : WORD = 0;
  aTimersToKill : array[ 0..cv_MaxCount ] of zglPTimer;

  // Formats, etc.
  texNFCount : Byte = 0;
  texFormats : array of zglTTextureFormat;

  sndNFCount : Byte = 0;
  sndFormats : array of zglTSoundFormat;

  widgetTCount : Byte = 0;
  widgetTypes  : array of zglTWidgetType;
  widgetTLast  : Byte;
  {------------------------------------ APP -----------------------------------}
  app_Work     : Boolean;
  app_WorkTime : DWORD;
  app_Log      : Boolean;

  app_InitToHandle : Boolean;
  
  app_FullScreen : Boolean;

  // call-back
  app_PLoad : procedure = zero;
  app_PDraw : procedure = zero;
  app_PExit : procedure = zero;

  app_Pause     : Boolean;
  app_AutoPause : Boolean = TRUE;
  
  app_Focus     : Boolean;
  app_AutoMinimize : Boolean;
  
  {$IFDEF LINUX}
  app_Cursor : TCursor = None;
  app_XIM    : PXIM;
  app_XIC    : PXIC;
  {$ENDIF}
  app_ShowCursor : Boolean = TRUE;
  
  app_FPS      : DWORD;
  app_FPSCount : DWORD;
  app_FPSAll   : DWORD;
  
  app_Flags : DWORD = APP_USE_LOG or COLOR_BUFFER_CLEAR or DEPTH_BUFFER or DEPTH_BUFFER_CLEAR or CROP_INVISIBLE;
  {---------------------------------- WINDOW ----------------------------------}
  wnd_X      : WORD;
  wnd_Y      : WORD;
  wnd_Width  : WORD = defWidth;
  wnd_Height : WORD = defHeight;
  
  wnd_Caption : String = cs_ZenGL;
  
  {$IFDEF LINUX}
  wnd_Handle : TWindow;
  wnd_Root   : TWindow;
  wnd_Attr   : TXSetWindowAttributes;
  wnd_Title  : TXTextProperty;
  
  wnd_ValueMask : DWORD;
  
  wnd_DestroyAtom : TAtom;
  wnd_Protocols   : TAtom;
  //wnd_MotifAtom   : TAtom;
  {$ENDIF}
  {$IFDEF WIN32}
  wnd_Handle    : HWND;
  wnd_DC        : HDC;
  wnd_INST      : HINST;
  wnd_Class     : TWndClassEx;
  wnd_ClassName : PChar = 'ZenGL';
  
  wnd_Style   : DWORD;
  wnd_StyleEx : DWORD;
  
  wnd_CpnSize  : WORD;
  wnd_BrdSizeX : WORD;
  wnd_BrdSizeY : WORD;
  {$ENDIF}
  {$IFDEF DARWIN}
  wnd_Handle : WindowRef;
  wnd_Attr   : WindowAttributes;

  wnd_CpnSize  : WORD;
  {$ENDIF}
  {---------------------------------- SCREEN ----------------------------------}
  scr_Width   : WORD;
  scr_Height  : WORD;
  scr_BPP     : WORD = defBPP;
  scr_Refresh : WORD;
  scr_VSync   : Boolean;
  scr_ResList : zglTResolutionList;
  
  // Resolution Correct
  scr_ResCX   : Single = 1;
  scr_ResCY   : Single = 1;
  scr_AddCX   : Integer = 0;
  scr_AddCY   : Integer = 0;
  scr_SubCX   : Integer = 0;
  scr_SubCY   : Integer = 0;
  
  {$IFDEF LINUX}
  scr_Display  : PDisplay;
  scr_Default  : cint;
  
  scr_Settings  : TXF86VidModeModeInfo;
  scr_Desktop   : TXF86VidModeModeInfo;
  scr_ModeList  : array of PXF86VidModeModeInfo;
  scr_ModeCount : DWORD;
  {$ENDIF}
  {$IFDEF WIN32}
  scr_Settings : DEVMODE;
  scr_Desktop  : DEVMODE;
  {$ENDIF}
  {---------------------------------- OpenGL ----------------------------------}
  ogl_InvertZ     : zglTMatrix4f;
  ogl_LastTexture : zglPTexture;

  ogl_zDepth     : Byte;
  ogl_Stencil    : Byte;
  ogl_FSAA       : Byte;
  ogl_Anisotropy : Byte;
  ogl_FOVY       : Single = 45;
  ogl_zNear      : Single = 0.1;
  ogl_zFar       : Single = 100;
  ogl_MTexActive : array[ 0..8 ] of Boolean;
  ogl_MTexture   : array[ 0..8 ] of DWORD;
  
  ogl_Mode : WORD = 3; // 2D/3D Modes

  ogl_X      : WORD;
  ogl_Y      : WORD;
  ogl_Width  : WORD = defWidth;
  ogl_Height : WORD = defHeight;
  ogl_CropX  : WORD;
  ogl_CropY  : WORD;
  ogl_CropW  : WORD;
  ogl_CropH  : WORD;

  ogl_3DAccelerator : Boolean;
  ogl_CanVSync      : Boolean;
  ogl_CanCompress   : Boolean;
  ogl_CanARB        : Boolean; // ARBvp/ARBfp шейдеры
  ogl_CanGLSL       : Boolean; // GLSL шейдеры
  ogl_CanVBO        : Boolean;
  ogl_CanFBO        : Boolean;
  ogl_CanPBuffer    : Boolean;
  ogl_MaxLights     : Integer;
  ogl_MaxTexSize    : Integer;
  ogl_MaxAnisotropy : Integer;
  ogl_MaxTexLevels  : Integer;
  
  {$IFDEF LINUX}
  ogl_Context    : GLXContext;
  ogl_VisualInfo : PXVisualInfo;
  ogl_Attr       : array[ 0..31 ] of DWORD;
  {$ENDIF}
  {$IFDEF WIN32}
  ogl_Context : HGLRC;
  ogl_fAttr   : array[ 0..1  ] of Single = ( 0, 0 );
  ogl_iAttr   : array[ 0..31 ] of Integer;
  ogl_Format  : Integer;
  ogl_Formats : DWORD;
  {$ENDIF}
  {$IFDEF DARWIN}
  ogl_Context : TAGLContext;
  ogl_Format  : TAGLPixelFormat;
  ogl_Attr    : array[ 0..31 ] of DWORD;
  {$ENDIF}

  {---------------------------------- Sound -----------------------------------}
  sndInitialized : Boolean = FALSE;
  sndVolume      : Byte    = 100;
  sndCanPlay     : Boolean = TRUE;
  sndCanPlayFile : Boolean = TRUE;
  sndStopFile    : Boolean = FALSE;
  sndLoopFile    : Boolean = FALSE;

  {$IFDEF LINUX_OR_DARWIN}
  oal_Device  : TALCdevice  = nil;
  oal_Context : TALCcontext = nil;

  // Параметры слушателя
  oal_Position    : array[ 0..2 ] of TALfloat = ( 0.0, 0.0, 0.0);  //позиция
  oal_Velocity    : array[ 0..2 ] of TALfloat = ( 0.0, 0.0, 0.0 ); //движение
  oal_Orientation : array[ 0..5 ] of TALfloat = ( 0.0, 0.0, -1.0, 0.0, 1.0, 0.0 ); //ориентация
  {$ENDIF}
  {$IFDEF WIN32}
  ds_Device : IDirectSound;

  ds_Position    : zglTPoint3D;
  ds_Plane       : zglTPoint3D;
  ds_Orientation : array[ 0..5 ] of Single = ( 0.0, 0.0, -1.0, 0.0, 1.0, 0.0 );
  {$ENDIF}

  sfStream   : zglPSoundFile = nil;
  {$IFDEF LINUX_OR_DARWIN}
  sfFormat   : array[ 1..2 ] of TALenum = ( AL_FORMAT_MONO16, AL_FORMAT_STEREO16 );
  sfBufCount : Integer = 4;
  sfSource   : TALuint;
  sfBuffers  : array[ 0..3 ] of TALuint;
  {$ENDIF}
  {$IFDEF WIN32}
  sfBufferRead : array[ 0..3 ] of Boolean = ( FALSE, FALSE, FALSE, FALSE );
  sfBuffer     : IDirectSoundBuffer;
  sfBufferPos  : Integer;
  sfBufferPrev : Integer;
  sfBufferNext : Integer;
  sfCurrPos    : DWORD;
  {$ENDIF}

implementation

procedure zero;
begin
end;

end.
