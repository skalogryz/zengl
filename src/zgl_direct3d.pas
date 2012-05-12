{
 *  Copyright © Andrey Kemka aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://zengl.org
 *
 *  This file is part of ZenGL.
 *
 *  ZenGL is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *  published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *
 *  ZenGL is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with ZenGL. If not, see http://www.gnu.org/licenses/
}
unit zgl_direct3d;

{$I zgl_config.cfg}

interface
uses
  Windows,
  {$IFDEF USE_DIRECT3D8}
  DirectXGraphics
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  Direct3D9
  {$ENDIF}
  ;

const
  TARGET_SCREEN  = 1;
  TARGET_TEXTURE = 2;

function  d3d_Create : Boolean;
procedure d3d_Destroy;
function  d3d_Restore : Boolean;
procedure d3d_ResetState;
{$IFDEF USE_DIRECT3D8}
function  d3d_GetFormatID( Format : DWORD ) : DWORD;
{$ENDIF}
{$IFDEF USE_DIRECT3D9}
function  d3d_GetFormatID( Format : TD3DFormat ) : DWORD;
{$ENDIF}
function  d3d_CheckFSAA : TD3DMultiSampleType;

function  d3d_BeginScene : Boolean;
procedure d3d_EndScene;

var
  {$IFDEF USE_DIRECT3D8}
  d3d         : IDirect3D8;
  d3dDevice   : IDirect3DDevice8;
  d3dSurface  : IDirect3DSurface8;
  d3dStencil  : IDirect3DSurface8;
  d3dViewport : TD3DViewport8;
  d3dCaps     : TD3DCaps8;
  d3dAdapter  : TD3DAdapterIdentifier8;
  d3dMode     : TD3DDisplayMode;
  d3dFormat   : TD3DFormat = D3DFMT_UNKNOWN;
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  d3d         : IDirect3D9;
  d3dDevice   : IDirect3DDevice9;
  d3dSurface  : IDirect3DSurface9;
  d3dStencil  : IDirect3DSurface9;
  d3dViewport : TD3DViewport9;
  d3dCaps     : TD3DCaps9;
  d3dAdapter  : TD3DAdapterIdentifier9;
  d3dMode     : TD3DDisplayMode;
  d3dFormat   : TD3DFormat = D3DFMT_UNKNOWN;
  {$ENDIF}

  d3dParams   : TD3DPresentParameters;
  d3dParamsW  : TD3DPresentParameters;
  d3dParamsF  : TD3DPresentParameters;

  d3dCanDraw : Boolean;

  oglzDepth     : Byte;
  oglStencil    : Byte;
  oglFSAA       : Byte;
  oglAnisotropy : Byte;
  oglFOVY       : Single = 45;
  oglzNear      : Single = 0.1;
  oglzFar       : Single = 100;
  oglMTexActive : array[ 0..8 ] of Boolean;
  oglMTexture   : array[ 0..8 ] of DWORD;

  oglMode    : Integer = 2; // 2D/3D Modes
  oglTarget  : Integer = TARGET_SCREEN;
  oglTargetW : Integer;
  oglTargetH : Integer;
  oglWidth   : Integer;
  oglHeight  : Integer;

  oglVRAMUsed : LongWord;

  oglCanAnisotropy : Boolean;
  oglCanS3TC       : Boolean;
  oglCanAutoMipMap : Boolean;
  oglMaxTexSize    : Integer;
  oglMaxAnisotropy : Integer;
  oglMaxTexUnits   : Integer;
  oglSeparate      : Boolean;

implementation
uses
  zgl_direct3d_all,
  zgl_application,
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_camera_2d,
  zgl_render_2d,
  zgl_textures,
  zgl_render_target,
  zgl_log,
  zgl_utils;

var
  tSCount  : Integer;
  tScissor : array of array[ 0..3 ] of Integer;

function d3d_Create : Boolean;
  var
    i, modeCount : Integer;
begin
  Result := FALSE;

  {$IFDEF USE_DIRECT3D8}
  d3d := Direct3DCreate8( D3D_SDK_VERSION );
  if not Assigned( d3d ) Then
    begin
      u_Error( 'Direct3DCreate8 Error' );
      exit;
    end else log_Add( 'Direct3DCreate8' );

  d3d.GetAdapterIdentifier( D3DADAPTER_DEFAULT, D3DENUM_NO_WHQL_LEVEL, d3dAdapter );
  log_Add( 'D3D8_RENDERER: ' + d3dAdapter.Description );
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  d3d := Direct3DCreate9( D3D_SDK_VERSION );
  if not Assigned( d3d ) Then
    begin
      u_Error( 'Direct3DCreate9 Error' );
      exit;
    end else log_Add( 'Direct3DCreate9' );

  d3d.GetAdapterIdentifier( D3DADAPTER_DEFAULT, 0, d3dAdapter );
  log_Add( 'D3D9_RENDERER: ' + d3dAdapter.Description );
  {$ENDIF}

  d3d.GetDeviceCaps( D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, d3dCaps );
  oglMaxTexSize    := d3dCaps.MaxTextureWidth;
  oglCanAnisotropy := d3dCaps.RasterCaps and D3DPRASTERCAPS_ANISOTROPY > 0;
  oglMaxAnisotropy := d3dCaps.MaxAnisotropy;
  oglMaxTexUnits   := d3dCaps.MaxSimultaneousTextures;
  {$IFDEF USE_DIRECT3D8}
  oglCanAutoMipMap := FALSE;
  log_Add( 'D3D8_MAX_TEXTURE_SIZE: ' + u_IntToStr( oglMaxTexSize ) );
  log_Add( 'D3D8_TEXTURE_FILTER_ANISOTROPIC: ' + u_BoolToStr( oglCanAnisotropy ) );
  log_Add( 'D3D8_MAX_TEXTURE_ANISOTROPY: ' + u_IntToStr( oglMaxAnisotropy ) );
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  oglCanAutoMipMap := d3dCaps.Caps2 and D3DCAPS2_CANAUTOGENMIPMAP > 0;
  log_Add( 'D3D9_AUTOGENMIPMAP: ' + u_BoolToStr( oglCanAutoMipMap ) );
  log_Add( 'D3D9_MAX_TEXTURE_SIZE: ' + u_IntToStr( oglMaxTexSize ) );
  log_Add( 'D3D9_TEXTURE_FILTER_ANISOTROPIC: ' + u_BoolToStr( oglCanAnisotropy ) );
  log_Add( 'D3D9_MAX_TEXTURE_ANISOTROPY: ' + u_IntToStr( oglMaxAnisotropy ) );
  {$ENDIF}

  {$IFDEF USE_DIRECT3D8}
  oglSeparate := FALSE;
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  oglSeparate := d3dCaps.PrimitiveMiscCaps and D3DPMISCCAPS_SEPARATEALPHABLEND > 0;
  log_Add( 'D3DPMISCCAPS_SEPARATEALPHABLEND: ' + u_BoolToStr( oglSeparate ) );
  {$ENDIF}

  // Windowed
  if ( d3d.GetAdapterDisplayMode( D3DADAPTER_DEFAULT, d3dMode ) <> D3D_OK ) or ( d3dMode.Format = D3DFMT_UNKNOWN ) Then
    begin
      u_Warning( 'GetAdapterDisplayMode = D3DFMT_UNKNOWN' );
      if not wndFullScreen Then exit;
    end;

  FillChar( d3dParamsW, SizeOf( TD3DPresentParameters ), 0 );
  with d3dParamsW do
    begin
      BackBufferWidth  := wndWidth;
      BackBufferHeight := wndHeight;
      BackBufferFormat := d3dMode.Format;
      BackBufferCount  := 1;
      MultiSampleType  := D3DMULTISAMPLE_NONE;
      hDeviceWindow    := wndHandle;
      Windowed         := TRUE;
      {$IFDEF USE_DIRECT3D8}
      if scrVSync Then
        SwapEffect := D3DSWAPEFFECT_COPY_VSYNC
      else
        SwapEffect := D3DSWAPEFFECT_COPY;
      {$ENDIF}
      {$IFDEF USE_DIRECT3D9}
      SwapEffect := D3DSWAPEFFECT_COPY;
      if scrVSync Then
        PresentationInterval := D3DPRESENT_INTERVAL_ONE
      else
        PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
      {$ENDIF}
      EnableAutoDepthStencil := TRUE;
      AutoDepthStencilFormat := D3DFMT_D16;
    end;

  // FullScreen
  {$IFDEF USE_DIRECT3D8}
  modeCount := d3d.GetAdapterModeCount( D3DADAPTER_DEFAULT );
  for i := 0 to modeCount - 1 do
    begin
      d3d.EnumAdapterModes( D3DADAPTER_DEFAULT, i, d3dMode );
      if ( d3dMode.Width <> scrWidth ) or ( d3dMode.Height <> scrHeight) Then continue;
      if ( d3d_GetFormatID( d3dMode.Format ) > d3d_GetFormatID( d3dFormat ) ) Then d3dFormat := d3dMode.Format;
    end;
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  d3d.GetAdapterDisplayMode( D3DADAPTER_DEFAULT, d3dMode );
  d3dFormat := d3dMode.Format;
  modeCount := d3d.GetAdapterModeCount( D3DADAPTER_DEFAULT, d3dFormat );
  for i := 0 to modeCount - 1 do
    begin
      d3d.EnumAdapterModes( D3DADAPTER_DEFAULT, d3dFormat, i, d3dMode );
      if ( d3dMode.Width <> scrWidth ) or ( d3dMode.Height <> scrHeight) Then continue;
      if ( d3d_GetFormatID( d3dMode.Format ) > d3d_GetFormatID( d3dFormat ) ) Then d3dFormat := d3dMode.Format;
    end;
  {$ENDIF}

  if ( d3dFormat = D3DFMT_UNKNOWN ) and wndFullScreen Then
    begin
      u_Warning( 'Cannot set fullscreen mode' );
      wndFullScreen := FALSE;
      exit;
    end;

  FillChar( d3dParamsF, SizeOf( TD3DPresentParameters ), 0 );
  with d3dParamsF do
    begin
      BackBufferWidth  := scrWidth;
      BackBufferHeight := scrHeight;
      BackBufferFormat := d3dFormat;
      BackBufferCount  := 1;
      MultiSampleType  := d3d_CheckFSAA();
      hDeviceWindow    := wndHandle;
      Windowed         := FALSE;
      SwapEffect       := D3DSWAPEFFECT_DISCARD;
      {$IFDEF USE_DIRECT3D8}
      FullScreen_RefreshRateInHz := D3DPRESENT_RATE_DEFAULT;
      if scrVSync Then
        FullScreen_PresentationInterval := D3DPRESENT_INTERVAL_ONE
      else
        FullScreen_PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
      {$ENDIF}
      {$IFDEF USE_DIRECT3D9}
      if scrVSync Then
        PresentationInterval := D3DPRESENT_INTERVAL_ONE
      else
        PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
      {$ENDIF}
      EnableAutoDepthStencil := TRUE;
      AutoDepthStencilFormat := D3DFMT_D16;
    end;

  if wndFullScreen Then
    d3dParams := d3dParamsF
  else
    d3dParams := d3dParamsW;

  // D3D Device
  // D3DCREATE_HARDWARE_VERTEXPROCESSING
  if d3d.CreateDevice( D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, wndHandle, $40 or D3DCREATE_FPU_PRESERVE, d3dParams, d3dDevice ) <> D3D_OK Then
    begin
      //D3DCREATE_SOFTWARE_VERTEXPROCESSING
      if d3d.CreateDevice( D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, wndHandle, $20 or D3DCREATE_FPU_PRESERVE, d3dParams, d3dDevice ) <> D3D_OK Then
        begin
          //D3DCREATE_PUREDEVICE
          if d3d.CreateDevice( D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, wndHandle, $10 or D3DCREATE_FPU_PRESERVE, d3dParams, d3dDevice ) <> D3D_OK Then
            begin
              u_Error( 'Can''t create d3d device' );
              exit;
            end else log_Add( 'D3DCREATE_PUREDEVICE' );
        end else log_Add( 'D3DCREATE_SOFTWARE_VERTEXPROCESSING' );
    end else log_Add( 'D3DCREATE_HARDWARE_VERTEXPROCESSING' );

  d3d_ResetState();

  Result := TRUE;
end;

procedure d3d_Destroy;
  var
    i : Integer;
begin
  for i := 0 to d3dTexCount - 1 do
    d3dTexArray[ i ].Texture := nil;
  SetLength( d3dTexArray, 0 );
  for i := 0 to d3dTexCount - 1 do
    d3dResArray[ i ] := nil;
  SetLength( d3dResArray, 0 );

  //d3dDevice._Release;
  d3dDevice := nil;
  //d3d._Release;
  d3d       := nil;
end;

function d3d_Restore : Boolean;
  var
    r : zglPRenderTarget;
    t : zglPTexture;
begin
  Result := FALSE;
  if not Assigned( d3dDevice ) Then exit;

  r := managerRTarget.First.Next;
  while Assigned( r ) do
    begin
      r.Handle.Depth := nil;
      r := r.Next;
    end;
  t := managerTexture.First.Next;
  while Assigned( t ) do
    begin
      if d3dTexArray[ t.ID ].Pool = D3DPOOL_DEFAULT Then
        d3dTexArray[ t.ID ].Texture := nil;
      t := t.Next;
    end;

  d3dParamsW.BackBufferWidth  := wndWidth;
  d3dParamsW.BackBufferHeight := wndHeight;
  d3dParamsF.BackBufferWidth  := wndWidth;
  d3dParamsF.BackBufferHeight := wndHeight;
  d3dParamsF.MultiSampleType  := d3d_CheckFSAA();
  {$IFDEF USE_DIRECT3D8}
  if scrVSync Then
    begin
      d3dParamsW.SwapEffect := D3DSWAPEFFECT_COPY_VSYNC;
      d3dParamsF.FullScreen_PresentationInterval := D3DPRESENT_INTERVAL_ONE
    end else
      begin
        d3dParamsW.SwapEffect := D3DSWAPEFFECT_COPY;
        d3dParamsF.FullScreen_PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
      end;
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  if scrVSync Then
    begin
      d3dParamsW.PresentationInterval := D3DPRESENT_INTERVAL_ONE;
      d3dParamsF.PresentationInterval := D3DPRESENT_INTERVAL_ONE;
    end else
      begin
        d3dParamsW.PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
        d3dParamsF.PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
      end;
  {$ENDIF}

  if wndFullScreen Then
    d3dParams := d3dParamsF
  else
    d3dParams := d3dParamsW;

  d3dDevice.Reset( d3dParams );
  d3d_ResetState();

  r := managerRTarget.First.Next;
  while Assigned( r ) do
    begin
      if r.Flags and RT_USE_DEPTH > 0 Then
      {$IFDEF USE_DIRECT3D8}
      d3dDevice.CreateDepthStencilSurface( Round( r.Surface.Width / r.Surface.U ), Round( r.Surface.Height / r.Surface.V ), d3dParams.AutoDepthStencilFormat,
                                           D3DMULTISAMPLE_NONE, r.Handle.Depth );
      {$ENDIF}
      {$IFDEF USE_DIRECT3D9}
      d3dDevice.CreateDepthStencilSurface( Round( r.Surface.Width / r.Surface.U ), Round( r.Surface.Height / r.Surface.V ), d3dParams.AutoDepthStencilFormat,
                                           D3DMULTISAMPLE_NONE, 0, TRUE, r.Handle.Depth, nil );
      {$ENDIF}
      r := r.Next;
    end;
  t := managerTexture.First.Next;
  while Assigned( t ) do
    begin
      if not Assigned( d3dTexArray[ t.ID ].Texture ) Then
        begin
          {$IFDEF USE_DIRECT3D8}
          d3dDevice.CreateTexture( Round( t.Width / t.U ), Round( t.Height / t.V ), 1, D3DUSAGE_RENDERTARGET, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT,
                                   d3dTexArray[ t.ID ].Texture );
          {$ENDIF}
          {$IFDEF USE_DIRECT3D9}
          d3dDevice.CreateTexture( Round( t.Width / t.U ), Round( t.Height / t.V ), 1, D3DUSAGE_RENDERTARGET, D3DFMT_A8R8G8B8, D3DPOOL_DEFAULT,
                                   d3dTexArray[ t.ID ].Texture, nil );
          {$ENDIF}
          d3dTexArray[ t.ID ].Pool := D3DPOOL_DEFAULT;
          rtarget_Restore( t );
        end;
      t := t.Next;
    end;

  Result := TRUE;
end;

procedure d3d_ResetState;
begin
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
  glAlphaFunc( GL_GREATER, 0 );

  {$IFDEF USE_DIRECT3D9}
  if oglSeparate Then
    begin
      d3dDevice.SetRenderState( D3DRS_BLENDOP, D3DBLENDOP_ADD );
      glBlendFuncSeparate( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ONE_MINUS_SRC_ALPHA );
    end;
  {$ENDIF}

  glDisable( GL_BLEND );
  glDisable( GL_ALPHA_TEST );
  glDisable( GL_DEPTH_TEST );
  glDisable( GL_TEXTURE_2D );

  glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

  d3dDevice.SetRenderState( D3DRS_CULLMODE, D3DCULL_NONE );
  d3dDevice.SetRenderState( D3DRS_LIGHTING, iFALSE );
end;

function d3d_GetFormatID;
begin
  case Format of
    D3DFMT_X8R8G8B8: Result := 4;
    D3DFMT_A8R8G8B8: Result := 5;
  else
    Result := 0;
  end;
end;

function d3d_CheckFSAA : TD3DMultiSampleType;
  var
    fsaa : Integer;
begin
  fsaa := oglFSAA;
  if ( fsaa = 0 ) or ( fsaa = 1 ) Then
    Result := D3DMULTISAMPLE_NONE;
  if fsaa > 16 Then
    fsaa := 16;
  if wndFullScreen Then
    begin
      {$IFDEF USE_DIRECT3D8}
      while d3d.CheckDeviceMultiSampleType( D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, d3dFormat, FALSE, TD3DMultiSampleType( fsaa ) ) <> D3D_OK do
      {$ENDIF}
      {$IFDEF USE_DIRECT3D9}
      while d3d.CheckDeviceMultiSampleType( D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, d3dFormat, FALSE, TD3DMultiSampleType( fsaa ), nil ) <> D3D_OK do
      {$ENDIF}
        begin
          if fsaa = 1 Then break;
          DEC( fsaa );
        end;
    end else
      {$IFDEF USE_DIRECT3D8}
      while d3d.CheckDeviceMultiSampleType( D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, d3dMode.Format, TRUE, TD3DMultiSampleType( fsaa ) ) <> D3D_OK do
      {$ENDIF}
      {$IFDEF USE_DIRECT3D9}
      while d3d.CheckDeviceMultiSampleType( D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, d3dMode.Format, TRUE, TD3DMultiSampleType( fsaa ), nil ) <> D3D_OK do
      {$ENDIF}
        begin
          if fsaa = 1 Then break;
          DEC( fsaa );
        end;

  Result := TD3DMultiSampleType( fsaa );
end;

function d3d_BeginScene : Boolean;
  var
    hr : HRESULT;
begin
  if d3dCanDraw Then
    begin
      Result := TRUE;
      exit;
    end else
      Result := FALSE;

  hr := d3dDevice.TestCooperativeLevel();
  case hr of
    D3DERR_DEVICELOST: exit;
    D3DERR_DEVICENOTRESET:
      begin
        if not wndFullScreen Then
          begin
            if ( d3d.GetAdapterDisplayMode( D3DADAPTER_DEFAULT, d3dMode ) <> D3D_OK ) or ( d3dMode.Format = D3DFMT_UNKNOWN ) Then
              begin
                u_Warning( 'GetAdapterDisplayMode = D3DFMT_UNKNOWN' );
                exit;
              end;

            d3dParamsW.BackBufferFormat := d3dMode.Format;
          end;

        d3d_Restore();
        exit;
      end;
  end;

  if d3dDevice.BeginScene() <> D3D_OK Then exit;
  d3dCanDraw := TRUE;

  Result := TRUE;
end;

procedure d3d_EndScene;
begin
  d3dCanDraw := FALSE;
  d3dDevice.EndScene();
  d3dDevice.Present( nil, nil, 0, nil );
end;

end.
