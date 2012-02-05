{
 *  Copyright © Kemka Andrey aka Andru
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
unit zgl_render_target;

{$I zgl_config.cfg}

interface
uses
  Windows,
  {$IFDEF USE_DIRECT3D8}
  DirectXGraphics,
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  Direct3D9,
  {$ENDIF}
  zgl_direct3d,
  zgl_direct3d_all,
  zgl_textures;

const
  RT_DEFAULT      = $00;
  RT_FULL_SCREEN  = $01;
  RT_USE_DEPTH    = $02;
  RT_CLEAR_COLOR  = $04;
  RT_CLEAR_DEPTH  = $08;
  RT_SAVE_CONTENT = $10;

type
  zglPD3DTarget = ^zglTD3DTarget;
  zglTD3DTarget = record
    Old   : zglPTexture;
    {$IFDEF USE_DIRECT3D8}
    Depth : IDirect3DSurface8;
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    Depth : IDirect3DSurface9;
    {$ENDIF}
  end;

type
  zglPRenderTarget = ^zglTRenderTarget;
  zglTRenderTarget = record
    _type   : Byte;
    Handle  : zglPD3DTarget;
    Surface : zglPTexture;
    Flags   : Byte;

    prev, next : zglPRenderTarget;
  end;

type
  zglPRenderTargetManager = ^zglTRenderTargetManager;
  zglTRenderTargetManager = record
    Count : Integer;
    First : zglTRenderTarget;
  end;

type
  zglTRenderCallback = procedure( Data : Pointer );

function rtarget_Add( Surface : zglPTexture; Flags : Byte ) : zglPRenderTarget;
procedure rtarget_Del( var Target : zglPRenderTarget );
procedure rtarget_Set( Target : zglPRenderTarget );
procedure rtarget_DrawIn( Target : zglPRenderTarget; RenderCallback : zglTRenderCallback; Data : Pointer );

procedure rtarget_Save( Target : zglPTexture );
procedure rtarget_Restore( Target : zglPTexture );

var
  managerRTarget : zglTRenderTargetManager;

implementation
uses
  zgl_main,
  zgl_application,
  zgl_screen,
  zgl_sprite_2d,
  zgl_render_2d,
  zgl_camera_2d;

var
  lCanDraw : Boolean;
  lRTarget : zglPRenderTarget;
  {$IFDEF USE_DIRECT3D8}
  lSurface : IDirect3DSurface8;
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  lSurface : IDirect3DSurface9;
  {$ENDIF}
  lGLW     : Integer;
  lGLH     : Integer;
  lClipW   : Integer;
  lClipH   : Integer;
  lResCX   : Single;
  lResCY   : Single;

procedure rtarget_Save( Target : zglPTexture );
  var
    s, d : TD3DSurface_Desc;
    {$IFDEF USE_DIRECT3D8}
    src, dst : IDirect3DSurface8;
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    src : IDirect3DSurface9;
    {$ENDIF}
begin
  d3dResArray[ Target.ID ] := nil;
  {$IFDEF USE_DIRECT3D8}
  d3dTexArray[ Target.ID ].Texture.GetLevelDesc( 0, d );
  if Assigned( d3dResArray[ Target.ID ] ) Then
    begin
      d3dResArray[ Target.ID ].GetLevelDesc( 0, s );
      if ( s.Width < d.Width ) or ( s.Height < d.Height ) or ( s.Format <> d.Format ) Then
        d3dResArray[ Target.ID ] := nil;
    end;
  if not Assigned( d3dResArray[ Target.ID ] ) Then
    d3dDevice.CreateTexture( d.Width, d.Height, 1, 0, d.Format, D3DPOOL_MANAGED, d3dResArray[ Target.ID ] );

  d3dTexArray[ Target.ID ].Texture.GetSurfaceLevel( 0, src );
  d3dResArray[ Target.ID ].GetSurfaceLevel( 0, dst );
  d3dDevice.CopyRects( src, nil, 0, dst, nil );

  src := nil;
  dst := nil;
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  d3dTexArray[ Target.ID ].Texture.GetLevelDesc( 0, d );
  if Assigned( d3dResArray[ Target.ID ] ) Then
    begin
      d3dResArray[ Target.ID ].GetDesc( s );
      if ( s.Width < d.Width ) or ( s.Height < d.Height ) or ( s.Format <> d.Format ) Then
        d3dResArray[ Target.ID ] := nil;
    end;
  if not Assigned( d3dResArray[ Target.ID ] ) Then
    d3dDevice.CreateOffscreenPlainSurface( d.Width, d.Height, d.Format, D3DPOOL_SYSTEMMEM, d3dResArray[ Target.ID ], 0 );

  d3dTexArray[ Target.ID ].Texture.GetSurfaceLevel( 0, src );
  d3dDevice.GetRenderTargetData( src, d3dResArray[ Target.ID ] );

  src := nil;
  {$ENDIF}
end;

procedure rtarget_Restore( Target : zglPTexture );
  var
    {$IFDEF USE_DIRECT3D8}
    src, dst : IDirect3DSurface8;
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    dst : IDirect3DSurface9;
    {$ENDIF}
begin
  if not Assigned( d3dResArray[ Target.ID ] ) Then exit;
  {$IFDEF USE_DIRECT3D8}
  d3dTexArray[ Target.ID ].Texture.GetSurfaceLevel( 0, dst );
  d3dResArray[ Target.ID ].GetSurfaceLevel( 0, src );
  d3dDevice.CopyRects( src, nil, 0, dst, nil );

  src := nil;
  dst := nil;
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  d3dTexArray[ Target.ID ].Texture.GetSurfaceLevel( 0, dst );
  d3dDevice.UpdateSurface( d3dResArray[ Target.ID ], nil, dst, nil );

  dst := nil;
  {$ENDIF}
end;

function rtarget_Add( Surface : zglPTexture; Flags : Byte ) : zglPRenderTarget;
begin
  Result := @managerRTarget.First;
  while Assigned( Result.Next ) do
    Result := Result.Next;

  zgl_GetMem( Pointer( Result.Next ), SizeOf( zglTRenderTarget ) );
  zgl_GetMem( Pointer( Result.Next.Handle ), SizeOf( zglTD3DTarget ) );

  rtarget_Save( Surface );
  d3dTexArray[ Surface.ID ].Texture := nil;
  {$IFDEF USE_DIRECT3D8}
  d3dDevice.CreateTexture( Round( Surface.Width / Surface.U ), Round( Surface.Height / Surface.V ), 1, D3DUSAGE_RENDERTARGET, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT,
                           d3dTexArray[ Surface.ID ].Texture );
  if Flags and RT_USE_DEPTH > 0 Then
    d3dDevice.CreateDepthStencilSurface( Round( Surface.Width / Surface.U ), Round( Surface.Height / Surface.V ), d3dParams.AutoDepthStencilFormat,
                                         D3DMULTISAMPLE_NONE, Result.Next.Handle.Depth );
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  d3dDevice.CreateTexture( Round( Surface.Width / Surface.U ), Round( Surface.Height / Surface.V ), 1, D3DUSAGE_RENDERTARGET, D3DFMT_A8R8G8B8, D3DPOOL_DEFAULT,
                           d3dTexArray[ Surface.ID ].Texture, nil );
  if Flags and RT_USE_DEPTH > 0 Then
    d3dDevice.CreateDepthStencilSurface( Round( Surface.Width / Surface.U ), Round( Surface.Height / Surface.V ), d3dParams.AutoDepthStencilFormat,
                                         D3DMULTISAMPLE_NONE, 0, TRUE, Result.Next.Handle.Depth, nil );
  {$ENDIF}
  d3dTexArray[ Surface.ID ].Pool := D3DPOOL_DEFAULT;
  rtarget_Restore( Surface );

  Result.next._type      := 0;
  Result.next.Handle.Old := Surface;
  Result.next.Surface    := Surface;
  Result.next.Flags      := Flags;
  Result.next.prev       := Result;
  Result.next.next       := nil;
  Result                 := Result.next;
  INC( managerRTarget.Count );
end;

procedure rtarget_Del( var Target : zglPRenderTarget );
begin
  if not Assigned( Target ) Then exit;

  tex_Del( Target.Surface );

  if Assigned( Target.prev ) Then
    Target.prev.next := Target.next;
  if Assigned( Target.Next ) Then
    Target.next.prev := Target.prev;

  Target.Handle.Depth := nil;
  FreeMem( Target.Handle );
  FreeMem( Target );
  Target := nil;

  DEC( managerRTarget.Count );
end;

procedure rtarget_Set( Target : zglPRenderTarget );
  var
    d : TD3DSurface_Desc;
begin
  batch2d_Flush();

  if Assigned( Target ) Then
    begin
      lCanDraw := d3dCanDraw;
      d3d_BeginScene();
      lRTarget   := Target;
      lGLW       := oglWidth;
      lGLH       := oglHeight;
      lClipW     := oglClipW;
      lClipH     := oglClipH;
      lResCX     := scrResCX;
      lResCY     := scrResCY;

      if Target.Surface <> Target.Handle.Old Then
        begin
          d3dTexArray[ Target.Surface.ID ].Texture.GetLevelDesc( 0, d );
          if d.Pool <> D3DPOOL_DEFAULT Then
            begin
              Target.Handle.Old := Target.Surface;
              rtarget_Save( Target.Surface );
              d3dTexArray[ Target.Surface.ID ].Texture := nil;
              Target.Handle.Depth := nil;
              {$IFDEF USE_DIRECT3D8}
              d3dDevice.CreateTexture( d.Width, d.Height, 1, D3DUSAGE_RENDERTARGET, d.Format, D3DPOOL_DEFAULT, d3dTexArray[ Target.Surface.ID ].Texture );
              if Target.Flags and RT_USE_DEPTH > 0 Then
                d3dDevice.CreateDepthStencilSurface( d.Width, d.Height, d3dParams.AutoDepthStencilFormat, D3DMULTISAMPLE_NONE, Target.Handle.Depth );
              {$ENDIF}
              {$IFDEF USE_DIRECT3D9}
              d3dDevice.CreateTexture( d.Width, d.Height, 1, D3DUSAGE_RENDERTARGET, d.Format, D3DPOOL_DEFAULT, d3dTexArray[ Target.Surface.ID ].Texture, nil );
              if Target.Flags and RT_USE_DEPTH > 0 Then
                d3dDevice.CreateDepthStencilSurface( d.Width, d.Height, d3dParams.AutoDepthStencilFormat, D3DMULTISAMPLE_NONE, 0, TRUE, Target.Handle.Depth,
                                                      nil );
              {$ENDIF}
              d3dTexArray[ Target.Surface.ID ].Pool := D3DPOOL_DEFAULT;
              rtarget_Restore( Target.Surface );
            end;
        end;
      {$IFDEF USE_DIRECT3D8}
      d3dDevice.GetRenderTarget( d3dSurface );
      d3dDevice.GetDepthStencilSurface( d3dStencil );
      d3dTexArray[ Target.Surface.ID ].Texture.GetSurfaceLevel( 0, lSurface );
      if Target.Flags and RT_USE_DEPTH > 0 Then
        d3dDevice.SetRenderTarget( lSurface, Target.Handle.Depth )
      else
        d3dDevice.SetRenderTarget( lSurface, nil );
      {$ENDIF}
      {$IFDEF USE_DIRECT3D9}
      d3dDevice.GetDepthStencilSurface( d3dStencil );
      d3dDevice.GetRenderTarget( 0, d3dSurface );
      d3dTexArray[ Target.Surface.ID ].Texture.GetSurfaceLevel( 0, lSurface );
      d3dDevice.SetRenderTarget( 0, lSurface );
      if Target.Flags and RT_USE_DEPTH > 0 Then
        d3dDevice.SetDepthStencilSurface( Target.Handle.Depth )
      else
        d3dDevice.SetDepthStencilSurface( nil );
      {$ENDIF}

      oglTarget  := TARGET_TEXTURE;
      oglTargetW := Target.Surface.Width;
      oglTargetH := Target.Surface.Height;
      if Target.Flags and RT_FULL_SCREEN > 0 Then
        begin
          if appFlags and CORRECT_RESOLUTION > 0 Then
            begin
              oglWidth  := scrResW;
              oglHeight := scrResH;
            end;
        end else
          begin
            oglWidth  := Target.Surface.Width;
            oglHeight := Target.Surface.Height;
            scrResCX  := 1;
            scrResCY  := 1;
          end;
      SetCurrentMode();

      if Target.Flags and RT_CLEAR_COLOR > 0 Then
        d3dDevice.Clear( 0, nil, D3DCLEAR_TARGET, D3DCOLOR_ARGB( 0, 0, 0, 0 ), 1, 0 );
      if Target.Flags and RT_CLEAR_DEPTH > 0 Then
        d3dDevice.Clear( 0, nil, D3DCLEAR_ZBUFFER, D3DCOLOR_ARGB( 0, 0, 0, 0 ), 1, 0 );
    end else
      if Assigned( lRTarget ) Then
        begin
          {$IFDEF USE_DIRECT3D8}
          d3dDevice.SetRenderTarget( d3dSurface, d3dStencil );
          {$ENDIF}
          {$IFDEF USE_DIRECT3D9}
          d3dDevice.SetRenderTarget( 0, d3dSurface );
          d3dDevice.SetDepthStencilSurface( d3dStencil );
          {$ENDIF}
          lSurface   := nil;
          d3dSurface := nil;
          d3dStencil := nil;

          if lRTarget.Flags and RT_SAVE_CONTENT > 0 Then
            rtarget_Save( lRTarget.Surface );

          oglTarget  := TARGET_SCREEN;
          oglWidth   := lGLW;
          oglHeight  := lGLH;
          oglTargetW := oglWidth;
          oglTargetH := oglHeight;
          if lRTarget.Flags and RT_FULL_SCREEN = 0 Then
            begin
              oglClipW := lClipW;
              oglClipH := lClipH;
              scrResCX := lResCX;
              scrResCY := lResCY;
            end;

          lRTarget := nil;
          SetCurrentMode();
          if not lCanDraw then
            d3d_EndScene();
        end;
end;

procedure rtarget_DrawIn( Target : zglPRenderTarget; RenderCallback : zglTRenderCallback; Data : Pointer );
begin
  if oglSeparate Then
    begin
      rtarget_Set( Target );
      RenderCallback( Data );
      rtarget_Set( nil );
    end else
      begin
        rtarget_Set( Target );

        glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
        glColorMask( GL_TRUE, GL_TRUE, GL_TRUE, GL_FALSE );
        RenderCallback( Data );
        batch2d_Flush();

        glBlendFunc( GL_ONE, GL_ONE_MINUS_SRC_ALPHA );
        glColorMask( GL_FALSE, GL_FALSE, GL_FALSE, GL_TRUE );
        RenderCallback( Data );

        rtarget_Set( nil );

        glColorMask( GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE );
        glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
      end;
end;

end.
