{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.inf.ua
 *
 * This file is part of ZenGL
 *
 * ZenGL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * ZenGL is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_opengl_simple;

{$I zgl_config.cfg}

interface

procedure Set2DMode;
procedure Set3DMode( const FOVY : Single = 45 );
procedure SetCurrentMode;

procedure zbuffer_SetDepth( const zNear, zFar : Single );
procedure zbuffer_Clear;

procedure scissor_Begin( X, Y, Width, Height : Integer );
procedure scissor_End;

implementation
uses
  zgl_application,
  zgl_main,
  zgl_window,
  zgl_screen,
  zgl_render_2d,
  zgl_camera_2d,
  zgl_opengl,
  zgl_opengl_all;

var
  tSCount  : Integer;
  tScissor : array of array[ 0..3 ] of Integer;

procedure Set2DMode;
begin
  if cam2dApply Then cam2d_Apply( nil );
  if ogl_Mode <> 1 Then ogl_Mode := 2;

  glDisable( GL_DEPTH_TEST );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;
  if app_Flags and CORRECT_RESOLUTION > 0 Then
    glOrtho( 0, ogl_Width - scr_AddCX * 2 / scr_ResCX, ogl_Height - scr_AddCY * 2 / scr_ResCY, 0, -1, 1 )
  else
    glOrtho( 0, wnd_Width, wnd_Height, 0, -1, 1 );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  scr_SetViewPort;
end;

procedure Set3DMode;
begin
  if cam2dApply Then cam2d_Apply( nil );
  if ogl_Mode <> 1 Then ogl_Mode := 3;
  ogl_FOVY := FOVY;

  glColor4ub( 255, 255, 255, 255 );

  glEnable( GL_DEPTH_TEST );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;
  gluPerspective( ogl_FOVY, ogl_Width / ogl_Height, ogl_zNear, ogl_zFar );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  scr_SetViewPort;
end;

procedure SetCurrentMode;
begin
  if ogl_Mode = 2 Then
    Set2DMode
  else
    Set3DMode( ogl_FOVY );
end;

procedure zbuffer_SetDepth;
begin
  ogl_zNear := zNear;
  ogl_zFar  := zFar;
end;

procedure zbuffer_Clear;
begin
  glClear( GL_DEPTH_BUFFER_BIT );
end;

procedure scissor_Begin;
begin
  if b2d_Started Then
    batch2d_Flush;
  if ( Width < 0 ) or ( Height < 0 ) Then
    exit;
  if cam2DGlobal <> @constCamera2D Then
    begin
      X      := Trunc( ( X - cam2dGlobal.X ) * cam2dGlobal.Zoom.X + ( ( ogl_Width  / 2 ) - ( ogl_Width  / 2 ) * cam2dGlobal.Zoom.X ) );
      Y      := Trunc( ( Y - cam2dGlobal.Y ) * cam2dGlobal.Zoom.Y + ( ( ogl_Height / 2 ) - ( ogl_Height / 2 ) * cam2dGlobal.Zoom.Y ) );
      Width  := Trunc( Width  * cam2DGlobal.Zoom.X );
      Height := Trunc( Height * cam2DGlobal.Zoom.Y );
    end;
  if app_Flags and CORRECT_RESOLUTION > 0 Then
    begin
      X      := Round( X * scr_ResCX + scr_AddCX );
      Y      := Round( Y * scr_ResCY + scr_AddCY );
      Width  := Round( Width * scr_ResCX );
      Height := Round( Height * scr_ResCY );
    end;
  glEnable( GL_SCISSOR_TEST );
  glScissor( X, wnd_Height - Y - Height, Width, Height );

  INC( tSCount );
  SetLength( tScissor, tSCount );
  tScissor[ tSCount - 1 ][ 0 ] := ogl_CropX;
  tScissor[ tSCount - 1 ][ 1 ] := ogl_CropY;
  tScissor[ tSCount - 1 ][ 2 ] := ogl_CropW;
  tScissor[ tSCount - 1 ][ 3 ] := ogl_CropH;

  ogl_CropX := X;
  ogl_CropY := Y;
  ogl_CropW := Width;
  ogl_CropH := Height;
end;

procedure scissor_End;
begin
  if b2d_Started Then
    batch2d_Flush;
  if tSCount - 1 < 0 Then
    exit;
  DEC( tSCount );
  ogl_CropX := tScissor[ tSCount ][ 0 ];
  ogl_CropY := tScissor[ tSCount ][ 1 ];
  ogl_CropW := tScissor[ tSCount ][ 2 ];
  ogl_CropH := tScissor[ tSCount ][ 3 ];
  SetLength( tScissor, tSCount );

  if tSCount > 0 Then
    begin
      glEnable( GL_SCISSOR_TEST );
      glScissor( ogl_CropX, wnd_Height - ogl_CropY - ogl_CropH, ogl_CropW, ogl_CropH );
    end else
      glDisable( GL_SCISSOR_TEST );
end;

end.
