{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://andru-kun.inf.ua
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
unit zgl_opengl_simple;

{$I zgl_config.cfg}

interface

procedure Set2DMode;
procedure Set3DMode( FOVY : Single = 45 );
procedure SetCurrentMode;

procedure zbuffer_SetDepth( zNear, zFar : Single );
procedure zbuffer_Clear;

procedure scissor_Begin( X, Y, Width, Height : Integer );
procedure scissor_End;

implementation
uses
  zgl_application,
  zgl_main,
  zgl_window,
  zgl_screen,
  zgl_opengl,
  zgl_opengl_all,
  zgl_render_2d,
  zgl_camera_2d;

var
  tSCount  : Integer;
  tScissor : array of array[ 0..3 ] of Integer;

procedure Set2DMode;
begin
  ogl_Mode := 2;
  batch2d_Flush();
  if cam2d.Apply Then glPopMatrix();
  cam2d := @cam2dTarget[ ogl_Target ];

  glDisable( GL_DEPTH_TEST );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity();
  if ogl_Target = TARGET_SCREEN Then
    begin
      if app_Flags and CORRECT_RESOLUTION > 0 Then
        glOrtho( 0, Round( ogl_Width - scr_AddCX * 2 / scr_ResCX ), Round( ogl_Height - scr_AddCY * 2 / scr_ResCY ), 0, -1, 1 )
      else
        glOrtho( 0, wnd_Width, wnd_Height, 0, -1, 1 );
    end else
      glOrtho( 0, ogl_Width, ogl_Height, 0, -1, 1 );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity();
  scr_SetViewPort();

  if cam2d.Apply Then cam2d_Set( cam2d.Global );
end;

procedure Set3DMode( FOVY : Single = 45 );
begin
  ogl_Mode := 3;
  ogl_FOVY := FOVY;
  batch2d_Flush();
  if cam2d.Apply Then glPopMatrix();
  cam2d := @cam2dTarget[ ogl_Target ];

  glColor4ub( 255, 255, 255, 255 );

  glEnable( GL_DEPTH_TEST );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity();
  gluPerspective( ogl_FOVY, ogl_Width / ogl_Height, ogl_zNear, ogl_zFar );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity();
  scr_SetViewPort();
end;

procedure SetCurrentMode;
begin
  if ogl_Mode = 2 Then
    Set2DMode()
  else
    Set3DMode( ogl_FOVY );
end;

procedure zbuffer_SetDepth( zNear, zFar : Single );
begin
  ogl_zNear := zNear;
  ogl_zFar  := zFar;
end;

procedure zbuffer_Clear;
begin
  batch2d_Flush();
  glClear( GL_DEPTH_BUFFER_BIT );
end;

procedure scissor_Begin( X, Y, Width, Height : Integer );
begin
  batch2d_Flush();

  if ( Width < 0 ) or ( Height < 0 ) Then exit;
  if not cam2d.OnlyXY Then
    begin
      X      := Trunc( ( X - cam2d.Global.X ) * cam2d.Global.Zoom.X + ( ( ogl_Width  / 2 ) - ( ogl_Width  / 2 ) * cam2d.Global.Zoom.X ) );
      Y      := Trunc( ( Y - cam2d.Global.Y ) * cam2d.Global.Zoom.Y + ( ( ogl_Height / 2 ) - ( ogl_Height / 2 ) * cam2d.Global.Zoom.Y ) );
      Width  := Trunc( Width  * cam2D.Global.Zoom.X );
      Height := Trunc( Height * cam2D.Global.Zoom.Y );
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
  tScissor[ tSCount - 1 ][ 0 ] := ogl_ClipX;
  tScissor[ tSCount - 1 ][ 1 ] := ogl_ClipY;
  tScissor[ tSCount - 1 ][ 2 ] := ogl_ClipW;
  tScissor[ tSCount - 1 ][ 3 ] := ogl_ClipH;

  ogl_ClipX := X;
  ogl_ClipY := Y;
  ogl_ClipW := Width;
  ogl_ClipH := Height;
end;

procedure scissor_End;
begin
  batch2d_Flush();

  if tSCount - 1 < 0 Then exit;
  DEC( tSCount );
  ogl_ClipX := tScissor[ tSCount ][ 0 ];
  ogl_ClipY := tScissor[ tSCount ][ 1 ];
  ogl_ClipW := tScissor[ tSCount ][ 2 ];
  ogl_ClipH := tScissor[ tSCount ][ 3 ];
  SetLength( tScissor, tSCount );

  if tSCount > 0 Then
    begin
      glEnable( GL_SCISSOR_TEST );
      glScissor( ogl_ClipX, wnd_Height - ogl_ClipY - ogl_ClipH, ogl_ClipW, ogl_ClipH );
    end else
      glDisable( GL_SCISSOR_TEST );
end;

end.
