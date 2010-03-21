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
unit zgl_camera_2d;

{$I zgl_config.cfg}
{$IFDEF LINUX_OR_DARWIN}
  {$DEFINE stdcall := cdecl}
{$ENDIF}

interface
uses
  zgl_math_2d;

type
  zglPCamera2D = ^zglTCamera2D;
  zglTCamera2D = record
    X, Y  : Single;
    Angle : Single;
    Zoom  : zglTPoint2D;
end;

procedure cam2d_Set( const Camera : zglPCamera2D );
procedure cam2d_Apply( const Camera : zglPCamera2D );

procedure cam2d_Vertex2f( X, Y : Single ); stdcall;
procedure cam2d_Vertex2fv( V : Pointer ); stdcall;

var
  cam2dApply    : Boolean;
  cam2dZoomX    : Single;
  cam2dZoomY    : Single;
  cam2dAngle    : Single;
  cam2dCos      : Single;
  cam2dSin      : Single;
  cam2dGlobal   : zglPCamera2D = nil;
  constCamera2D : zglTCamera2D = ( X: 0; Y: 0; Angle: 0; Zoom: ( X: 1; Y: 1 ) );
  cam2dSX       : Integer = 0;
  cam2dSY       : Integer = 0;

implementation
uses
  zgl_types,
  zgl_screen,
  zgl_opengl,
  zgl_opengl_all,
  zgl_render_2d;

procedure cam2d_Set;
begin
  cam2dGlobal := Camera;
  if Camera = nil Then
    begin
      cam2dGlobal  := @constCamera2D;
      gl_Vertex2f  := @glVertex2f;
      gl_Vertex2fv := @glVertex2fv;
      sprite2d_InScreen := sprite2d_InScreenSimple;
    end else
      begin
        gl_Vertex2f  := @cam2d_Vertex2f;
        gl_Vertex2fv := @cam2d_Vertex2fv;
        sprite2d_InScreen := sprite2d_InScreenCamera;
      end;
end;

procedure cam2d_Apply;
begin
  batch2d_Flush();

  if cam2dApply Then
    glPopMatrix();

  if Assigned( Camera ) Then
    begin
      cam2dApply  := TRUE;
      cam2dGlobal := Camera;

      glPushMatrix();
      if ( Camera.Angle <> 0 ) or ( Camera.Zoom.X <> 1 ) or ( Camera.Zoom.Y <> 1 ) Then
        begin
          glTranslatef( ogl_Width / 2 - scr_AddCX / scr_ResCX, ogl_Height / 2 - scr_AddCY / scr_ResCY, 0 );
          if ( Camera.Zoom.X <> 1 ) or ( Camera.Zoom.Y <> 1 ) Then
            glScalef( Camera.Zoom.X, Camera.Zoom.Y, 1 );
          if Camera.Angle <> 0 Then
            glRotatef( Camera.Angle, 0, 0, 1 );
          glTranslatef( -ogl_Width / 2 + scr_AddCX / scr_ResCX, -ogl_Height / 2 + scr_AddCY / scr_ResCY, 0 );
        end;
      if ( Camera.X <> 0 ) or ( Camera.Y <> 0 ) Then
        glTranslatef( -Camera.X, -Camera.Y, 0 );

      sprite2d_InScreen := sprite2d_InScreenCamera;
    end else
      begin
        cam2dApply  := FALSE;
        cam2dGlobal := @constCamera2D;
        sprite2d_InScreen := sprite2d_InScreenSimple;
      end;
end;

procedure cam2d_Vertex2f;
  var
    xa, ya : Single;
begin
  if cam2dGlobal.Zoom.X = 1 Then
    X := X - cam2dGlobal.X
  else
    X := ( X - cam2dGlobal.X + cam2dSX ) * cam2dGlobal.Zoom.X - cam2dSX;

  if cam2dGlobal.Zoom.Y = 1 Then
    Y := Y - cam2dGlobal.Y
  else
    Y := ( Y - cam2dGlobal.Y + cam2dSY ) * cam2dGlobal.Zoom.Y - cam2dSY;

  if cam2dGlobal.Angle <> 0 Then
    begin
      if cam2dGlobal.Angle <> cam2dAngle Then
        begin
          cam2dAngle := cam2dGlobal.Angle;
          cam2dSin   := Sin( cam2dGlobal.Angle * deg2rad );
          cam2dCos   := Cos( cam2dGlobal.Angle * deg2rad );
        end;
      xa := ogl_Width  / 2 + ( X + cam2dSX ) * cam2dCos - ( Y + cam2dSY ) * cam2dSin - scr_AddCX / scr_ResCX;
      ya := ogl_Height / 2 + ( X + cam2dSX ) * cam2dSin + ( Y + cam2dSY ) * cam2dCos - scr_AddCY / scr_ResCY;
      glVertex2f( xa, ya );
    end else
      glVertex2f( X, Y );
end;

procedure cam2d_Vertex2fv;
  var
    v2  : array[ 0..1 ] of Single;
    v2a : array[ 0..1 ] of Single;
begin
  if cam2dGlobal.Zoom.X = 1 Then
    v2[ 0 ] := PSingle( Ptr( V ) + 0 )^ - cam2dGlobal.X
  else
    v2[ 0 ] := ( PSingle( Ptr( V ) + 0 )^ - cam2dGlobal.X + cam2dSX ) * cam2dGlobal.Zoom.X - cam2dSX;

  if cam2dGlobal.Zoom.Y = 1 Then
    v2[ 1 ] := PSingle( Ptr( V ) + 4 )^ - cam2dGlobal.Y
  else
    v2[ 1 ] := ( PSingle( Ptr( V ) + 4 )^ - cam2dGlobal.Y + cam2dSY ) * cam2dGlobal.Zoom.Y - cam2dSY;

  if cam2dGlobal.Angle <> 0 Then
    begin
      if cam2dGlobal.Angle <> cam2dAngle Then
        begin
          cam2dAngle := cam2dGlobal.Angle;
          cam2dSin   := Sin( cam2dGlobal.Angle * deg2rad );
          cam2dCos   := Cos( cam2dGlobal.Angle * deg2rad );
        end;
      v2a[ 0 ] := ogl_Width  / 2 + ( v2[ 0 ] + cam2dSX ) * cam2dCos - ( v2[ 1 ] + cam2dSY ) * cam2dSin - scr_AddCX / scr_ResCX;
      v2a[ 1 ] := ogl_Height / 2 + ( v2[ 0 ] + cam2dSX ) * cam2dSin + ( v2[ 1 ] + cam2dSY ) * cam2dCos - scr_AddCY / scr_ResCY;
      glVertex2fv( @v2a[ 0 ] );
    end else
      glVertex2fv( @v2[ 0 ] );
end;

initialization
  cam2dGlobal := @constCamera2D;

end.
