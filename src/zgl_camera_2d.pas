{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.ru
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

procedure cam2d_Vertex2f( X, Y : Single ); stdcall;
procedure cam2d_Vertex2fv( v : Pointer ); stdcall;

var
  cam2dGlobal   : zglPCamera2D = nil;
  constCamera2D : zglTCamera2D = ( X: 0; Y: 0; Angle: 0; Zoom: ( X: 1; Y: 1 ) );

implementation
uses
  zgl_types,
  zgl_window,
  zgl_opengl,
  zgl_opengl_all;

procedure cam2d_Set;
begin
  cam2dGlobal := Camera;
  if Camera = nil Then
    begin
      cam2dGlobal  := @constCamera2D;
      gl_Vertex2f  := @glVertex2f;
      gl_Vertex2fv := @glVertex2fv;
    end else
      begin
        gl_Vertex2f  := @cam2d_Vertex2f;
        gl_Vertex2fv := @cam2d_Vertex2fv;
      end;
end;

procedure cam2d_Vertex2f;
  var
    sa, ca : Single;
    Xa, Ya : Single;
begin
  if cam2dGlobal.Zoom.X = 1 Then
    X := X - cam2dGlobal.X
  else
    X := ( X - cam2dGlobal.X ) * cam2dGlobal.Zoom.X + ( ( wnd_Width / 2 ) - ( wnd_Width / 2 ) * cam2dGlobal.Zoom.X );

  if cam2dGlobal.Zoom.X = 1 Then
    Y := Y - cam2dGlobal.Y
  else
    Y := ( Y - cam2dGlobal.Y ) * cam2dGlobal.Zoom.Y + ( ( wnd_Height / 2 ) - ( wnd_Height / 2 ) * cam2dGlobal.Zoom.Y );

  if cam2dGlobal.Angle <> 0 Then
    begin
      sa := Sin( cam2dGlobal.Angle * deg2rad );
      ca := Cos( cam2dGlobal.Angle * deg2rad );
      Xa := wnd_Width  / 2 + ( X - wnd_Width / 2 ) * ca - ( Y - wnd_Height / 2 ) * sa;
      Ya := wnd_Height / 2 + ( X - wnd_Width / 2 ) * sa + ( Y - wnd_Height / 2 ) * ca;
      glVertex2f( Xa, Ya );
    end else
      glVertex2f( X, Y );
end;

procedure cam2d_Vertex2fv;
  var
    v2  : array[ 0..1 ] of Single;
    sa, ca : Single;
    v2a : array[ 0..1 ] of Single;
begin
  if cam2dGlobal.Zoom.X = 1 Then
    v2[ 0 ] := PSingle( Ptr( v ) + 0 )^ - cam2dGlobal.X
  else
    v2[ 0 ] := ( PSingle( Ptr( v ) + 0 )^ - cam2dGlobal.X ) * cam2dGlobal.Zoom.X + ( ( wnd_Width / 2 ) - ( wnd_Width / 2 ) * cam2dGlobal.Zoom.X );

  if cam2dGlobal.Zoom.Y = 1 Then
    v2[ 1 ] := PSingle( Ptr( v ) + 4 )^ - cam2dGlobal.Y
  else
    v2[ 1 ] := ( PSingle( Ptr( v ) + 4 )^ - cam2dGlobal.Y ) * cam2dGlobal.Zoom.Y + ( ( wnd_Height / 2 ) - ( wnd_Height / 2 ) * cam2dGlobal.Zoom.Y );


  if cam2dGlobal.Angle <> 0 Then
    begin
      sa := Sin( cam2dGlobal.Angle * deg2rad );
      ca := Cos( cam2dGlobal.Angle * deg2rad );
      v2a[ 0 ] := wnd_Width  / 2 + ( v2[ 0 ] - wnd_Width / 2 ) * ca - ( v2[ 1 ] - wnd_Height / 2 ) * sa;
      v2a[ 1 ] := wnd_Height / 2 + ( v2[ 0 ] - wnd_Width / 2 ) * sa + ( v2[ 1 ] - wnd_Height / 2 ) * ca;
      glVertex2fv( @v2a[ 0 ] );
    end else
      glVertex2fv( @v2[ 0 ] );
end;

initialization
  cam2dGlobal := @constCamera2D;

end.
