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
unit zgl_camera_2d;

{$I zgl_config.cfg}

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

procedure cam2d_Set( Camera : zglPCamera2D );
function  cam2d_Get : zglPCamera2D;

var
  cam2dGlobal   : zglPCamera2D = nil;
  constCamera2D : zglTCamera2D = ( X: 0; Y: 0; Angle: 0; Zoom: ( X: 1; Y: 1 ) );
  cam2dApply    : Boolean;
  cam2dOnlyXY   : Boolean;
  cam2dCX       : Single;
  cam2dCY       : Single;
  cam2dZoomX    : Single;
  cam2dZoomY    : Single;

implementation
uses
  zgl_types,
  zgl_screen,
  zgl_opengl,
  zgl_opengl_all,
  zgl_render_2d;

procedure cam2d_Set( Camera : zglPCamera2D );
begin
  batch2d_Flush();

  if cam2dApply Then
    glPopMatrix();

  if Assigned( Camera ) Then
    begin
      cam2dGlobal := Camera;
      cam2dApply  := TRUE;
      cam2dOnlyXY := ( cam2dGlobal.Angle = 0 ) and ( cam2dGlobal.Zoom.X = 1 ) and ( cam2dGlobal.Zoom.Y = 1 );
      if ( cam2dZoomX <> cam2dGlobal.Zoom.X ) or ( cam2dZoomY <> cam2dGlobal.Zoom.Y ) Then
        ogl_ClipR := Round( sqrt( sqr( ogl_Width / scr_ResCX / cam2dGlobal.Zoom.X ) + sqr( ogl_Height / scr_ResCY / cam2dGlobal.Zoom.Y ) ) ) div 2;
      cam2dCX     := cam2dGlobal.X + ( ogl_Width / scr_ResCX ) / 2;
      cam2dCY     := cam2dGlobal.Y + ( ogl_Height / scr_ResCY ) / 2;
      cam2dZoomX  := cam2dGlobal.Zoom.X;
      cam2dZoomY  := cam2dGlobal.Zoom.Y;

      glPushMatrix();
      if not cam2dOnlyXY Then
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
        cam2dGlobal := @constCamera2D;
        cam2dApply  := FALSE;
        sprite2d_InScreen := sprite2d_InScreenSimple;
      end;
end;

function cam2d_Get : zglPCamera2D;
begin
  Result := cam2dGlobal;
end;

initialization
  cam2dGlobal := @constCamera2D;

end.
