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
unit zgl_render_2d;

{$I zgl_config.cfg}

interface
uses
  zgl_opengl_all,
  zgl_textures;

procedure batch2d_Begin;
procedure batch2d_End;
procedure batch2d_Flush;
function  batch2d_Check( const Mode, FX : LongWord; const Texture : zglPTexture ) : Boolean;

function sprite2d_InScreenSimple( const X, Y, W, H, Angle : Single ) : Boolean;
function sprite2d_InScreenCamera( const X, Y, W, H, Angle : Single ) : Boolean;

var
  b2d_Started  : Boolean;
  b2d_New      : Boolean;
  b2d_Batches  : Integer;
  b2dcur_Mode  : LongWord;
  b2dcur_FX    : LongWord;
  b2dcur_Blend : LongWord;
  b2dcur_Color : LongWord;
  b2dcur_Tex   : zglPTexture;
  b2dcur_Smooth: Integer;
  sprite2d_InScreen : function( const X, Y, W, H, Angle : Single ) : Boolean;

implementation
uses
  zgl_screen,
  zgl_opengl,
  zgl_fx,
  zgl_camera_2d,
  zgl_primitives_2d;

procedure batch2d_Begin;
begin
  b2d_New     := TRUE;
  b2d_Started := TRUE;
end;

procedure batch2d_End;
begin
  batch2d_Flush();
  b2d_Batches  := 0;
  b2dcur_Mode  := 0;
  b2dcur_FX    := 0;
  b2dcur_Blend := 0;
  b2dcur_Color := 0;
  b2dcur_Tex   := nil;
  b2d_Started  := FALSE;
end;

procedure batch2d_Flush;
begin
  if b2d_Started and ( not b2d_New ) Then
    begin
      INC( b2d_Batches );
      b2d_New := TRUE;
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_ALPHA_TEST );
      glDisable( GL_BLEND );

      if b2dcur_Smooth > 0 Then
        begin
          b2dcur_Smooth := 0;
          glDisable( GL_LINE_SMOOTH    );
          glDisable( GL_POLYGON_SMOOTH );
        end;
    end;
end;

function batch2d_Check;
begin
  if ( Mode <> b2dcur_Mode ) or ( Texture <> b2dcur_Tex ) or ( ( FX and FX_BLEND = 0 ) and ( b2dcur_Blend <> 0 ) ) or
     ( b2dcur_Smooth <> FX and PR2D_SMOOTH ) Then
    begin
      if not b2d_New Then
        batch2d_Flush();
      b2d_New := TRUE;
    end;

  b2dcur_Mode   := Mode;
  b2dcur_Tex    := Texture;
  b2dcur_FX     := FX;
  b2dcur_Smooth := FX and PR2D_SMOOTH;
  if FX and FX_BLEND = 0 Then
    b2dcur_Blend := 0;

  Result := b2d_New;
  b2d_New := FALSE;
end;

function sprite2d_InScreenSimple;
begin
  if Angle <> 0 Then
    Result := ( ( X + W + H / 2 > ogl_ClipX ) and ( X - W - H / 2 < ogl_ClipX + ogl_ClipW / scr_ResCX ) and
                ( Y + H + W / 2 > ogl_ClipY ) and ( Y - W - H / 2 < ogl_ClipY + ogl_ClipH / scr_ResCY ) )
  else
    Result := ( ( X + W > ogl_ClipX ) and ( X < ogl_ClipX + ogl_ClipW / scr_ResCX ) and
                ( Y + H > ogl_ClipY ) and ( Y < ogl_ClipY + ogl_ClipH / scr_ResCY ) );
end;

function sprite2d_InScreenCamera;
  var
    cx, cy, crad : Single;
    sx, sy, srad : Single;
begin
  if ( cam2dGlobal.Zoom.X <> 1 ) or ( cam2dGlobal.Zoom.Y <> 1 ) or ( cam2dGlobal.Angle <> 0 ) Then
    begin
      if ( cam2dZoomX <> cam2dGlobal.Zoom.X ) or ( cam2dZoomY <> cam2dGlobal.Zoom.Y ) Then
        begin
          cam2dZoomX := cam2dGlobal.Zoom.X;
          cam2dZoomY := cam2dGlobal.Zoom.Y;
          ogl_ClipR  := Round( sqrt( sqr( ogl_ClipW / scr_ResCX / cam2dZoomX ) + sqr( ogl_ClipH / scr_ResCY / cam2dZoomY ) ) ) div 2;
        end;
      cx   := ogl_ClipX + cam2dGlobal.X + ( ogl_ClipW / scr_ResCX ) / 2;
      cy   := ogl_ClipY + cam2dGlobal.Y + ( ogl_ClipH / scr_ResCY ) / 2;
      crad := ogl_ClipR;

      sx   := X + W / 2;
      sy   := Y + H / 2;
      srad := ( W + H ) / 2;

      Result := sqr( sx - cx ) + sqr( sy - cy ) < sqr( srad + crad );
    end else
      if Angle <> 0 Then
        Result := ( ( X + W + H / 2 > ogl_ClipX + cam2dGlobal.X ) and ( X - W - H / 2 < ogl_ClipX + ogl_ClipW / scr_ResCX + cam2dGlobal.X ) and
                    ( Y + H + W / 2 > ogl_ClipY + cam2dGlobal.Y ) and ( Y - W - H / 2 < ogl_ClipY + ogl_ClipH / scr_ResCY + cam2dGlobal.Y ) )
      else
        Result := ( ( X + W > ogl_ClipX + cam2dGlobal.X ) and ( X < ogl_ClipX + ogl_ClipW / scr_ResCX + cam2dGlobal.X ) and
                    ( Y + H > ogl_ClipY + cam2dGlobal.Y ) and ( Y < ogl_ClipY + ogl_ClipH / scr_ResCY + cam2dGlobal.Y ) );
end;

initialization
  sprite2d_InScreen := sprite2d_InScreenSimple;

end.
