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
unit zgl_render_2d;

interface
uses
  zgl_types,
  zgl_opengl_all,
  zgl_textures;

procedure batch2d_Begin;
procedure batch2d_End;
procedure batch2d_Flush;
function  batch2d_Check( const Mode, FX : DWORD; const Texture : zglPTexture ) : Boolean;

function sprite2d_InScreenSimple( const X, Y, W, H, Angle : Single ) : Boolean;
function sprite2d_InScreenCamera( const X, Y, W, H, Angle : Single ) : Boolean;

var
  b2d_Started  : Boolean;
  b2d_New      : Boolean;
  b2d_Batches  : Integer;
  b2dcur_Mode  : DWORD;
  b2dcur_FX    : DWORD;
  b2dcur_Blend : DWORD;
  b2dcur_Color : DWORD;
  b2dcur_Tex   : zglPTexture;
  sprite2d_InScreen : function( const X, Y, W, H, Angle : Single ) : Boolean;

implementation
uses
  zgl_screen,
  zgl_opengl,
  zgl_fx,
  zgl_camera_2d;

procedure batch2d_Begin;
begin
  b2d_New     := TRUE;
  b2d_Started := TRUE;
end;

procedure batch2d_End;
begin
  batch2d_Flush;
  b2d_Batches  := 0;
  b2dcur_Mode  := 0;
  b2dcur_FX    := 0;
  b2dcur_Blend := 0;
  b2dcur_Color := $FFFFFF;
  b2dcur_Tex   := nil;
  b2d_Started  := FALSE;
end;

procedure batch2d_Flush;
begin
  if b2d_Started and ( not b2d_New ) Then
    begin
      INC( b2d_Batches );
      b2d_New := TRUE;
      glEnd;

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_ALPHA_TEST );
      glDisable( GL_BLEND );
    end;
end;

function batch2d_Check;
begin
  if ( Mode <> b2dcur_Mode ) or
     ( Texture <> b2dcur_Tex ) or
     ( FX and FX2D_COLORSET <> b2dcur_FX and FX2D_COLORSET ) or
     ( ( FX and FX_BLEND = 0 ) and ( b2dcur_Blend <> 0 ) ) Then
    begin
      if not b2d_New Then
        batch2d_Flush;
      b2d_New := TRUE;
    end;

  b2dcur_Mode := Mode;
  b2dcur_Tex  := Texture;
  b2dcur_FX   := FX;
  if FX and FX_BLEND = 0 Then
    b2dcur_Blend := 0;

  Result := b2d_New;
  b2d_New := FALSE;
end;

function sprite2d_InScreenSimple( const X, Y, W, H, Angle : Single ) : Boolean;
  var
    cx, cy, crad : Single;
    sx, sy, srad : Single;
begin
  if Angle <> 0 Then
    Result := ( ( X + W + H / 2 > ogl_CropX ) and ( X - W - H / 2 < ogl_CropX + ogl_CropW / scr_ResCX ) and
                ( Y + H + W / 2 > ogl_CropY ) and ( Y - W - H / 2 < ogl_CropY + ogl_CropH / scr_ResCY ) )
  else
    Result := ( ( X + W > ogl_CropX ) and ( X < ogl_CropX + ogl_CropW / scr_ResCX ) and
                ( Y + H > ogl_CropY ) and ( Y < ogl_CropY + ogl_CropH / scr_ResCY ) );
end;

function sprite2d_InScreenCamera( const X, Y, W, H, Angle : Single ) : Boolean;
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
          ogl_CropR  := Round( sqrt( sqr( ogl_CropW / cam2dZoomX ) + sqr( ogl_CropH / cam2dZoomY ) ) ) div 2;
        end;
      cx   := scr_AddCX / scr_ResCX + ogl_CropX + cam2dGlobal.X + ( ogl_CropW / scr_ResCX ) / 2;
      cy   := scr_AddCY / scr_ResCY + ogl_CropY + cam2dGlobal.Y + ( ogl_CropH / scr_ResCY ) / 2;
      crad := ogl_CropR;

      sx   := X + W / 2;
      sy   := Y + H / 2;
      srad := ( W + H ) / 2;

      Result := sqr( sx - cx ) + sqr( sy - cy ) < sqr( srad + crad );
    end else
      if Angle <> 0 Then
        Result := ( ( X + W + H / 2 > ogl_CropX + cam2dGlobal.X ) and ( X - W - H / 2 < ogl_CropX + ogl_CropW / scr_ResCX + cam2dGlobal.X ) and
                    ( Y + H + W / 2 > ogl_CropY + cam2dGlobal.Y ) and ( Y - W - H / 2 < ogl_CropY + ogl_CropH / scr_ResCY + cam2dGlobal.Y ) )
      else
        Result := ( ( X + W > ogl_CropX + cam2dGlobal.X ) and ( X < ogl_CropX + ogl_CropW / scr_ResCX + cam2dGlobal.X ) and
                    ( Y + H > ogl_CropY + cam2dGlobal.Y ) and ( Y < ogl_CropY + ogl_CropH / scr_ResCY + cam2dGlobal.Y ) );
end;

initialization
  sprite2d_InScreen := sprite2d_InScreenSimple;

end.
