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
unit zgl_render_2d;

{$I zgl_config.cfg}

interface
uses
  zgl_opengl_all,
  zgl_textures;

procedure batch2d_Begin;
procedure batch2d_End;
procedure batch2d_Flush;
function  batch2d_Check( Mode, FX : LongWord; Texture : zglPTexture ) : Boolean;

function sprite2d_InScreenSimple( X, Y, W, H, Angle : Single ) : Boolean;
function sprite2d_InScreenCamera( X, Y, W, H, Angle : Single ) : Boolean;

var
  render2dClip      : Boolean;
  b2dStarted        : Boolean;
  b2dNew            : Boolean;
  b2dBatches        : LongWord;
  b2dCurMode        : LongWord;
  b2dCurFX          : LongWord;
  b2dCurBlend       : LongWord;
  b2dCurColor       : LongWord;
  b2dCurColorMask   : LongWord;
  b2dCurTex         : zglPTexture;
  b2dCurSmooth      : LongWord;
  sprite2d_InScreen : function( X, Y, W, H, Angle : Single ) : Boolean;

implementation
uses
  zgl_screen,
  zgl_opengl,
  zgl_fx,
  zgl_camera_2d,
  zgl_primitives_2d;

procedure batch2d_Begin;
begin
  b2dNew     := TRUE;
  b2dStarted := TRUE;
  b2dBatches := 0;
end;

procedure batch2d_End;
begin
  batch2d_Flush();
  b2dCurMode  := 0;
  b2dCurFX    := 0;
  b2dCurBlend := 0;
  b2dCurColor := 0;
  b2dCurTex   := nil;
  b2dStarted  := FALSE;
end;

procedure batch2d_Flush;
begin
  if b2dStarted and ( not b2dNew ) Then
    begin
      INC( b2dBatches );
      b2dNew := TRUE;
      glEnd();

      glDisable( GL_TEXTURE_2D );
      if b2dCurBlend = 0 Then
        glDisable( GL_ALPHA_TEST )
      else
        glDisable( GL_BLEND );

      if b2dCurSmooth > 0 Then
        begin
          b2dCurSmooth := 0;
          glDisable( GL_LINE_SMOOTH    );
          glDisable( GL_POLYGON_SMOOTH );
        end;
    end;
end;

function batch2d_Check( Mode, FX : LongWord; Texture : zglPTexture ) : Boolean;
begin
  if ( Mode <> b2dCurMode ) or ( Texture <> b2dCurTex ) or ( ( FX and FX_BLEND = 0 ) and ( b2dCurBlend <> 0 ) ) or ( b2dCurSmooth <> FX and PR2D_SMOOTH ) Then
    begin
      if not b2dNew Then
        batch2d_Flush();
      b2dNew := TRUE;
    end;

  b2dCurMode   := Mode;
  b2dCurTex    := Texture;
  b2dCurFX     := FX;
  b2dCurSmooth := FX and PR2D_SMOOTH;
  if FX and FX_BLEND = 0 Then
    b2dCurBlend := 0;

  Result := b2dNew;
  b2dNew := FALSE;
end;

function sprite2d_InScreenSimple( X, Y, W, H, Angle : Single ) : Boolean;
begin
  if Angle <> 0 Then
    Result := ( ( X + W + H / 2 > oglClipX ) and ( X - W - H / 2 < oglClipX + oglClipW / scrResCX ) and
                ( Y + H + W / 2 > oglClipY ) and ( Y - W - H / 2 < oglClipY + oglClipH / scrResCY ) )
  else
    Result := ( ( X + W > oglClipX ) and ( X < oglClipX + oglClipW / scrResCX ) and
                ( Y + H > oglClipY ) and ( Y < oglClipY + oglClipH / scrResCY ) );
end;

function sprite2d_InScreenCamera( X, Y, W, H, Angle : Single ) : Boolean;
  var
    sx, sy, srad : Single;
begin
  if not cam2d.OnlyXY Then
    begin
      sx   := X + W / 2;
      sy   := Y + H / 2;
      srad := ( W + H ) / 2;

      Result := sqr( sx - cam2d.CX ) + sqr( sy - cam2d.CY ) < sqr( srad + oglClipR );
    end else
      if Angle <> 0 Then
        Result := ( ( X + W + H / 2 > oglClipX + cam2d.Global.X ) and ( X - W - H / 2 < oglClipX + oglClipW / scrResCX + cam2d.Global.X ) and
                    ( Y + H + W / 2 > oglClipY + cam2d.Global.Y ) and ( Y - W - H / 2 < oglClipY + oglClipH / scrResCY + cam2d.Global.Y ) )
      else
        Result := ( ( X + W > oglClipX + cam2d.Global.X ) and ( X < oglClipX + oglClipW / scrResCX + cam2d.Global.X ) and
                    ( Y + H > oglClipY + cam2d.Global.Y ) and ( Y < oglClipY + oglClipH / scrResCY + cam2d.Global.Y ) );
end;

initialization
  sprite2d_InScreen := sprite2d_InScreenSimple;

end.
