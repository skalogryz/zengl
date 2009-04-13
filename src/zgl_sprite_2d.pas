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
unit zgl_sprite_2d;

{$I zgl_config.cfg}

interface

uses
  zgl_types,
  zgl_textures,
  zgl_fx,
  zgl_math_2d;

procedure ssprite2d_Draw( const Texture : zglPTexture; X, Y, W, H, Angle : Single; const Alpha : Byte = 255; const FX : DWORD = FX_BLEND );
procedure asprite2d_Draw( const Texture : zglPTexture; X, Y, W, H, Angle : Single; Frame : WORD; const Alpha : Byte = 255; const FX : DWORD = FX_BLEND );
procedure csprite2d_Draw( const Texture : zglPTexture; X, Y, W, H, Angle : Single; const CutRect : zglTRect; const Alpha : Byte = 255; const FX : DWORD = FX_BLEND );

implementation
uses
  zgl_const,
  zgl_application,
  zgl_screen,
  zgl_opengl,
  zgl_opengl_all,
  zgl_camera_2d;

function sprite2d_InScreen( const X, Y, W, H, Angle : Single ) : Boolean;
  var
    cx, cy : Single;
    sx, sy : Single;
    radius : Single;
begin
// т.к. zglTCamera2D можно крутить, проверка будет на попадание спрайта в "окружность"
// расчет очень упрощенный
  if ( cam2dGlobal.Zoom.X <> 1 ) or ( cam2dGlobal.Zoom.Y <> 1 ) Then
    begin
      radius := sqr( ( W * cam2DGlobal.Zoom.X + H * cam2DGlobal.Zoom.Y ) / 2 + ( ogl_CropW + ogl_CropH ) / 2 );
      cx := ogl_CropX + cam2dGlobal.X + ( ogl_CropW / scr_ResCX ) / 2;
      cy := ogl_CropY + cam2dGlobal.Y + ( ogl_CropH / scr_ResCY ) / 2;
      sx := ( X + W / 2 ) * cam2DGlobal.Zoom.X;
      sy := ( Y + H / 2 ) * cam2DGlobal.Zoom.Y;
      Result := sqr( cx - sx ) + sqr( cy - sy ) < radius;
    end else
      if Angle <> 0 Then
        Result := ( ( X + W + H / 2 >= ogl_CropX + cam2dGlobal.X ) and ( X - W - H / 2 <= ogl_CropW / scr_ResCX + cam2dGlobal.X ) and
                    ( Y + H + W / 2 >= ogl_CropY + cam2dGlobal.Y ) and ( Y - W - H / 2 <= ogl_CropH / scr_ResCY + cam2dGlobal.Y ) )
      else
        Result := ( ( X + W >= ogl_CropX + cam2dGlobal.X ) and ( X <= ogl_CropW / scr_ResCX + cam2dGlobal.X ) and
                    ( Y + H >= ogl_CropY + cam2dGlobal.Y ) and ( Y <= ogl_CropH / scr_ResCY + cam2dGlobal.Y ) );
end;

{------------------------------------------------------------------------------}
{--------------------------------- SSprite 2D ---------------------------------}
{------------------------------------------------------------------------------}
procedure ssprite2d_Draw;
  var
    Quad : array[ 0..3 ] of zglTPoint2D;

    FU, FV : Single;

    x1, x2 : Single;
    y1, y2 : Single;
    cX, cY : Single;
    c, s   : Single;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_SCALE > 0 Then
    begin
      X := X + ( W - W * FX2D_SX ) / 2;
      Y := Y + ( H - H * FX2D_SY ) / 2;
      W := W * FX2D_SX;
      H := H * FX2D_SY;
    end;

  if ( app_Flags and CROP_INVISIBLE > 0 ) and ( not sprite2d_InScreen( X, Y, W, H, Angle ) ) Then Exit;

  // Текстурные координаты
  if FX and FX2D_FLIPX > 0 Then FU := Texture^.U else FU := 0;
  if FX and FX2D_FLIPY > 0 Then FV := Texture^.V else FV := 0;

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      x1 := -W / 2;
      y1 := -H / 2;
      x2 :=  W / 2;
      y2 :=  H / 2;
      cX :=  X + W / 2;
      cY :=  Y + H / 2;

      s := Sin( Angle * rad2deg );
      c := Cos( Angle * rad2deg );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          Quad[ 0 ].X := x1 * c - y1 * s + cX;
          Quad[ 0 ].Y := x1 * s + y1 * c + cY;
          Quad[ 1 ].X := x2 * c - y1 * s + cX;
          Quad[ 1 ].Y := x2 * s + y1 * c + cY;
          Quad[ 2 ].X := x2 * c - y2 * s + cX;
          Quad[ 2 ].Y := x2 * s + y2 * c + cY;
          Quad[ 3 ].X := x1 * c - y2 * s + cX;
          Quad[ 3 ].Y := x1 * s + y2 * c + cY;
        end else
          begin
            Quad[ 0 ].X := ( x1 + FX2D_VX1 ) * c - ( y1 + FX2D_VY1 ) * s + cX;
            Quad[ 0 ].Y := ( x1 + FX2D_VX1 ) * s + ( y1 + FX2D_VY1 ) * c + cY;
            Quad[ 1 ].X := ( x2 + FX2D_VX2 ) * c - ( y1 + FX2D_VY2 ) * s + cX;
            Quad[ 1 ].Y := ( x2 + FX2D_VX2 ) * s + ( y1 + FX2D_VY2 ) * c + cY;
            Quad[ 2 ].X := ( x2 + FX2D_VX3 ) * c - ( y2 + FX2D_VY3 ) * s + cX;
            Quad[ 2 ].Y := ( x2 + FX2D_VX3 ) * s + ( y2 + FX2D_VY3 ) * c + cY;
            Quad[ 3 ].X := ( x1 + FX2D_VX4 ) * c - ( y2 + FX2D_VY4 ) * s + cX;
            Quad[ 3 ].Y := ( x1 + FX2D_VX4 ) * s + ( y2 + FX2D_VY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          Quad[ 0 ].X := X;
          Quad[ 0 ].Y := Y;
          Quad[ 1 ].X := X + W;
          Quad[ 1 ].Y := Y;
          Quad[ 2 ].X := X + W;
          Quad[ 2 ].Y := Y + H;
          Quad[ 3 ].X := X;
          Quad[ 3 ].Y := Y + H;
        end else
          begin
            Quad[ 0 ].X := X     + FX2D_VX1;
            Quad[ 0 ].Y := Y     + FX2D_VY1;
            Quad[ 1 ].X := X + W + FX2D_VX2;
            Quad[ 1 ].Y := Y     + FX2D_VY2;
            Quad[ 2 ].X := X + W + FX2D_VX3;
            Quad[ 2 ].Y := Y + H + FX2D_VY3;
            Quad[ 3 ].X := X     + FX2D_VX4;
            Quad[ 3 ].Y := Y + H + FX2D_VY4;
          end;

  if FX and FX2D_COLORMIX > 0 Then
    glColor4ub( FX2D_R, FX2D_G, FX2D_B, Alpha )
  else
    glColor4ub( 255, 255, 255, Alpha );

  if FX and FX_BLEND > 0 Then
    glEnable( GL_BLEND )
  else
    glEnable( GL_ALPHA_TEST );
  glEnable( GL_TEXTURE_2D );
  glBindTexture( GL_TEXTURE_2D, Texture.ID );

   glBegin( GL_QUADS );
    if FX and FX2D_VCA > 0 Then
      begin
        glColor4ub  ( FX2D_VR1, FX2D_VG1, FX2D_VB1, FX2D_VA1 );
        glTexCoord2f( FU, Texture^.V - FV );
        gl_Vertex2fv( @Quad[ 0 ] );

        glColor4ub  ( FX2D_VR2, FX2D_VG2, FX2D_VB2, FX2D_VA2 );
        glTexCoord2f( Texture^.U - FU, Texture^.V - FV );
        gl_Vertex2fv( @Quad[ 1 ] );

        glColor4ub  ( FX2D_VR3, FX2D_VG3, FX2D_VB3, FX2D_VA3 );
        glTexCoord2f( Texture^.U - FU, FV );
        gl_Vertex2fv( @Quad[ 2 ] );

        glColor4ub  ( FX2D_VR4, FX2D_VG4, FX2D_VB4, FX2D_VA4 );
        glTexCoord2f( FU, FV );
        gl_Vertex2fv( @Quad[ 3 ] );
      end else
        begin
          glTexCoord2f( FU, Texture^.V - FV );
          gl_Vertex2fv( @Quad[ 0 ] );

          glTexCoord2f( Texture^.U - FU, Texture^.V - FV );
          gl_Vertex2fv( @Quad[ 1 ] );

          glTexCoord2f( Texture^.U - FU, FV );
          gl_Vertex2fv( @Quad[ 2 ] );

          glTexCoord2f( FU, FV );
          gl_Vertex2fv( @Quad[ 3 ] );
        end;
  glEnd;

  glDisable( GL_TEXTURE_2D );
  glDisable( GL_BLEND );
  glDisable( GL_ALPHA_TEST );
end;

{------------------------------------------------------------------------------}
{--------------------------------- ASprite 2D ---------------------------------}
{------------------------------------------------------------------------------}
procedure asprite2d_Draw;
  var
    Quad : array[ 0..3 ] of zglTPoint2D;

    tX, tY, tU, tV, SU, SV : Single;

    x1, x2 : Single;
    y1, y2 : Single;
    cX, cY : Single;
    c, s   : Single;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_SCALE > 0 Then
    begin
      X := X + ( W - W * FX2D_SX ) / 2;
      Y := Y + ( H - H * FX2D_SY ) / 2;
      W := W * FX2D_SX;
      H := H * FX2D_SY;
    end;

  if ( app_Flags and CROP_INVISIBLE > 0 ) and ( not sprite2d_InScreen( X, Y, W, H, Angle ) ) Then Exit;

  // Текстурные координаты
  SU := Texture.U / Texture.FramesX;
  SV := Texture.V / Texture.FramesY;
  if FX and FX2D_FLIPX > 0 Then tU := SU else tU := 0;
  if FX and FX2D_FLIPY > 0 Then tV := SV else tV := 0;
  while Frame > Texture.FramesX * Texture.FramesY do Frame := Frame - Texture.FramesX * Texture.FramesY;
  tX := abs( Frame );
  tY := Texture.FramesY;
  while tX > Texture.FramesX do
    begin
      tX := tX - Texture.FramesX;
      tY := tY - 1;
    end;
  if tY < 1 Then tY := tY + Texture.FramesY;
  tX := tX * SU;
  tY := tY * SV;

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      x1 := -W / 2;
      y1 := -H / 2;
      x2 :=  W / 2;
      y2 :=  H / 2;
      cX :=  X + W / 2;
      cY :=  Y + H / 2;

      s := Sin( Angle * rad2deg );
      c := Cos( Angle * rad2deg );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          Quad[ 0 ].X := x1 * c - y1 * s + cX;
          Quad[ 0 ].Y := x1 * s + y1 * c + cY;
          Quad[ 1 ].X := x2 * c - y1 * s + cX;
          Quad[ 1 ].Y := x2 * s + y1 * c + cY;
          Quad[ 2 ].X := x2 * c - y2 * s + cX;
          Quad[ 2 ].Y := x2 * s + y2 * c + cY;
          Quad[ 3 ].X := x1 * c - y2 * s + cX;
          Quad[ 3 ].Y := x1 * s + y2 * c + cY;
        end else
          begin
            Quad[ 0 ].X := ( x1 + FX2D_VX1 ) * c - ( y1 + FX2D_VY1 ) * s + cX;
            Quad[ 0 ].Y := ( x1 + FX2D_VX1 ) * s + ( y1 + FX2D_VY1 ) * c + cY;
            Quad[ 1 ].X := ( x2 + FX2D_VX2 ) * c - ( y1 + FX2D_VY2 ) * s + cX;
            Quad[ 1 ].Y := ( x2 + FX2D_VX2 ) * s + ( y1 + FX2D_VY2 ) * c + cY;
            Quad[ 2 ].X := ( x2 + FX2D_VX3 ) * c - ( y2 + FX2D_VY3 ) * s + cX;
            Quad[ 2 ].Y := ( x2 + FX2D_VX3 ) * s + ( y2 + FX2D_VY3 ) * c + cY;
            Quad[ 3 ].X := ( x1 + FX2D_VX4 ) * c - ( y2 + FX2D_VY4 ) * s + cX;
            Quad[ 3 ].Y := ( x1 + FX2D_VX4 ) * s + ( y2 + FX2D_VY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          Quad[ 0 ].X := X;
          Quad[ 0 ].Y := Y;
          Quad[ 1 ].X := X + W;
          Quad[ 1 ].Y := Y;
          Quad[ 2 ].X := X + W;
          Quad[ 2 ].Y := Y + H;
          Quad[ 3 ].X := X;
          Quad[ 3 ].Y := Y + H;
        end else
          begin
            Quad[ 0 ].X := X     + FX2D_VX1;
            Quad[ 0 ].Y := Y     + FX2D_VY1;
            Quad[ 1 ].X := X + W + FX2D_VX2;
            Quad[ 1 ].Y := Y     + FX2D_VY2;
            Quad[ 2 ].X := X + W + FX2D_VX3;
            Quad[ 2 ].Y := Y + H + FX2D_VY3;
            Quad[ 3 ].X := X     + FX2D_VX4;
            Quad[ 3 ].Y := Y + H + FX2D_VY4;
          end;

  if FX and FX2D_COLORMIX > 0 Then
    glColor4ub( FX2D_R, FX2D_G, FX2D_B, Alpha )
  else
    glColor4ub( 255, 255, 255, Alpha );

  if FX and FX_BLEND > 0 Then
    glEnable( GL_BLEND )
  else
    glEnable( GL_ALPHA_TEST );
  glEnable( GL_TEXTURE_2D );
  glBindTexture( GL_TEXTURE_2D, Texture^.ID );

  glBegin( GL_QUADS );
  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ub  ( FX2D_VR1, FX2D_VG1, FX2D_VB1, FX2D_VA1 );
      glTexCoord2f( tX - SU + tU, tY - tV );
      gl_Vertex2fv( @Quad[ 0 ] );

      glColor4ub  ( FX2D_VR2, FX2D_VG2, FX2D_VB2, FX2D_VA2 );
      glTexCoord2f( tX - tU, tY - tV );
      gl_Vertex2fv( @Quad[ 1 ] );

      glColor4ub  ( FX2D_VR3, FX2D_VG3, FX2D_VB3, FX2D_VA3 );
      glTexCoord2f( tX - tU, tY - SV + tV );
      gl_Vertex2fv( @Quad[ 2 ] );

      glColor4ub  ( FX2D_VR4, FX2D_VG4, FX2D_VB4, FX2D_VA4 );
      glTexCoord2f( tX - SU + tU, tY - SV + tV );
      gl_Vertex2fv( @Quad[ 3 ] );
    end else
      begin
        glTexCoord2f( tX - SU + tU, tY - tV );
        gl_Vertex2fv( @Quad[ 0 ] );

        glTexCoord2f( tX - tU, tY - tV );
        gl_Vertex2fv( @Quad[ 1 ] );

        glTexCoord2f( tX - tU, tY - SV + tV );
        gl_Vertex2fv( @Quad[ 2 ] );

        glTexCoord2f( tX - SU + tU, tY - SV + tV );
        gl_Vertex2fv( @Quad[ 3 ] );
      end;
  glEnd;

  glDisable( GL_TEXTURE_2D );
  glDisable( GL_BLEND );
  glDisable( GL_ALPHA_TEST );
end;

{------------------------------------------------------------------------------}
{--------------------------------- CSprite 2D ---------------------------------}
{------------------------------------------------------------------------------}
procedure csprite2d_Draw;
  var
    Quad : array[ 0..3 ] of zglTPoint2D;

    tU, tV, tX, tY, tW, tH : Single;

    x1, x2 : Single;
    y1, y2 : Single;
    cX, cY : Single;
    c, s   : Single;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_SCALE > 0 Then
    begin
      X := X + ( W - W * FX2D_SX ) / 2;
      Y := Y + ( H - H * FX2D_SY ) / 2;
      W := W * FX2D_SX;
      H := H * FX2D_SY;
    end;

  if ( app_Flags and CROP_INVISIBLE > 0 ) and ( not sprite2d_InScreen( X, Y, W, H, Angle ) ) Then Exit;

  // Текстурные координаты
  // бред, ога :)
  tU := 1 / ( Texture.Width  / Texture.U / Texture.U );
  tV := 1 / ( Texture.Height / Texture.V / Texture.V );
  tX := tU * CutRect.X;
  tY := tV * ( Texture.Height / Texture.V - CutRect.Y );
  tW := tX + tU * CutRect.W;
  tH := tY + tV * ( - CutRect.H );

  if FX and FX2D_FLIPX > 0 Then tU := tW - tX else tU := 0;
  if FX and FX2D_FLIPY > 0 Then tV := tH - tY else tV := 0;

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      x1 := -W / 2;
      y1 := -H / 2;
      x2 :=  W / 2;
      y2 :=  H / 2;
      cX :=  X + W / 2;
      cY :=  Y + H / 2;

      s := Sin( Angle * rad2deg );
      c := Cos( Angle * rad2deg );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          Quad[ 0 ].X := x1 * c - y1 * s + cX;
          Quad[ 0 ].Y := x1 * s + y1 * c + cY;
          Quad[ 1 ].X := x2 * c - y1 * s + cX;
          Quad[ 1 ].Y := x2 * s + y1 * c + cY;
          Quad[ 2 ].X := x2 * c - y2 * s + cX;
          Quad[ 2 ].Y := x2 * s + y2 * c + cY;
          Quad[ 3 ].X := x1 * c - y2 * s + cX;
          Quad[ 3 ].Y := x1 * s + y2 * c + cY;
        end else
          begin
            Quad[ 0 ].X := ( x1 + FX2D_VX1 ) * c - ( y1 + FX2D_VY1 ) * s + cX;
            Quad[ 0 ].Y := ( x1 + FX2D_VX1 ) * s + ( y1 + FX2D_VY1 ) * c + cY;
            Quad[ 1 ].X := ( x2 + FX2D_VX2 ) * c - ( y1 + FX2D_VY2 ) * s + cX;
            Quad[ 1 ].Y := ( x2 + FX2D_VX2 ) * s + ( y1 + FX2D_VY2 ) * c + cY;
            Quad[ 2 ].X := ( x2 + FX2D_VX3 ) * c - ( y2 + FX2D_VY3 ) * s + cX;
            Quad[ 2 ].Y := ( x2 + FX2D_VX3 ) * s + ( y2 + FX2D_VY3 ) * c + cY;
            Quad[ 3 ].X := ( x1 + FX2D_VX4 ) * c - ( y2 + FX2D_VY4 ) * s + cX;
            Quad[ 3 ].Y := ( x1 + FX2D_VX4 ) * s + ( y2 + FX2D_VY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          Quad[ 0 ].X := X;
          Quad[ 0 ].Y := Y;
          Quad[ 1 ].X := X + W;
          Quad[ 1 ].Y := Y;
          Quad[ 2 ].X := X + W;
          Quad[ 2 ].Y := Y + H;
          Quad[ 3 ].X := X;
          Quad[ 3 ].Y := Y + H;
        end else
          begin
            Quad[ 0 ].X := X     + FX2D_VX1;
            Quad[ 0 ].Y := Y     + FX2D_VY1;
            Quad[ 1 ].X := X + W + FX2D_VX2;
            Quad[ 1 ].Y := Y     + FX2D_VY2;
            Quad[ 2 ].X := X + W + FX2D_VX3;
            Quad[ 2 ].Y := Y + H + FX2D_VY3;
            Quad[ 3 ].X := X     + FX2D_VX4;
            Quad[ 3 ].Y := Y + H + FX2D_VY4;
          end;

  if FX and FX2D_COLORMIX > 0 Then
    glColor4ub( FX2D_R, FX2D_G, FX2D_B, Alpha )
  else
    glColor4ub( 255, 255, 255, Alpha );

  if FX and FX_BLEND > 0 Then
    glEnable( GL_BLEND )
  else
    glEnable( GL_ALPHA_TEST );
  glEnable( GL_TEXTURE_2D );
  glBindTexture( GL_TEXTURE_2D, Texture^.ID );

  glBegin( GL_QUADS );
  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ub  ( FX2D_VR1, FX2D_VG1, FX2D_VB1, FX2D_VA1 );
      glTexCoord2f( tX + tU, tY + tV );
      gl_Vertex2fv( @Quad[ 0 ] );

      glColor4ub  ( FX2D_VR2, FX2D_VG2, FX2D_VB2, FX2D_VA2 );
      glTexCoord2f( tW - tU, tY + tV );
      gl_Vertex2fv( @Quad[ 1 ] );

      glColor4ub  ( FX2D_VR3, FX2D_VG3, FX2D_VB3, FX2D_VA3 );
      glTexCoord2f( tW - tU, tH - tV );
      gl_Vertex2fv( @Quad[ 2 ] );

      glColor4ub  ( FX2D_VR4, FX2D_VG4, FX2D_VB4, FX2D_VA4 );
      glTexCoord2f( tX + tU, tH - tV );
      gl_Vertex2fv( @Quad[ 3 ] );
    end else
      begin
        glTexCoord2f( tX + tU, tY + tV );
        gl_Vertex2fv( @Quad[ 0 ] );

        glTexCoord2f( tW - tU, tY + tV );
        gl_Vertex2fv( @Quad[ 1 ] );

        glTexCoord2f( tW - tU, tH - tV );
        gl_Vertex2fv( @Quad[ 2 ] );

        glTexCoord2f( tX + tU, tH - tV );
        gl_Vertex2fv( @Quad[ 3 ] );
      end;
  glEnd;

  glDisable( GL_TEXTURE_2D );
  glDisable( GL_BLEND );
  glDisable( GL_ALPHA_TEST );
end;

end.
