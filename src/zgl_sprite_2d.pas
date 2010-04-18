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
unit zgl_sprite_2d;

{$I zgl_config.cfg}

interface

uses
  zgl_textures,
  zgl_fx,
  zgl_math_2d;

type
  zglPTiles2D = ^zglTTiles2D;
  zglTTiles2D = record
    Count : record
      X, Y : Integer;
            end;
    Size  : record
      W, H : Single;
            end;
    Tiles : array of array of Integer;
  end;

procedure texture2d_Draw( const Texture : zglPTexture; const TexCoord : array of zglTPoint2D; X, Y, W, H, Angle : Single; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );
procedure ssprite2d_Draw( const Texture : zglPTexture; X, Y, W, H, Angle : Single; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );
procedure asprite2d_Draw( const Texture : zglPTexture; X, Y, W, H, Angle : Single; Frame : Word; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );
procedure csprite2d_Draw( const Texture : zglPTexture; X, Y, W, H, Angle : Single; const CutRect : zglTRect; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );
procedure tiles2d_Draw( const Texture : zglPTexture; const X, Y : Single; const Tiles : zglTTiles2D; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );

implementation
uses
  zgl_application,
  zgl_main,
  zgl_screen,
  zgl_opengl,
  zgl_opengl_all,
  zgl_render_2d,
  zgl_camera_2d;

procedure texture2d_Draw;
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    texC : array[ 0..3 ] of zglTPoint2D;

    x1, x2 : Single;
    y1, y2 : Single;
    cX, cY : Single;
    c, s   : Single;
    mX, mY : Single;
    mW, mH : Single;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_SCALE > 0 Then
    begin
      X := X + ( W - W * FX2D_SX ) / 2;
      Y := Y + ( H - H * FX2D_SY ) / 2;
      W := W * FX2D_SX;
      H := H * FX2D_SY;
    end;

  if ( app_Flags and CLIP_INVISIBLE > 0 ) Then
    if FX and FX2D_VCHANGE = 0 Then
      begin
        if not sprite2d_InScreen( X, Y, W, H, Angle ) Then Exit;
      end else
        begin
          mX := min( X + FX2D_VX1, min( X + W + FX2D_VX2, min( X + W + FX2D_VX3, X + FX2D_VX4 ) ) );
          mY := min( Y + FX2D_VY1, min( Y + FX2D_VY2, min( Y + H + FX2D_VY3, Y + H + FX2D_VY4 ) ) );
          mW := max( X + FX2D_VX1, max( X + W + FX2D_VX2, max( X + W + FX2D_VX3, X + FX2D_VX4 ) ) ) - mx;
          mH := max( Y + FX2D_VY1, max( Y + FX2D_VY2, max( Y + H + FX2D_VY3, Y + H + FX2D_VY4 ) ) ) - mY;
          if not sprite2d_InScreen( mX, mY, mW + abs( X - mX ) + abs( mW - W ), mH + abs( Y - mY ) + abs( mH - H ), Angle ) Then Exit;
        end;

  // Текстурные координаты
  if ( FX and FX2D_FLIPX > 0 ) and ( FX and FX2D_FLIPY > 0 ) Then
    begin
      texC[ 0 ] := TexCoord[ 2 ];
      texC[ 1 ] := TexCoord[ 3 ];
      texC[ 2 ] := TexCoord[ 0 ];
      texC[ 3 ] := TexCoord[ 1 ];
    end else
      if FX and FX2D_FLIPX > 0 Then
        begin
          texC[ 0 ] := TexCoord[ 1 ];
          texC[ 1 ] := TexCoord[ 0 ];
          texC[ 2 ] := TexCoord[ 3 ];
          texC[ 3 ] := TexCoord[ 2 ];
        end else
          if FX and FX2D_FLIPY > 0 Then
            begin
              texC[ 0 ] := TexCoord[ 3 ];
              texC[ 1 ] := TexCoord[ 2 ];
              texC[ 2 ] := TexCoord[ 1 ];
              texC[ 3 ] := TexCoord[ 0 ];
            end else
              Move( TexCoord[ 0 ], texC[ 0 ], 4 * SizeOf( zglTPoint2D ) );

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      x1 := -W / 2;
      y1 := -H / 2;
      x2 :=  W / 2;
      y2 :=  H / 2;
      cX :=  X + W / 2;
      cY :=  Y + H / 2;

      s := Sin( Angle * deg2rad );
      c := Cos( Angle * deg2rad );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          quad[ 0 ].X := x1 * c - y1 * s + cX;
          quad[ 0 ].Y := x1 * s + y1 * c + cY;
          quad[ 1 ].X := x2 * c - y1 * s + cX;
          quad[ 1 ].Y := x2 * s + y1 * c + cY;
          quad[ 2 ].X := x2 * c - y2 * s + cX;
          quad[ 2 ].Y := x2 * s + y2 * c + cY;
          quad[ 3 ].X := x1 * c - y2 * s + cX;
          quad[ 3 ].Y := x1 * s + y2 * c + cY;
        end else
          begin
            quad[ 0 ].X := ( x1 + FX2D_VX1 ) * c - ( y1 + FX2D_VY1 ) * s + cX;
            quad[ 0 ].Y := ( x1 + FX2D_VX1 ) * s + ( y1 + FX2D_VY1 ) * c + cY;
            quad[ 1 ].X := ( x2 + FX2D_VX2 ) * c - ( y1 + FX2D_VY2 ) * s + cX;
            quad[ 1 ].Y := ( x2 + FX2D_VX2 ) * s + ( y1 + FX2D_VY2 ) * c + cY;
            quad[ 2 ].X := ( x2 + FX2D_VX3 ) * c - ( y2 + FX2D_VY3 ) * s + cX;
            quad[ 2 ].Y := ( x2 + FX2D_VX3 ) * s + ( y2 + FX2D_VY3 ) * c + cY;
            quad[ 3 ].X := ( x1 + FX2D_VX4 ) * c - ( y2 + FX2D_VY4 ) * s + cX;
            quad[ 3 ].Y := ( x1 + FX2D_VX4 ) * s + ( y2 + FX2D_VY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          quad[ 0 ].X := X;
          quad[ 0 ].Y := Y;
          quad[ 1 ].X := X + W;
          quad[ 1 ].Y := Y;
          quad[ 2 ].X := X + W;
          quad[ 2 ].Y := Y + H;
          quad[ 3 ].X := X;
          quad[ 3 ].Y := Y + H;
        end else
          begin
            quad[ 0 ].X := X     + FX2D_VX1;
            quad[ 0 ].Y := Y     + FX2D_VY1;
            quad[ 1 ].X := X + W + FX2D_VX2;
            quad[ 1 ].Y := Y     + FX2D_VY2;
            quad[ 2 ].X := X + W + FX2D_VX3;
            quad[ 2 ].Y := Y + H + FX2D_VY3;
            quad[ 3 ].X := X     + FX2D_VX4;
            quad[ 3 ].Y := Y + H + FX2D_VY4;
          end;

  if ( not b2d_Started ) or batch2d_Check( GL_TRIANGLES, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture.ID );

      glBegin( GL_TRIANGLES );
    end;

  if FX and FX_COLOR > 0 Then
    glColor4ub( FX2D_R, FX2D_G, FX2D_B, Alpha )
  else
    glColor4ub( 255, 255, 255, Alpha );

  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ubv( @FX2D_VCA1[ 0 ] );
      glTexCoord2fv( @texC[ 0 ] );
      gl_Vertex2fv( @quad[ 0 ] );

      glColor4ubv( @FX2D_VCA2[ 0 ] );
      glTexCoord2fv( @texC[ 1 ] );
      gl_Vertex2fv( @quad[ 1 ] );

      glColor4ubv( @FX2D_VCA3[ 0 ] );
      glTexCoord2fv( @texC[ 2 ] );
      gl_Vertex2fv( @quad[ 2 ] );

      glColor4ubv( @FX2D_VCA3[ 0 ] );
      glTexCoord2fv( @texC[ 2 ] );
      gl_Vertex2fv( @quad[ 2 ] );

      glColor4ubv( @FX2D_VCA4[ 0 ] );
      glTexCoord2fv( @texC[ 3 ] );
      gl_Vertex2fv( @quad[ 3 ] );

      glColor4ubv( @FX2D_VCA1[ 0 ] );
      glTexCoord2fv( @texC[ 0 ] );
      gl_Vertex2fv( @quad[ 0 ] );
    end else
      begin
        glTexCoord2fv( @texC[ 0 ] );
        gl_Vertex2fv( @quad[ 0 ] );

        glTexCoord2fv( @texC[ 1 ] );
        gl_Vertex2fv( @quad[ 1 ] );

        glTexCoord2fv( @texC[ 2 ] );
        gl_Vertex2fv( @quad[ 2 ] );

        glTexCoord2fv( @texC[ 2 ] );
        gl_Vertex2fv( @quad[ 2 ] );

        glTexCoord2fv( @texC[ 3 ] );
        gl_Vertex2fv( @quad[ 3 ] );

        glTexCoord2fv( @texC[ 0 ] );
        gl_Vertex2fv( @quad[ 0 ] );
      end;

  if not b2d_Started Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure ssprite2d_Draw;
  var
    quad : array[ 0..3 ] of zglTPoint2D;

    fU, fV : Single;
    x1, x2 : Single;
    y1, y2 : Single;
    cX, cY : Single;
    c, s   : Single;
    mX, mY : Single;
    mW, mH : Single;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_SCALE > 0 Then
    begin
      X := X + ( W - W * FX2D_SX ) / 2;
      Y := Y + ( H - H * FX2D_SY ) / 2;
      W := W * FX2D_SX;
      H := H * FX2D_SY;
    end;

  if ( app_Flags and CLIP_INVISIBLE > 0 ) Then
    if FX and FX2D_VCHANGE = 0 Then
      begin
        if not sprite2d_InScreen( X, Y, W, H, Angle ) Then Exit;
      end else
        begin
          mX := min( X + FX2D_VX1, min( X + W + FX2D_VX2, min( X + W + FX2D_VX3, X + FX2D_VX4 ) ) );
          mY := min( Y + FX2D_VY1, min( Y + FX2D_VY2, min( Y + H + FX2D_VY3, Y + H + FX2D_VY4 ) ) );
          mW := max( X + FX2D_VX1, max( X + W + FX2D_VX2, max( X + W + FX2D_VX3, X + FX2D_VX4 ) ) ) - mx;
          mH := max( Y + FX2D_VY1, max( Y + FX2D_VY2, max( Y + H + FX2D_VY3, Y + H + FX2D_VY4 ) ) ) - mY;
          if not sprite2d_InScreen( mX, mY, mW + abs( X - mX ) + abs( mW - W ), mH + abs( Y - mY ) + abs( mH - H ), Angle ) Then Exit;
        end;

  // Текстурные координаты
  if FX and FX2D_FLIPX > 0 Then fU := Texture^.U else fU := 0;
  if FX and FX2D_FLIPY > 0 Then fV := Texture^.V else fV := 0;

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      x1 := -W / 2;
      y1 := -H / 2;
      x2 :=  W / 2;
      y2 :=  H / 2;
      cX :=  X + W / 2;
      cY :=  Y + H / 2;

      s := Sin( Angle * deg2rad );
      c := Cos( Angle * deg2rad );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          quad[ 0 ].X := x1 * c - y1 * s + cX;
          quad[ 0 ].Y := x1 * s + y1 * c + cY;
          quad[ 1 ].X := x2 * c - y1 * s + cX;
          quad[ 1 ].Y := x2 * s + y1 * c + cY;
          quad[ 2 ].X := x2 * c - y2 * s + cX;
          quad[ 2 ].Y := x2 * s + y2 * c + cY;
          quad[ 3 ].X := x1 * c - y2 * s + cX;
          quad[ 3 ].Y := x1 * s + y2 * c + cY;
        end else
          begin
            quad[ 0 ].X := ( x1 + FX2D_VX1 ) * c - ( y1 + FX2D_VY1 ) * s + cX;
            quad[ 0 ].Y := ( x1 + FX2D_VX1 ) * s + ( y1 + FX2D_VY1 ) * c + cY;
            quad[ 1 ].X := ( x2 + FX2D_VX2 ) * c - ( y1 + FX2D_VY2 ) * s + cX;
            quad[ 1 ].Y := ( x2 + FX2D_VX2 ) * s + ( y1 + FX2D_VY2 ) * c + cY;
            quad[ 2 ].X := ( x2 + FX2D_VX3 ) * c - ( y2 + FX2D_VY3 ) * s + cX;
            quad[ 2 ].Y := ( x2 + FX2D_VX3 ) * s + ( y2 + FX2D_VY3 ) * c + cY;
            quad[ 3 ].X := ( x1 + FX2D_VX4 ) * c - ( y2 + FX2D_VY4 ) * s + cX;
            quad[ 3 ].Y := ( x1 + FX2D_VX4 ) * s + ( y2 + FX2D_VY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          quad[ 0 ].X := X;
          quad[ 0 ].Y := Y;
          quad[ 1 ].X := X + W;
          quad[ 1 ].Y := Y;
          quad[ 2 ].X := X + W;
          quad[ 2 ].Y := Y + H;
          quad[ 3 ].X := X;
          quad[ 3 ].Y := Y + H;
        end else
          begin
            quad[ 0 ].X := X     + FX2D_VX1;
            quad[ 0 ].Y := Y     + FX2D_VY1;
            quad[ 1 ].X := X + W + FX2D_VX2;
            quad[ 1 ].Y := Y     + FX2D_VY2;
            quad[ 2 ].X := X + W + FX2D_VX3;
            quad[ 2 ].Y := Y + H + FX2D_VY3;
            quad[ 3 ].X := X     + FX2D_VX4;
            quad[ 3 ].Y := Y + H + FX2D_VY4;
          end;

  if ( not b2d_Started ) or batch2d_Check( GL_TRIANGLES, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture.ID );

      glBegin( GL_TRIANGLES );
    end;

  if FX and FX_COLOR > 0 Then
    glColor4ub( FX2D_R, FX2D_G, FX2D_B, Alpha )
  else
    glColor4ub( 255, 255, 255, Alpha );

  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ubv( @FX2D_VCA1[ 0 ] );
      glTexCoord2f( fU, Texture^.V - fV );
      gl_Vertex2fv( @quad[ 0 ] );

      glColor4ubv( @FX2D_VCA2[ 0 ] );
      glTexCoord2f( Texture^.U - fU, Texture^.V - fV );
      gl_Vertex2fv( @quad[ 1 ] );

      glColor4ubv( @FX2D_VCA3[ 0 ] );
      glTexCoord2f( Texture^.U - fU, fV );
      gl_Vertex2fv( @quad[ 2 ] );

      glColor4ubv( @FX2D_VCA3[ 0 ] );
      glTexCoord2f( Texture^.U - fU, fV );
      gl_Vertex2fv( @quad[ 2 ] );

      glColor4ubv( @FX2D_VCA4[ 0 ] );
      glTexCoord2f( fU, fV );
      gl_Vertex2fv( @quad[ 3 ] );

      glColor4ubv( @FX2D_VCA1[ 0 ] );
      glTexCoord2f( fU, Texture^.V - fV );
      gl_Vertex2fv( @quad[ 0 ] );
    end else
      begin
        glTexCoord2f( fU, Texture^.V - fV );
        gl_Vertex2fv( @quad[ 0 ] );

        glTexCoord2f( Texture^.U - fU, Texture^.V - fV );
        gl_Vertex2fv( @quad[ 1 ] );

        glTexCoord2f( Texture^.U - fU, fV );
        gl_Vertex2fv( @quad[ 2 ] );

        glTexCoord2f( Texture^.U - fU, fV );
        gl_Vertex2fv( @quad[ 2 ] );

        glTexCoord2f( fU, fV );
        gl_Vertex2fv( @quad[ 3 ] );

        glTexCoord2f( fU, Texture^.V - fV );
        gl_Vertex2fv( @quad[ 0 ] );
      end;

  if not b2d_Started Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure asprite2d_Draw;
  var
    quad : array[ 0..3 ] of zglTPoint2D;

    tX, tY, tU, tV, u, v : Single;

    x1, x2 : Single;
    y1, y2 : Single;
    cX, cY : Single;
    c, s   : Single;
    mX, mY : Single;
    mW, mH : Single;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_SCALE > 0 Then
    begin
      X := X + ( W - W * FX2D_SX ) / 2;
      Y := Y + ( H - H * FX2D_SY ) / 2;
      W := W * FX2D_SX;
      H := H * FX2D_SY;
    end;

  if ( app_Flags and CLIP_INVISIBLE > 0 ) Then
    if FX and FX2D_VCHANGE = 0 Then
      begin
        if not sprite2d_InScreen( X, Y, W, H, Angle ) Then Exit;
      end else
        begin
          mX := min( X + FX2D_VX1, min( X + W + FX2D_VX2, min( X + W + FX2D_VX3, X + FX2D_VX4 ) ) );
          mY := min( Y + FX2D_VY1, min( Y + FX2D_VY2, min( Y + H + FX2D_VY3, Y + H + FX2D_VY4 ) ) );
          mW := max( X + FX2D_VX1, max( X + W + FX2D_VX2, max( X + W + FX2D_VX3, X + FX2D_VX4 ) ) ) - mx;
          mH := max( Y + FX2D_VY1, max( Y + FX2D_VY2, max( Y + H + FX2D_VY3, Y + H + FX2D_VY4 ) ) ) - mY;
          if not sprite2d_InScreen( mX, mY, mW + abs( X - mX ) + abs( mW - W ), mH + abs( Y - mY ) + abs( mH - H ), Angle ) Then Exit;
        end;

  // Текстурные координаты
  u := Texture.U / Texture.FramesX;
  v := Texture.V / Texture.FramesY;
  if FX and FX2D_FLIPX > 0 Then tU := u else tU := 0;
  if FX and FX2D_FLIPY > 0 Then tV := v else tV := 0;
  tY := Frame div Texture.FramesX;
  tX := Frame - tY * Texture.FramesX;
  tY := Texture.FramesY - tY;
  if tX = 0 Then
    begin
      tX := Texture.FramesX;
      tY := tY + 1;
    end;
  tX := tX * u;
  tY := tY * v;

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      x1 := -W / 2;
      y1 := -H / 2;
      x2 :=  W / 2;
      y2 :=  H / 2;
      cX :=  X + W / 2;
      cY :=  Y + H / 2;

      s := Sin( Angle * deg2rad );
      c := Cos( Angle * deg2rad );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          quad[ 0 ].X := x1 * c - y1 * s + cX;
          quad[ 0 ].Y := x1 * s + y1 * c + cY;
          quad[ 1 ].X := x2 * c - y1 * s + cX;
          quad[ 1 ].Y := x2 * s + y1 * c + cY;
          quad[ 2 ].X := x2 * c - y2 * s + cX;
          quad[ 2 ].Y := x2 * s + y2 * c + cY;
          quad[ 3 ].X := x1 * c - y2 * s + cX;
          quad[ 3 ].Y := x1 * s + y2 * c + cY;
        end else
          begin
            quad[ 0 ].X := ( x1 + FX2D_VX1 ) * c - ( y1 + FX2D_VY1 ) * s + cX;
            quad[ 0 ].Y := ( x1 + FX2D_VX1 ) * s + ( y1 + FX2D_VY1 ) * c + cY;
            quad[ 1 ].X := ( x2 + FX2D_VX2 ) * c - ( y1 + FX2D_VY2 ) * s + cX;
            quad[ 1 ].Y := ( x2 + FX2D_VX2 ) * s + ( y1 + FX2D_VY2 ) * c + cY;
            quad[ 2 ].X := ( x2 + FX2D_VX3 ) * c - ( y2 + FX2D_VY3 ) * s + cX;
            quad[ 2 ].Y := ( x2 + FX2D_VX3 ) * s + ( y2 + FX2D_VY3 ) * c + cY;
            quad[ 3 ].X := ( x1 + FX2D_VX4 ) * c - ( y2 + FX2D_VY4 ) * s + cX;
            quad[ 3 ].Y := ( x1 + FX2D_VX4 ) * s + ( y2 + FX2D_VY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          quad[ 0 ].X := X;
          quad[ 0 ].Y := Y;
          quad[ 1 ].X := X + W;
          quad[ 1 ].Y := Y;
          quad[ 2 ].X := X + W;
          quad[ 2 ].Y := Y + H;
          quad[ 3 ].X := X;
          quad[ 3 ].Y := Y + H;
        end else
          begin
            quad[ 0 ].X := X     + FX2D_VX1;
            quad[ 0 ].Y := Y     + FX2D_VY1;
            quad[ 1 ].X := X + W + FX2D_VX2;
            quad[ 1 ].Y := Y     + FX2D_VY2;
            quad[ 2 ].X := X + W + FX2D_VX3;
            quad[ 2 ].Y := Y + H + FX2D_VY3;
            quad[ 3 ].X := X     + FX2D_VX4;
            quad[ 3 ].Y := Y + H + FX2D_VY4;
          end;

  if ( not b2d_Started ) or batch2d_Check( GL_TRIANGLES, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture^.ID );

      glBegin( GL_TRIANGLES );
    end;

  if FX and FX_COLOR > 0 Then
    glColor4ub( FX2D_R, FX2D_G, FX2D_B, Alpha )
  else
    glColor4ub( 255, 255, 255, Alpha );

  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ubv( @FX2D_VCA1[ 0 ] );
      glTexCoord2f( tX - u + tU, tY - tV );
      gl_Vertex2fv( @quad[ 0 ] );

      glColor4ubv( @FX2D_VCA2[ 0 ] );
      glTexCoord2f( tX - tU, tY - tV );
      gl_Vertex2fv( @quad[ 1 ] );

      glColor4ubv( @FX2D_VCA3[ 0 ] );
      glTexCoord2f( tX - tU, tY - v + tV );
      gl_Vertex2fv( @quad[ 2 ] );

      glColor4ubv( @FX2D_VCA3[ 0 ] );
      glTexCoord2f( tX - tU, tY - v + tV );
      gl_Vertex2fv( @quad[ 2 ] );

      glColor4ubv( @FX2D_VCA4[ 0 ] );
      glTexCoord2f( tX - u + tU, tY - v + tV );
      gl_Vertex2fv( @quad[ 3 ] );

      glColor4ubv( @FX2D_VCA1[ 0 ] );
      glTexCoord2f( tX - u + tU, tY - tV );
      gl_Vertex2fv( @quad[ 0 ] );
    end else
      begin
        glTexCoord2f( tX - u + tU, tY - tV );
        gl_Vertex2fv( @quad[ 0 ] );

        glTexCoord2f( tX - tU, tY - tV );
        gl_Vertex2fv( @quad[ 1 ] );

        glTexCoord2f( tX - tU, tY - v + tV );
        gl_Vertex2fv( @quad[ 2 ] );

        glTexCoord2f( tX - tU, tY - v + tV );
        gl_Vertex2fv( @quad[ 2 ] );

        glTexCoord2f( tX - u + tU, tY - v + tV );
        gl_Vertex2fv( @quad[ 3 ] );

        glTexCoord2f( tX - u + tU, tY - tV );
        gl_Vertex2fv( @quad[ 0 ] );
      end;

  if not b2d_Started Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure csprite2d_Draw;
  var
    quad : array[ 0..3 ] of zglTPoint2D;

    tU, tV, tX, tY, tW, tH : Single;

    x1, x2 : Single;
    y1, y2 : Single;
    cX, cY : Single;
    c, s   : Single;
    mX, mY : Single;
    mW, mH : Single;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_SCALE > 0 Then
    begin
      X := X + ( W - W * FX2D_SX ) / 2;
      Y := Y + ( H - H * FX2D_SY ) / 2;
      W := W * FX2D_SX;
      H := H * FX2D_SY;
    end;

  if ( app_Flags and CLIP_INVISIBLE > 0 ) Then
    if FX and FX2D_VCHANGE = 0 Then
      begin
        if not sprite2d_InScreen( X, Y, W, H, Angle ) Then Exit;
      end else
        begin
          mX := min( X + FX2D_VX1, min( X + W + FX2D_VX2, min( X + W + FX2D_VX3, X + FX2D_VX4 ) ) );
          mY := min( Y + FX2D_VY1, min( Y + FX2D_VY2, min( Y + H + FX2D_VY3, Y + H + FX2D_VY4 ) ) );
          mW := max( X + FX2D_VX1, max( X + W + FX2D_VX2, max( X + W + FX2D_VX3, X + FX2D_VX4 ) ) ) - mx;
          mH := max( Y + FX2D_VY1, max( Y + FX2D_VY2, max( Y + H + FX2D_VY3, Y + H + FX2D_VY4 ) ) ) - mY;
          if not sprite2d_InScreen( mX, mY, mW + abs( X - mX ) + abs( mW - W ), mH + abs( Y - mY ) + abs( mH - H ), Angle ) Then Exit;
        end;

  // Текстурные координаты
  // бред, ога :)
  tU := 1 / ( Texture.Width  / Texture.U / Texture.U );
  tV := 1 / ( Texture.Height / Texture.V / Texture.V );
  tX := tU * ( CutRect.X / Texture.U );
  tY := tV * ( Texture.Height / Texture.V - CutRect.Y / Texture.V );
  tW := tX + tU * ( CutRect.W / Texture.U );
  tH := tY + tV * ( -CutRect.H / Texture.V );

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

      s := Sin( Angle * deg2rad );
      c := Cos( Angle * deg2rad );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          quad[ 0 ].X := x1 * c - y1 * s + cX;
          quad[ 0 ].Y := x1 * s + y1 * c + cY;
          quad[ 1 ].X := x2 * c - y1 * s + cX;
          quad[ 1 ].Y := x2 * s + y1 * c + cY;
          quad[ 2 ].X := x2 * c - y2 * s + cX;
          quad[ 2 ].Y := x2 * s + y2 * c + cY;
          quad[ 3 ].X := x1 * c - y2 * s + cX;
          quad[ 3 ].Y := x1 * s + y2 * c + cY;
        end else
          begin
            quad[ 0 ].X := ( x1 + FX2D_VX1 ) * c - ( y1 + FX2D_VY1 ) * s + cX;
            quad[ 0 ].Y := ( x1 + FX2D_VX1 ) * s + ( y1 + FX2D_VY1 ) * c + cY;
            quad[ 1 ].X := ( x2 + FX2D_VX2 ) * c - ( y1 + FX2D_VY2 ) * s + cX;
            quad[ 1 ].Y := ( x2 + FX2D_VX2 ) * s + ( y1 + FX2D_VY2 ) * c + cY;
            quad[ 2 ].X := ( x2 + FX2D_VX3 ) * c - ( y2 + FX2D_VY3 ) * s + cX;
            quad[ 2 ].Y := ( x2 + FX2D_VX3 ) * s + ( y2 + FX2D_VY3 ) * c + cY;
            quad[ 3 ].X := ( x1 + FX2D_VX4 ) * c - ( y2 + FX2D_VY4 ) * s + cX;
            quad[ 3 ].Y := ( x1 + FX2D_VX4 ) * s + ( y2 + FX2D_VY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          quad[ 0 ].X := X;
          quad[ 0 ].Y := Y;
          quad[ 1 ].X := X + W;
          quad[ 1 ].Y := Y;
          quad[ 2 ].X := X + W;
          quad[ 2 ].Y := Y + H;
          quad[ 3 ].X := X;
          quad[ 3 ].Y := Y + H;
        end else
          begin
            quad[ 0 ].X := X     + FX2D_VX1;
            quad[ 0 ].Y := Y     + FX2D_VY1;
            quad[ 1 ].X := X + W + FX2D_VX2;
            quad[ 1 ].Y := Y     + FX2D_VY2;
            quad[ 2 ].X := X + W + FX2D_VX3;
            quad[ 2 ].Y := Y + H + FX2D_VY3;
            quad[ 3 ].X := X     + FX2D_VX4;
            quad[ 3 ].Y := Y + H + FX2D_VY4;
          end;

  if ( not b2d_Started ) or batch2d_Check( GL_TRIANGLES, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture^.ID );

      glBegin( GL_TRIANGLES );
    end;

  if FX and FX_COLOR > 0 Then
    glColor4ub( FX2D_R, FX2D_G, FX2D_B, Alpha )
  else
    glColor4ub( 255, 255, 255, Alpha );

  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ubv( @FX2D_VCA1[ 0 ] );
      glTexCoord2f( tX + tU, tY + tV );
      gl_Vertex2fv( @quad[ 0 ] );

      glColor4ubv( @FX2D_VCA2[ 0 ] );
      glTexCoord2f( tW - tU, tY + tV );
      gl_Vertex2fv( @quad[ 1 ] );

      glColor4ubv( @FX2D_VCA3[ 0 ] );
      glTexCoord2f( tW - tU, tH - tV );
      gl_Vertex2fv( @quad[ 2 ] );

      glColor4ubv( @FX2D_VCA3[ 0 ] );
      glTexCoord2f( tW - tU, tH - tV );
      gl_Vertex2fv( @quad[ 2 ] );

      glColor4ubv( @FX2D_VCA4[ 0 ] );
      glTexCoord2f( tX + tU, tH - tV );
      gl_Vertex2fv( @quad[ 3 ] );

      glColor4ubv( @FX2D_VCA1[ 0 ] );
      glTexCoord2f( tX + tU, tY + tV );
      gl_Vertex2fv( @quad[ 0 ] );
    end else
      begin
        glTexCoord2f( tX + tU, tY + tV );
        gl_Vertex2fv( @quad[ 0 ] );

        glTexCoord2f( tW - tU, tY + tV );
        gl_Vertex2fv( @quad[ 1 ] );

        glTexCoord2f( tW - tU, tH - tV );
        gl_Vertex2fv( @quad[ 2 ] );

        glTexCoord2f( tW - tU, tH - tV );
        gl_Vertex2fv( @quad[ 2 ] );

        glTexCoord2f( tX + tU, tH - tV );
        gl_Vertex2fv( @quad[ 3 ] );

        glTexCoord2f( tX + tU, tY + tV );
        gl_Vertex2fv( @quad[ 0 ] );
      end;

  if not b2d_Started Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure tiles2d_Draw;
  var
    w, h, tX, tY, tU, tV, u, v   : Single;
    i, j, aI, aJ, bI, bJ, tI, tJ : Integer;
begin
  if not Assigned( Texture ) Then exit;

  i  := Round( Tiles.Size.W );
  j  := Round( Tiles.Size.H );
  tX := X;
  tY := Y;

  if tX < 0 Then
    begin
      aI := Round( -tX ) div i;
      bI := Round( ogl_ClipW / scr_ResCX ) div i + aI;
    end else
      begin
        aI := 0;
        bI := Round( ogl_ClipW / scr_ResCX ) div i - Round( tX ) div i;
      end;

  if tY < 0 Then
    begin
      aJ := Round( -tY ) div j;
      bJ := Round( ogl_ClipH / scr_ResCY ) div j + aJ;
    end else
      begin
        aJ := 0;
        bJ := Round( ogl_ClipH / scr_ResCY ) div j - Round( tY ) div j;
      end;

  if ( cam2dGlobal.Zoom.X <> 1 ) or ( cam2dGlobal.Zoom.Y <> 1 ) or ( cam2dGlobal.Angle <> 0 ) Then
    begin
      if ( cam2dZoomX <> cam2dGlobal.Zoom.X ) or ( cam2dZoomY <> cam2dGlobal.Zoom.Y ) Then
        begin
          cam2dZoomX := cam2dGlobal.Zoom.X;
          cam2dZoomY := cam2dGlobal.Zoom.Y;
          ogl_ClipR  := Round( sqrt( sqr( ogl_ClipW / cam2dZoomX ) + sqr( ogl_ClipH / cam2dZoomY ) ) ) div 2;
        end;

      tI := ogl_ClipR div i - Round( ogl_ClipW / scr_ResCX ) div i div 2 + 3;
      tJ := ogl_ClipR div j - Round( ogl_ClipH / scr_ResCY ) div j div 2 + 3;
      DEC( aI, tI );
      INC( bI, tI );
      DEC( aJ, tJ );
      INC( bJ, tJ );
    end;
  if tX >= 0 Then
    INC( aI, Round( ( cam2dGlobal.X - tX ) / i ) - 1 )
  else
    INC( aI, Round( cam2dGlobal.X / i ) - 1 );
  INC( bI, Round( ( cam2dGlobal.X ) / i ) + 1 );
  if tY >= 0 Then
    INC( aJ, Round( ( cam2dGlobal.Y - tY ) / j ) - 1 )
  else
    INC( aJ, Round( cam2dGlobal.Y / j ) - 1 );
  INC( bJ, Round( cam2dGlobal.Y / j ) + 1 );
  if aI < 0 Then aI := 0;
  if aJ < 0 Then aJ := 0;
  if bI >= Tiles.Count.X Then bI := Tiles.Count.X - 1;
  if bJ >= Tiles.Count.Y Then bJ := Tiles.Count.Y - 1;

  if ( not b2d_Started ) or batch2d_Check( GL_TRIANGLES, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture^.ID );

      glBegin( GL_TRIANGLES );
    end;

  if FX and FX_COLOR > 0 Then
    glColor4ub( FX2D_R, FX2D_G, FX2D_B, Alpha )
  else
    glColor4ub( 255, 255, 255, Alpha );

  u := Texture.U / Texture.FramesX;
  v := Texture.V / Texture.FramesY;
  if FX and FX2D_FLIPX > 0 Then tU := u else tU := 0;
  if FX and FX2D_FLIPY > 0 Then tV := v else tV := 0;

  w := Tiles.Size.W;
  h := Tiles.Size.H;
  for i := aI to bI do
    for j := aJ to bJ do
      begin
        // Текстурные координаты
        tY := Tiles.Tiles[ i, j ] div Texture.FramesX;
        tX := Tiles.Tiles[ i, j ] - tY * Texture.FramesX;
        tY := Texture.FramesY - tY;
        if tX = 0 Then
          begin
            tX := Texture.FramesX;
            tY := tY + 1;
          end;
        tX := tX * u;
        tY := tY * v;

        glTexCoord2f( tX - u + tU, tY - tV );
        gl_Vertex2f( x + i * w, y + j * h );

        glTexCoord2f( tX - tU, tY - tV );
        gl_Vertex2f( x + i * w + w, y + j * h );

        glTexCoord2f( tX - tU, tY - v + tV );
        gl_Vertex2f( x + i * w + w, y + j * h + h );

        glTexCoord2f( tX - tU, tY - v + tV );
        gl_Vertex2f( x + i * w + w, y + j * h + h );

        glTexCoord2f( tX - u + tU, tY - v + tV );
        gl_Vertex2f( x + i * w, y + j * h + h );

        glTexCoord2f( tX - u + tU, tY - tV );
        gl_Vertex2f( x + i * w, y + j * h );
      end;

  if not b2d_Started Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

end.
