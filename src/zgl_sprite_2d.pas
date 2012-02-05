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
  zgl_types,
  zgl_fx,
  zgl_textures,
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

procedure texture2d_Draw( Texture : zglPTexture; const TexCoord : array of zglTPoint2D; X, Y, W, H, Angle : Single; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
procedure ssprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Single; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
procedure asprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Single; Frame : Word; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
procedure csprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Single; const CutRect : zglTRect; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
procedure tiles2d_Draw( Texture : zglPTexture; X, Y : Single; Tiles : zglPTiles2D; Alpha : Byte = 255; FX : LongWord = FX_BLEND );

implementation
uses
  zgl_application,
  zgl_main,
  zgl_screen,
  zgl_direct3d,
  zgl_direct3d_all,
  zgl_render_2d,
  zgl_camera_2d;

const
  FLIP_TEXCOORD : array[ 0..3 ] of zglTTexCoordIndex = ( ( 0, 1, 2, 3 ), ( 1, 0, 3, 2 ), ( 3, 2, 1, 0 ), ( 2, 3, 0, 1 ) );

procedure texture2d_Draw( Texture : zglPTexture; const TexCoord : array of zglTPoint2D; X, Y, W, H, Angle : Single; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    tci  : zglPTexCoordIndex;
    p    : zglPPoint2D;

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
      X := X + ( W - W * fx2dSX ) / 2;
      Y := Y + ( H - H * fx2dSY ) / 2;
      W := W * fx2dSX;
      H := H * fx2dSY;
    end;

  if render2dClip Then
    if FX and FX2D_VCHANGE = 0 Then
      begin
        if not sprite2d_InScreen( X, Y, W, H, Angle ) Then Exit;
      end else
        begin
          mX := min( X + fx2dVX1, min( X + W + fx2dVX2, min( X + W + fx2dVX3, X + fx2dVX4 ) ) );
          mY := min( Y + fx2dVY1, min( Y + fx2dVY2, min( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) );
          mW := max( X + fx2dVX1, max( X + W + fx2dVX2, max( X + W + fx2dVX3, X + fx2dVX4 ) ) ) - mx;
          mH := max( Y + fx2dVY1, max( Y + fx2dVY2, max( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) ) - mY;
          if not sprite2d_InScreen( mX, mY, mW + abs( X - mX ) + abs( mW - W ), mH + abs( Y - mY ) + abs( mH - H ), Angle ) Then Exit;
        end;

  // Текстурные координаты
  tci := @FLIP_TEXCOORD[ FX and FX2D_FLIPX + FX and FX2D_FLIPY ];

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      x1 := -W / 2;
      y1 := -H / 2;
      x2 := -x1;
      y2 := -y1;
      cX :=  X + x2;
      cY :=  Y + y2;

      m_SinCos( Angle * deg2rad, s, c );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := x1 * c - y1 * s + cX;
          p.Y := x1 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y1 * s + cX;
          p.Y := x2 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y2 * s + cX;
          p.Y := x2 * s + y2 * c + cY;
          INC( p );
          p.X := x1 * c - y2 * s + cX;
          p.Y := x1 * s + y2 * c + cY;
        end else
          begin
            p := @quad[ 0 ];
            p.X := ( x1 + fx2dVX1 ) * c - ( y1 + fx2dVY1 ) * s + cX;
            p.Y := ( x1 + fx2dVX1 ) * s + ( y1 + fx2dVY1 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX2 ) * c - ( y1 + fx2dVY2 ) * s + cX;
            p.Y := ( x2 + fx2dVX2 ) * s + ( y1 + fx2dVY2 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX3 ) * c - ( y2 + fx2dVY3 ) * s + cX;
            p.Y := ( x2 + fx2dVX3 ) * s + ( y2 + fx2dVY3 ) * c + cY;
            INC( p );
            p.X := ( x1 + fx2dVX4 ) * c - ( y2 + fx2dVY4 ) * s + cX;
            p.Y := ( x1 + fx2dVX4 ) * s + ( y2 + fx2dVY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := X;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y + H;
          INC( p );
          p.X := X;
          p.Y := Y + H;
        end else
          begin
            p := @quad[ 0 ];
            p.X := X     + fx2dVX1;
            p.Y := Y     + fx2dVY1;
            INC( p );
            p.X := X + W + fx2dVX2;
            p.Y := Y     + fx2dVY2;
            INC( p );
            p.X := X + W + fx2dVX3;
            p.Y := Y + H + fx2dVY3;
            INC( p );
            p.X := X     + fx2dVX4;
            p.Y := Y + H + fx2dVY4;
          end;

  if ( not b2dStarted ) or batch2d_Check( GL_QUADS, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture.ID );

      glBegin( GL_QUADS );
    end;

  if FX and FX_COLOR > 0 Then
    begin
      fx2dAlpha^ := Alpha;
      glColor4ubv( @fx2dColor[ 0 ] );
    end else
      begin
        fx2dAlphaDef^ := Alpha;
        glColor4ubv( @fx2dColorDef[ 0 ] );
      end;

  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci[ 0 ] ] );
      glVertex2fv( @quad[ 0 ] );

      glColor4ubv( @fx2dVCA2[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci[ 1 ] ] );
      glVertex2fv( @quad[ 1 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci[ 2 ] ] );
      glVertex2fv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA4[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci[ 3 ] ] );
      glVertex2fv( @quad[ 3 ] );
    end else
      begin
        glTexCoord2fv( @TexCoord[ tci[ 0 ] ] );
        glVertex2fv( @quad[ 0 ] );

        glTexCoord2fv( @TexCoord[ tci[ 1 ] ] );
        glVertex2fv( @quad[ 1 ] );

        glTexCoord2fv( @TexCoord[ tci[ 2 ] ] );
        glVertex2fv( @quad[ 2 ] );

        glTexCoord2fv( @TexCoord[ tci[ 3 ] ] );
        glVertex2fv( @quad[ 3 ] );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure ssprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Single; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    p    : zglPPoint2D;
    tc   : zglPTextureCoord;
    tci  : zglPTexCoordIndex;

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
      X := X + ( W - W * fx2dSX ) / 2;
      Y := Y + ( H - H * fx2dSY ) / 2;
      W := W * fx2dSX;
      H := H * fx2dSY;
    end;

  if render2dClip Then
    if FX and FX2D_VCHANGE = 0 Then
      begin
        if not sprite2d_InScreen( X, Y, W, H, Angle ) Then Exit;
      end else
        begin
          mX := min( X + fx2dVX1, min( X + W + fx2dVX2, min( X + W + fx2dVX3, X + fx2dVX4 ) ) );
          mY := min( Y + fx2dVY1, min( Y + fx2dVY2, min( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) );
          mW := max( X + fx2dVX1, max( X + W + fx2dVX2, max( X + W + fx2dVX3, X + fx2dVX4 ) ) ) - mx;
          mH := max( Y + fx2dVY1, max( Y + fx2dVY2, max( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) ) - mY;
          if not sprite2d_InScreen( mX, mY, mW + abs( X - mX ) + abs( mW - W ), mH + abs( Y - mY ) + abs( mH - H ), Angle ) Then Exit;
        end;

  // Текстурные координаты
  tci := @FLIP_TEXCOORD[ FX and FX2D_FLIPX + FX and FX2D_FLIPY ];
  tc  := @Texture.FramesCoord[ 0 ];

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      x1 := -W / 2;
      y1 := -H / 2;
      x2 := -x1;
      y2 := -y1;
      cX :=  X + x2;
      cY :=  Y + y2;

      m_SinCos( Angle * deg2rad, s, c );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := x1 * c - y1 * s + cX;
          p.Y := x1 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y1 * s + cX;
          p.Y := x2 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y2 * s + cX;
          p.Y := x2 * s + y2 * c + cY;
          INC( p );
          p.X := x1 * c - y2 * s + cX;
          p.Y := x1 * s + y2 * c + cY;
        end else
          begin
            p := @quad[ 0 ];
            p.X := ( x1 + fx2dVX1 ) * c - ( y1 + fx2dVY1 ) * s + cX;
            p.Y := ( x1 + fx2dVX1 ) * s + ( y1 + fx2dVY1 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX2 ) * c - ( y1 + fx2dVY2 ) * s + cX;
            p.Y := ( x2 + fx2dVX2 ) * s + ( y1 + fx2dVY2 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX3 ) * c - ( y2 + fx2dVY3 ) * s + cX;
            p.Y := ( x2 + fx2dVX3 ) * s + ( y2 + fx2dVY3 ) * c + cY;
            INC( p );
            p.X := ( x1 + fx2dVX4 ) * c - ( y2 + fx2dVY4 ) * s + cX;
            p.Y := ( x1 + fx2dVX4 ) * s + ( y2 + fx2dVY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := X;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y + H;
          INC( p );
          p.X := X;
          p.Y := Y + H;
        end else
          begin
            p := @quad[ 0 ];
            p.X := X     + fx2dVX1;
            p.Y := Y     + fx2dVY1;
            INC( p );
            p.X := X + W + fx2dVX2;
            p.Y := Y     + fx2dVY2;
            INC( p );
            p.X := X + W + fx2dVX3;
            p.Y := Y + H + fx2dVY3;
            INC( p );
            p.X := X     + fx2dVX4;
            p.Y := Y + H + fx2dVY4;
          end;

  if ( not b2dStarted ) or batch2d_Check( GL_QUADS, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture.ID );

      glBegin( GL_QUADS );
    end;

  if FX and FX_COLOR > 0 Then
    begin
      fx2dAlpha^ := Alpha;
      glColor4ubv( @fx2dColor[ 0 ] );
    end else
      begin
        fx2dAlphaDef^ := Alpha;
        glColor4ubv( @fx2dColorDef[ 0 ] );
      end;

  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 0 ] ] );
      glVertex2fv( @quad[ 0 ] );

      glColor4ubv( @fx2dVCA2[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 1 ] ] );
      glVertex2fv( @quad[ 1 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 2 ] ] );
      glVertex2fv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA4[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 3 ] ] );
      glVertex2fv( @quad[ 3 ] );
    end else
      begin
        glTexCoord2fv( @tc[ tci[ 0 ] ] );
        glVertex2fv( @quad[ 0 ] );

        glTexCoord2fv( @tc[ tci[ 1 ] ] );
        glVertex2fv( @quad[ 1 ] );

        glTexCoord2fv( @tc[ tci[ 2 ] ] );
        glVertex2fv( @quad[ 2 ] );

        glTexCoord2fv( @tc[ tci[ 3 ] ] );
        glVertex2fv( @quad[ 3 ] );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure asprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Single; Frame : Word; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    p    : zglPPoint2D;
    tc   : zglPTextureCoord;
    tci  : zglPTexCoordIndex;
    fc   : Integer;

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
      X := X + ( W - W * fx2dSX ) / 2;
      Y := Y + ( H - H * fx2dSY ) / 2;
      W := W * fx2dSX;
      H := H * fx2dSY;
    end;

  if render2dClip Then
    if FX and FX2D_VCHANGE = 0 Then
      begin
        if not sprite2d_InScreen( X, Y, W, H, Angle ) Then Exit;
      end else
        begin
          mX := min( X + fx2dVX1, min( X + W + fx2dVX2, min( X + W + fx2dVX3, X + fx2dVX4 ) ) );
          mY := min( Y + fx2dVY1, min( Y + fx2dVY2, min( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) );
          mW := max( X + fx2dVX1, max( X + W + fx2dVX2, max( X + W + fx2dVX3, X + fx2dVX4 ) ) ) - mx;
          mH := max( Y + fx2dVY1, max( Y + fx2dVY2, max( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) ) - mY;
          if not sprite2d_InScreen( mX, mY, mW + abs( X - mX ) + abs( mW - W ), mH + abs( Y - mY ) + abs( mH - H ), Angle ) Then Exit;
        end;

  // Текстурные координаты
  fc := Texture.FramesX * Texture.FramesY;
  if Frame > fc Then
    DEC( Frame, ( ( Frame - 1 ) div fc ) * fc )
  else
    if Frame < 1 Then
      INC( Frame, ( abs( Frame ) div fc + 1 ) * fc );
  tci := @FLIP_TEXCOORD[ FX and FX2D_FLIPX + FX and FX2D_FLIPY ];
  tc  := @Texture.FramesCoord[ Frame ];

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      x1 := -W / 2;
      y1 := -H / 2;
      x2 := -x1;
      y2 := -y1;
      cX :=  X + x2;
      cY :=  Y + y2;

      m_SinCos( Angle * deg2rad, s, c );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := x1 * c - y1 * s + cX;
          p.Y := x1 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y1 * s + cX;
          p.Y := x2 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y2 * s + cX;
          p.Y := x2 * s + y2 * c + cY;
          INC( p );
          p.X := x1 * c - y2 * s + cX;
          p.Y := x1 * s + y2 * c + cY;
        end else
          begin
            p := @quad[ 0 ];
            p.X := ( x1 + fx2dVX1 ) * c - ( y1 + fx2dVY1 ) * s + cX;
            p.Y := ( x1 + fx2dVX1 ) * s + ( y1 + fx2dVY1 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX2 ) * c - ( y1 + fx2dVY2 ) * s + cX;
            p.Y := ( x2 + fx2dVX2 ) * s + ( y1 + fx2dVY2 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX3 ) * c - ( y2 + fx2dVY3 ) * s + cX;
            p.Y := ( x2 + fx2dVX3 ) * s + ( y2 + fx2dVY3 ) * c + cY;
            INC( p );
            p.X := ( x1 + fx2dVX4 ) * c - ( y2 + fx2dVY4 ) * s + cX;
            p.Y := ( x1 + fx2dVX4 ) * s + ( y2 + fx2dVY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := X;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y + H;
          INC( p );
          p.X := X;
          p.Y := Y + H;
        end else
          begin
            p := @quad[ 0 ];
            p.X := X     + fx2dVX1;
            p.Y := Y     + fx2dVY1;
            INC( p );
            p.X := X + W + fx2dVX2;
            p.Y := Y     + fx2dVY2;
            INC( p );
            p.X := X + W + fx2dVX3;
            p.Y := Y + H + fx2dVY3;
            INC( p );
            p.X := X     + fx2dVX4;
            p.Y := Y + H + fx2dVY4;
          end;

  if ( not b2dStarted ) or batch2d_Check( GL_QUADS, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture^.ID );

      glBegin( GL_QUADS );
    end;

  if FX and FX_COLOR > 0 Then
    begin
      fx2dAlpha^ := Alpha;
      glColor4ubv( @fx2dColor[ 0 ] );
    end else
      begin
        fx2dAlphaDef^ := Alpha;
        glColor4ubv( @fx2dColorDef[ 0 ] );
      end;

  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 0 ] ] );
      glVertex2fv( @quad[ 0 ] );

      glColor4ubv( @fx2dVCA2[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 1 ] ] );
      glVertex2fv( @quad[ 1 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 2 ] ] );
      glVertex2fv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA4[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 3 ] ] );
      glVertex2fv( @quad[ 3 ] );
    end else
      begin
        glTexCoord2fv( @tc[ tci[ 0 ] ] );
        glVertex2fv( @quad[ 0 ] );

        glTexCoord2fv( @tc[ tci[ 1 ] ] );
        glVertex2fv( @quad[ 1 ] );

        glTexCoord2fv( @tc[ tci[ 2 ] ] );
        glVertex2fv( @quad[ 2 ] );

        glTexCoord2fv( @tc[ tci[ 3 ] ] );
        glVertex2fv( @quad[ 3 ] );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure csprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Single; const CutRect : zglTRect; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    p    : zglPPoint2D;

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
      X := X + ( W - W * fx2dSX ) / 2;
      Y := Y + ( H - H * fx2dSY ) / 2;
      W := W * fx2dSX;
      H := H * fx2dSY;
    end;

  if render2dClip Then
    if FX and FX2D_VCHANGE = 0 Then
      begin
        if not sprite2d_InScreen( X, Y, W, H, Angle ) Then Exit;
      end else
        begin
          mX := min( X + fx2dVX1, min( X + W + fx2dVX2, min( X + W + fx2dVX3, X + fx2dVX4 ) ) );
          mY := min( Y + fx2dVY1, min( Y + fx2dVY2, min( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) );
          mW := max( X + fx2dVX1, max( X + W + fx2dVX2, max( X + W + fx2dVX3, X + fx2dVX4 ) ) ) - mx;
          mH := max( Y + fx2dVY1, max( Y + fx2dVY2, max( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) ) - mY;
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
      x2 := -x1;
      y2 := -y1;
      cX :=  X + x2;
      cY :=  Y + y2;

      m_SinCos( Angle * deg2rad, s, c );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := x1 * c - y1 * s + cX;
          p.Y := x1 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y1 * s + cX;
          p.Y := x2 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y2 * s + cX;
          p.Y := x2 * s + y2 * c + cY;
          INC( p );
          p.X := x1 * c - y2 * s + cX;
          p.Y := x1 * s + y2 * c + cY;
        end else
          begin
            p := @quad[ 0 ];
            p.X := ( x1 + fx2dVX1 ) * c - ( y1 + fx2dVY1 ) * s + cX;
            p.Y := ( x1 + fx2dVX1 ) * s + ( y1 + fx2dVY1 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX2 ) * c - ( y1 + fx2dVY2 ) * s + cX;
            p.Y := ( x2 + fx2dVX2 ) * s + ( y1 + fx2dVY2 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX3 ) * c - ( y2 + fx2dVY3 ) * s + cX;
            p.Y := ( x2 + fx2dVX3 ) * s + ( y2 + fx2dVY3 ) * c + cY;
            INC( p );
            p.X := ( x1 + fx2dVX4 ) * c - ( y2 + fx2dVY4 ) * s + cX;
            p.Y := ( x1 + fx2dVX4 ) * s + ( y2 + fx2dVY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := X;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y + H;
          INC( p );
          p.X := X;
          p.Y := Y + H;
        end else
          begin
            p := @quad[ 0 ];
            p.X := X     + fx2dVX1;
            p.Y := Y     + fx2dVY1;
            INC( p );
            p.X := X + W + fx2dVX2;
            p.Y := Y     + fx2dVY2;
            INC( p );
            p.X := X + W + fx2dVX3;
            p.Y := Y + H + fx2dVY3;
            INC( p );
            p.X := X     + fx2dVX4;
            p.Y := Y + H + fx2dVY4;
          end;

  if ( not b2dStarted ) or batch2d_Check( GL_QUADS, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture^.ID );

      glBegin( GL_QUADS );
    end;

  if FX and FX_COLOR > 0 Then
    begin
      fx2dAlpha^ := Alpha;
      glColor4ubv( @fx2dColor[ 0 ] );
    end else
      begin
        fx2dAlphaDef^ := Alpha;
        glColor4ubv( @fx2dColorDef[ 0 ] );
      end;

  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2f( tX + tU, tY + tV );
      glVertex2fv( @quad[ 0 ] );

      glColor4ubv( @fx2dVCA2[ 0 ] );
      glTexCoord2f( tW - tU, tY + tV );
      glVertex2fv( @quad[ 1 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2f( tW - tU, tH - tV );
      glVertex2fv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA4[ 0 ] );
      glTexCoord2f( tX + tU, tH - tV );
      glVertex2fv( @quad[ 3 ] );
    end else
      begin
        glTexCoord2f( tX + tU, tY + tV );
        glVertex2fv( @quad[ 0 ] );

        glTexCoord2f( tW - tU, tY + tV );
        glVertex2fv( @quad[ 1 ] );

        glTexCoord2f( tW - tU, tH - tV );
        glVertex2fv( @quad[ 2 ] );

        glTexCoord2f( tX + tU, tH - tV );
        glVertex2fv( @quad[ 3 ] );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure tiles2d_Draw( Texture : zglPTexture; X, Y : Single; Tiles : zglPTiles2D; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    w, h, tX, tY, tU, tV, u, v   : Single;
    i, j, aI, aJ, bI, bJ, tI, tJ : Integer;
begin
  if ( not Assigned( Texture ) ) or ( not Assigned( Tiles ) ) Then exit;

  i  := Round( Tiles.Size.W );
  j  := Round( Tiles.Size.H );
  tX := X;
  tY := Y;

  if tX < 0 Then
    begin
      aI := Round( -tX ) div i;
      bI := Round( oglClipW / scrResCX ) div i + aI;
    end else
      begin
        aI := 0;
        bI := Round( oglClipW / scrResCX ) div i - Round( tX ) div i;
      end;

  if tY < 0 Then
    begin
      aJ := Round( -tY ) div j;
      bJ := Round( oglClipH / scrResCY ) div j + aJ;
    end else
      begin
        aJ := 0;
        bJ := Round( oglClipH / scrResCY ) div j - Round( tY ) div j;
      end;

  if not cam2d.OnlyXY Then
    begin
      tI := oglClipR div i - Round( oglWidth / scrResCX ) div i div 2 + 3;
      tJ := oglClipR div j - Round( oglHeight / scrResCY ) div j div 2 + 3;
      DEC( aI, tI );
      INC( bI, tI );
      DEC( aJ, tJ );
      INC( bJ, tJ );
    end;
  if tX >= 0 Then
    INC( aI, Round( ( cam2d.Global.X - tX ) / i ) - 1 )
  else
    INC( aI, Round( cam2d.Global.X / i ) - 1 );
  INC( bI, Round( ( cam2d.Global.X ) / i ) + 1 );
  if tY >= 0 Then
    INC( aJ, Round( ( cam2d.Global.Y - tY ) / j ) - 1 )
  else
    INC( aJ, Round( cam2d.Global.Y / j ) - 1 );
  INC( bJ, Round( cam2d.Global.Y / j ) + 1 );
  if aI < 0 Then aI := 0;
  if aJ < 0 Then aJ := 0;
  if bI >= Tiles.Count.X Then bI := Tiles.Count.X - 1;
  if bJ >= Tiles.Count.Y Then bJ := Tiles.Count.Y - 1;

  if ( not b2dStarted ) or batch2d_Check( GL_QUADS, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture^.ID );

      glBegin( GL_QUADS );
    end;

  if FX and FX_COLOR > 0 Then
    begin
      fx2dAlpha^ := Alpha;
      glColor4ubv( @fx2dColor[ 0 ] );
    end else
      begin
        fx2dAlphaDef^ := Alpha;
        glColor4ubv( @fx2dColorDef[ 0 ] );
      end;

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
        glVertex2f( x + i * w, y + j * h );

        glTexCoord2f( tX - tU, tY - tV );
        glVertex2f( x + i * w + w, y + j * h );

        glTexCoord2f( tX - tU, tY - v + tV );
        glVertex2f( x + i * w + w, y + j * h + h );

        glTexCoord2f( tX - u + tU, tY - v + tV );
        glVertex2f( x + i * w, y + j * h + h );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

end.
