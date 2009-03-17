{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.ru
 *
 * This file is part of FontGen
 *
 * FontGen is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.

 * FontGen is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
}
program fontgen;

uses
  zgl_freetype,
  zgl_main,
  zgl_opengl_simple,
  zgl_const,
  zgl_timers,
  zgl_log,
  zgl_keyboard,
  zgl_textures,
  zgl_textures_tga,
  zgl_font,
  zgl_text,
  zgl_sprite_2d,
  zgl_file,
  zgl_utils,
  zgl_math_2d,
  math;

procedure Init;
  var
    i : Integer;
begin
  fnt_Init;
  fnt_Load( 'impact.ttf' );
  // English
  for i := 32 to 126 do
    begin
      fntChars[ i ] := TRUE;
      INC( Font.Count.Chars );
    end;
  // Russian
  for i := 1040 to 1103 do
    begin
      fntChars[ i ] := TRUE;
      INC( Font.Count.Chars );
    end;
  fnt_SetCharSize( 24 );
  fnt_GenChars;
  fnt_GenPages;
  fnt_Save( 'test' );
  fnt_FreeChars;
  fnt_Free;
  fnt_Done;

  {Font := font_LoadFromFile( 'test.zfi' );
  Font.Pages[ 0 ] := tex_LoadFromFile( 'test_0.tga', $FF000000, TEX_DEFAULT_2D );}
  {Font.Pages[ 1 ] := tex_LoadFromFile( 'test_1.tga', $FF000000, TEX_DEFAULT_2D );
  Font.Pages[ 2 ] := tex_LoadFromFile( 'test_2.tga', $FF000000, TEX_DEFAULT_2D );
  Font.Pages[ 3 ] := tex_LoadFromFile( 'test_3.tga', $FF000000, TEX_DEFAULT_2D );}
end;

procedure Draw;
  var
    rect : zglTRect;
    s : String;
begin
  Set2DMode;
  ssprite2d_Draw( Font.Pages[ 0 ], 0, 0, 512, 512, 0 );
  text_Draw( Font, 0, 600 - Font.MaxHeight, 'FPS:Щ ' + u_IntToStr( zgl_Get( SYS_FPS ) ) );
  rect.X := 0;
  rect.Y := 0;
  rect.W := 100;
  rect.H := 500;
end;

procedure Proc;
begin
  if key_Down( K_ESCAPE ) Then zgl_Exit;
  key_ClearState;
end;

Begin
  timer_Add( @Proc, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  zgl_Init;
End.
