{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.inf.ua
 *
 * This file is part of ZenFont
 *
 * ZenFont is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * ZenFont is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
program ZenFont;

uses
  uUI,
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_mouse,
  zgl_keyboard,
  zgl_primitives_2d,
  zgl_textures,
  zgl_textures_tga,
  zgl_sprite_2d,
  zgl_font,
  zgl_text,
  zgl_gui_main,
  zgl_gui_types,
  zgl_utils,
  zgl_font_gen;

procedure Init;
  var
    i : Integer;
begin
  fontgen_Init;
  gui_Init;
  ui_Init;

  fg_Font := font_Add;
  // English
  for i := 32 to 126 do
    begin
      fg_CharsUse[ i ] := TRUE;
      INC( fg_Font.Count.Chars );
    end;
  // Europe
  for i := 161 to 255 do
    begin
      fg_CharsUse[ i ] := TRUE;
      INC( fg_Font.Count.Chars );
    end;
  // Russian
  fg_CharsUse[ 1025 ] := TRUE; // Ё
  fg_CharsUse[ 1105 ] := TRUE; // ё
  INC( fg_Font.Count.Chars, 2 );
  for i := 1040 to 1103 do
    begin
      fg_CharsUse[ i ] := TRUE;
      INC( fg_Font.Count.Chars );
    end;
  // Ukranian
  fg_CharsUse[ 1028 ] := TRUE; // Є
  fg_CharsUse[ 1108 ] := TRUE; // є
  fg_CharsUse[ 1030 ] := TRUE; // І
  fg_CharsUse[ 1110 ] := TRUE; // і
  fg_CharsUse[ 1031 ] := TRUE; // Ї
  fg_CharsUse[ 1111 ] := TRUE; // ї
  INC( fg_Font.Count.Chars, 6 );

  fontgen_BuildFont( fg_font, fg_FontList.Items[ 0 ] );
end;

procedure Proc;
begin
  gui_Proc;

  if key_Up( K_ESCAPE ) Then zgl_Exit;

  mouse_ClearState;
  key_ClearState;
end;

procedure Draw;
begin
  pr2d_Rect( 0, 0, 800, 600, $505050, 255, PR2D_FILL );
  pr2d_Rect( 0, 0, fg_PageSize, fg_PageSize, $000000, 255, PR2D_FILL );

  if Assigned( fg_Font.Pages ) Then
    ssprite2d_Draw( fg_Font.Pages[ zglPSpinDesc( sn_cpage.desc ).Value - 1 ], 0, 0, fg_PageSize, fg_PageSize, 0 );

  gui_Draw;
end;

begin
  timer_Add( @Proc, 16 );
  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  zgl_Disable( APP_USE_LOG );

  scr_SetOptions( 800, 600, 32, 0, FALSE, TRUE );
  wnd_SetCaption( 'ZenFont' );
  wnd_ShowCursor( TRUE );

  zgl_Init;
end.
