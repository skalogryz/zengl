{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru.2x4.ru
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_gui_render;

{$I define.inc}

interface
uses
  zgl_gui_types,
  zgl_primitives_2d,
  zgl_text,
  zgl_math, Utils;

const
  COLOR_WINDOW = $535755;
  COLOR_WIDGET = $424644;
  COLOR_EDIT   = $36342E;

procedure gui_DrawButton( const Widget : zglPWidget );
procedure gui_DrawCheckBox( const Widget : zglPWidget );
procedure gui_DrawEdit( const Widget : zglPWidget );

implementation

procedure gui_DrawButton;
  var
    color : DWORD;
begin
  with zglTButtonDesc( Widget.desc^ ), Widget.Rect do
    begin
      color := COLOR_WIDGET;
      if Pressed Then DEC( color, $222222 ) else
      if Widget.mousein  Then INC( color, $222222 );
      pr2d_Rect( X, Y, W, H, color, 255, PR2D_FILL );
      pr2d_Rect( X, Y, W, H, $000000, 255, 0 );

      text_Draw( Font, m_Round( X + ( W - text_GetWidth( Font, Caption, 0, 1 ) ) / 2 ),
                       m_Round( Y + ( H - Font.Height ) / 2 ), Caption, 255, $FFFFFF, 0, 1 );
    end;
end;

procedure gui_DrawCheckBox;
  var
    color : DWORD;
begin
  with zglTCheckBoxDesc( Widget.desc^ ), Widget.Rect do
    begin
      color := COLOR_WIDGET;
      if Widget.mousein Then INC( color, $222222 );
      pr2d_Rect( X, Y, W, H, color, 255, PR2D_FILL );
      pr2d_Rect( X, Y, W, H, $000000, 255, 0 );
      if Checked Then
        pr2d_Rect( X + 2, Y + 2, W - 4, H - 4, color - $222222, 255, PR2D_FILL );

      text_Draw( Font, X + W + Font.Width[ Byte( ' ' ) ],
                       m_Round( Y + ( H - Font.Height ) / 2 ), Caption, 255, $FFFFFF, 0, 1 );
    end;
end;

procedure gui_DrawEdit;
begin
end;

end.
