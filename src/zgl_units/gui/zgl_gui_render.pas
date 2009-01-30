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
  GL,
  zgl_opengl,
  zgl_opengl_simple,
  zgl_gui_types,
  zgl_mouse,
  zgl_primitives_2d,
  zgl_text,
  zgl_math;

const
  COLOR_WINDOW = $646866;
  COLOR_WIDGET = $424644;
  COLOR_LIGHT  = $BBBBBB;
  COLOR_EDIT   = $36342E;

procedure gui_DrawWidget( const Widget : zglPWidget );

procedure gui_DrawButton     ( const Widget : zglPWidget );
procedure gui_DrawCheckBox   ( const Widget : zglPWidget );
procedure gui_DrawRadioButton( const Widget : zglPWidget );
procedure gui_DrawLabel      ( const Widget : zglPWidget );
procedure gui_DrawEditBox    ( const Widget : zglPWidget );
procedure gui_DrawListBox    ( const Widget : zglPWidget );
procedure gui_DrawGroupBox   ( const Widget : zglPWidget );
procedure gui_DrawSpin       ( const Widget : zglPWidget );

implementation

procedure gui_DrawWidget;
  var
    w : zglPWidget;
begin
  if Assigned( Widget.OnDraw ) Then Widget.OnDraw( Widget );
  if Assigned( Widget.child ) Then
    begin
      w := Widget.child;
      repeat
        //scissor_Begin( Round( w.Parent.rect.X ), Round( w.Parent.rect.Y ), Round( w.Parent.rect.W ), Round( w.Parent.rect.H ) );
        w := w.Next;
        gui_DrawWidget( w );
        //scissor_End;
      until not Assigned( w.Next );
    end;
end;

procedure gui_DrawButton;
  var
    color : DWORD;
begin
  with zglTButtonDesc( Widget.desc^ ), Widget.rect do
    begin
      color := COLOR_WIDGET;
      if Widget.mousein  Then INC( color, $222222 );
      pr2d_Rect( X, Y, W, H, color, 255, PR2D_FILL );
      pr2d_Rect( X, Y, W, H, $000000, 255, 0 );

      color := COLOR_LIGHT;
      if Pressed Then color := COLOR_WIDGET - $222222;
      pr2d_Line( X + 1, Y + 1, X + W - 2, Y + 1, color, 255, 0 );
      pr2d_Line( X + 1, Y + 1, X + 1, Y + H - 2, color, 255, 0 );
      color := COLOR_WIDGET - $222222;
      if Pressed Then color := COLOR_LIGHT;
      pr2d_Line( X + 1, Y + H - 2, X + W - 2, Y + H - 2, color, 255, 0 );
      pr2d_Line( X + W - 2, Y + 1, X + W - 2, Y + H - 2, color, 255, 0 );

      text_Draw( Font, Round( X + ( W - text_GetWidth( Font, Caption, 0, 1 ) ) / 2 ) + Byte( Pressed ),
                       Round( Y + ( H - Font.Height ) / 2 ) + Byte( Pressed ), Caption, 255, $FFFFFF, 0, 1 );
    end;
end;

procedure gui_DrawCheckBox;
  var
    color : DWORD;
begin
  with zglTCheckBoxDesc( Widget.desc^ ), Widget.rect do
    begin
      color := COLOR_WIDGET;
      if Widget.mousein Then INC( color, $222222 );
      pr2d_Rect( X, Y, W, H, color, 255, PR2D_FILL );
      pr2d_Rect( X, Y, W, H, $000000, 255, 0 );
      if Checked Then
        pr2d_Rect( X + 2, Y + 2, W - 4, H - 4, color - $222222, 255, PR2D_FILL );

      text_Draw( Font, X + W + Font.Width[ Byte( ' ' ) ], Round( Y + ( H - Font.Height ) / 2 ), Caption, 255, $FFFFFF, 0, 1 );
    end;
end;

procedure gui_DrawRadioButton;
begin
end;

procedure gui_DrawLabel;
begin
end;

procedure gui_DrawEditBox;
begin
  with zglTEditBoxDesc( Widget.desc^ ), Widget.rect do
    begin
      pr2d_Rect( X, Y, W, H, COLOR_EDIT, 255, PR2D_FILL );
      pr2d_Rect( X, Y, W, H, $000000, 255, 0 );

      text_Draw( Font, X + Font.Width[ Byte( ' ' ) ], Round( Y + ( H - Font.Height ) / 2 ), Text, 255, $FFFFFF, 0, 1 );
    end;
end;

procedure gui_DrawListBox;
begin
end;

procedure gui_DrawGroupBox;
  var
    th : Integer;
begin
  with zglTGroupBoxDesc( Widget.desc^ ), Widget.rect do
    begin
      th := Trunc( Font.Height / 2 );
      pr2d_Rect( X, Y, W, H, COLOR_WINDOW, 255, PR2D_FILL );

      pr2d_Rect( X, Y, W - 1, H - 1, COLOR_WIDGET, 255, 0 );
      pr2d_Rect( X + 1, Y + 1, W - 1, H - 1, COLOR_LIGHT, 255, 0 );
      pr2d_Rect( X + Font.Width[ Byte( ' ' ) ], Y, text_GetWidth( Font, Caption, 0, 1 ), th, COLOR_WINDOW, 255, PR2D_FILL );

      text_Draw( Font, X + Font.Width[ Byte( ' ' ) ], Y - th, Caption, 255, $FFFFFF, 0, 1 );
    end;
end;

procedure gui_DrawSpin;
  var
    color : DWORD;
begin
  with zglTSpinDesc( Widget.desc^ ), Widget.rect do
    begin
      color := COLOR_WIDGET;
      if ( Widget.mousein ) and ( mouse_Y < Y + H / 2 ) Then INC( color, $222222 );
      pr2d_Rect( X, Y, W, H, color, 255, PR2D_FILL );
      pr2d_Rect( X, Y, W, H / 2, $000000, 255, 0 );
      glColor3f( 0, 0, 0 );
      glBegin( GL_TRIANGLES );
        gl_Vertex2f( X + W / 2 + Byte( UPressed ), Y + 2 + Byte( UPressed ) );
        gl_Vertex2f( X + W - 2 + Byte( UPressed ), Y + H / 2 - 2 + Byte( UPressed ) );
        gl_Vertex2f( X + 2 + Byte( UPressed ),     Y + H / 2 - 2 + Byte( UPressed ) );
      glEnd;
      color := COLOR_LIGHT;
      if UPressed Then color := COLOR_WIDGET - $222222;
      pr2d_Line( X + 1, Y + 1, X + W - 2, Y + 1, color, 255, 0 );
      pr2d_Line( X + 1, Y + 1, X + 1, Y + H / 2 - 2, color, 255, 0 );
      color := COLOR_WIDGET - $222222;
      if UPressed Then color := COLOR_LIGHT;
      pr2d_Line( X + 1, Y + H / 2 - 2, X + W - 2, Y + H / 2 - 2, color, 255, 0 );
      pr2d_Line( X + W - 2, Y + 1, X + W - 2, Y + H / 2 - 2, color, 255, 0 );

      color := COLOR_WIDGET;
      if ( Widget.mousein ) and ( mouse_Y > Y + H / 2 ) Then INC( color, $222222 );
      pr2d_Rect( X, Y + H / 2, W, H / 2, color, 255, PR2D_FILL );
      pr2d_Rect( X, Y + H / 2, W, H / 2, $000000, 255, 0 );
      glColor3f( 0, 0, 0 );
      glBegin( GL_TRIANGLES );
        gl_Vertex2f( X + 2 + Byte( DPressed ),     Y + H / 2 + 2 + Byte( DPressed ) );
        gl_Vertex2f( X + W - 2 + Byte( DPressed ), Y + H / 2 + 2 + Byte( DPressed ) );
        gl_Vertex2f( X + W / 2 + Byte( DPressed ), Y + H - 2 + Byte( DPressed ) );
      glEnd;
      color := COLOR_LIGHT;
      if DPressed Then color := COLOR_WIDGET - $222222;
      pr2d_Line( X + 1, Y + H / 2 + 1, X + W - 2, Y + H / 2 + 1, color, 255, 0 );
      pr2d_Line( X + 1, Y + H / 2 + 1, X + 1, Y + H - 2, color, 255, 0 );
      color := COLOR_WIDGET - $222222;
      if DPressed Then color := COLOR_LIGHT;
      pr2d_Line( X + 1, Y + H - 2, X + W - 2, Y + H - 2, color, 255, 0 );
      pr2d_Line( X + W - 2, Y + H / 2 + 1, X + W - 2, Y + H - 2, color, 255, 0 );
    end;
end;

end.
