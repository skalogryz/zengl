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
unit zgl_gui_render;

{$I zgl_config.cfg}

interface
uses
  zgl_gui_types;

const
  COLOR_WINDOW = $666864;
  COLOR_WIDGET = $444642;
  COLOR_LIGHT  = $BBBBBB;
  COLOR_DARK   = $222420;
  COLOR_EDIT   = $2E3436;
  COLOR_SELECT = $95B0DB;

  SCROLL_SIZE  = 16;

  SCROLL_UP    = 0;
  SCROLL_DOWN  = 1;
  SCROLL_LEFT  = 2;
  SCROLL_RIGHT = 3;

procedure gui_DrawWidget( const Widget : zglPWidget );

procedure gui_DrawButton( const Widget : zglPWidget );
procedure gui_DrawCheckBox( const Widget : zglPWidget );
procedure gui_DrawRadioButton( const Widget : zglPWidget );
procedure gui_DrawLabel( const Widget : zglPWidget );
procedure gui_DrawEditBox( const Widget : zglPWidget );
procedure gui_DrawListBox( const Widget : zglPWidget );
procedure gui_DrawComboBox( const Widget : zglPWidget );
procedure gui_DrawGroupBox( const Widget : zglPWidget );
procedure gui_DrawScrollBox( const Widget : zglPWidget );
procedure gui_DrawSpin( const Widget : zglPWidget );
procedure gui_DrawScrollBar( const Widget : zglPWidget );

implementation
uses
  zgl_opengl_all,
  zgl_opengl_simple,
  zgl_render_2d,
  zgl_mouse,
  zgl_primitives_2d,
  zgl_text,
  zgl_gui_main,
  zgl_gui_utils,
  zgl_math_2d,
  zgl_collision_2d;

procedure _lock;
begin
end;

procedure _unlock;
begin
end;

procedure _button_draw( const X, Y, W, H : Single; const Pressed : Boolean );
  var
    color : LongWord;
begin
  color := COLOR_WIDGET;
  pr2d_Rect( X, Y, W, H, color, 255, PR2D_FILL );
  pr2d_Rect( X, Y, W, H, $000000, 255, 0 );

  color := COLOR_LIGHT;
  if Pressed Then color := COLOR_DARK;
  pr2d_Line( X + 1, Y + 1, X + W - 1, Y + 1, color, 255, 0 );
  pr2d_Line( X + 1, Y + 1, X + 1, Y + H - 1, color, 255, 0 );
  color := COLOR_DARK;
  if Pressed Then color := COLOR_LIGHT;
  pr2d_Line( X + 1, Y + H - 2, X + W - 1, Y + H - 2, color, 255, 0 );
  pr2d_Line( X + W - 2, Y + 1, X + W - 2, Y + H - 1, color, 255, 0 );
  if pressed Then
    pr2d_Rect( X, Y, W, H, COLOR_SELECT, 25, PR2D_FILL );
end;

procedure _scroll_draw( const X, Y, W, H : Single; const _type : Integer; const Pressed : Boolean );
begin
  _button_draw( X, Y, W, H, Pressed );
  if b2d_Started Then
    batch2d_Flush();
  glColor4f( 0, 0, 0, 1 );
  case _type of
    SCROLL_UP:
      begin
        glBegin( GL_TRIANGLES );
          gl_Vertex2f( X + W / 2 + Byte( Pressed ), Y + 3 + Byte( Pressed )     );
          gl_Vertex2f( X + W - 3 + Byte( Pressed ), Y + H - 3 + Byte( Pressed ) );
          gl_Vertex2f( X + 3 + Byte( Pressed ),     Y + H - 3 + Byte( Pressed ) );
        glEnd();
      end;
    SCROLL_DOWN:
      begin
        glBegin( GL_TRIANGLES );
          gl_Vertex2f( X + 3 + Byte( Pressed ),     Y + 3 + Byte( Pressed )     );
          gl_Vertex2f( X + W - 3 + Byte( Pressed ), Y + 3 + Byte( Pressed )     );
          gl_Vertex2f( X + W / 2 + Byte( Pressed ), Y + H - 3 + Byte( Pressed ) );
        glEnd();
      end;
    SCROLL_LEFT:
      begin
        glBegin( GL_TRIANGLES );
          gl_Vertex2f( X + 3 + Byte( Pressed ),     Y + H / 2 + Byte( Pressed ) );
          gl_Vertex2f( X + W - 3 + Byte( Pressed ), Y + 3 + Byte( Pressed )     );
          gl_Vertex2f( X + W - 3 + Byte( Pressed ), Y + H - 3 + Byte( Pressed ) );
        glEnd();
      end;
    SCROLL_RIGHT:
      begin
        glBegin( GL_TRIANGLES );
          gl_Vertex2f( X + 3 + Byte( Pressed ),     Y + 3 + Byte( Pressed )     );
          gl_Vertex2f( X + W - 3 + Byte( Pressed ), Y + H / 2 + Byte( Pressed ) );
          gl_Vertex2f( X + 3 + Byte( Pressed ),     Y + H - 3 + Byte( Pressed ) );
        glEnd();
      end;
  end;
end;

procedure gui_DrawWidget;
  var
    i : Integer;
begin
  if ( not Assigned( Widget ) ) or ( not Widget.Visible ) Then exit;
  if Widget.Modal Then
    begin
      gui_AddEvent( EVENT_DRAW_MODAL, Widget, nil, nil );
      exit;
    end;

  if Assigned( Widget.OnDraw ) Then
    Widget.OnDraw( Widget );

  if Widget.parts > 0 Then
    begin
      i := 0;
      _clip( Widget, TRUE );
      while i < Widget.parts do
        begin
          gui_DrawWidget( Widget.part[ i ] );
          if Assigned( Widget.part[ i ] ) Then INC( i );
        end;
      scissor_End();
    end;

  if Widget.children > 0 Then
    begin
      i := 0;
      _clip( Widget );
      while i < Widget.children do
        begin
          gui_DrawWidget( Widget.child[ i ] );
          if Assigned( Widget.child[ i ] ) Then INC( i );
        end;
      scissor_End();
    end;
end;

procedure gui_DrawButton;
begin
  with zglTButtonDesc( Widget.Desc^ ), Widget.Rect do
    begin
      _button_draw( X, Y, W, H, Pressed );

      if Widget.Focus Then
        pr2d_Rect( X - 1, Y - 1, W + 2, H + 2, COLOR_SELECT, 155 );

      _clip( Widget, X + 2, Y + 2, W - 4, H - 4 );
      text_Draw( Font, Round( X + ( W - text_GetWidth( Font, Caption ) ) / 2 ) + Byte( Pressed ),
                       Round( Y + ( H - Font.MaxHeight ) / 2 ) + Byte( Pressed ), Caption );
      scissor_End();
    end;
end;

procedure gui_DrawCheckBox;
begin
  with zglTCheckBoxDesc( Widget.Desc^ ), Widget.Rect do
    begin
      pr2d_Rect( X, Y, W, H, COLOR_WIDGET, 255, PR2D_FILL );
      pr2d_Rect( X, Y, W, H, $000000, 255, 0 );
      if Widget.MouseIn Then
        pr2d_Rect( X + 1, Y + 1, W - 2, H - 2, COLOR_SELECT, 55, PR2D_FILL );
      if Checked Then
        pr2d_Rect( X + 3, Y + 3, W - 6, H - 6, $000000, 255, PR2D_FILL );
      if Widget.focus Then
        pr2d_Rect( X - 1, Y - 1, W + 2, H + 2, COLOR_SELECT, 155 );

      text_Draw( Font, X + W + Font.CharDesc[ Byte( ' ' ) ].ShiftP, Round( Y + ( H - Font.MaxHeight ) / 2 ), Caption );
    end;
end;

procedure gui_DrawRadioButton;
begin
  with zglTRadioButtonDesc( Widget.Desc^ ), Widget.Rect do
    begin
      pr2d_Circle( X + W / 2, Y + H / 2, W / 2, COLOR_WIDGET, 255, 8, PR2D_FILL );
      pr2d_Circle( X + W / 2, Y + H / 2, W / 2, $000000, 255, 8 );
      if Widget.MouseIn Then
        pr2d_Circle( X + W / 2, Y + H / 2, W / 2 - 1, COLOR_SELECT, 55, 8, PR2D_FILL );
      if Checked Then
        pr2d_Circle( X + W / 2, Y + H / 2, W / 3, $000000, 255, 8, PR2D_FILL );
      if Widget.Focus Then
        pr2d_Circle( X + W / 2, Y + H / 2, W / 2 + 1, COLOR_SELECT, 155, 8 );

      text_Draw( Font, X + W + Font.CharDesc[ Byte( ' ' ) ].ShiftP, Round( Y + ( H - Font.MaxHeight ) / 2 ), Caption );
    end;
end;

procedure gui_DrawLabel;
begin
  with zglTCheckBoxDesc( Widget.Desc^ ), Widget.Rect do
    text_Draw( Font, X, Y, Caption );
end;

procedure gui_DrawEditBox;
  var
    tw, th : Single;
begin
  with zglTEditBoxDesc( Widget.Desc^ ), Widget.Rect do
    begin
      pr2d_Rect( X, Y, W, H, COLOR_EDIT, 255, PR2D_FILL );
      pr2d_Rect( X, Y, W, H, COLOR_WIDGET, 255, 0 );
      pr2d_Rect( X + 1, Y + 1, W - 2, H - 2, $000000, 255, 0 );
      if Widget.MouseIn Then
        pr2d_Rect( X + 1, Y + 1, W - 2, H - 2, COLOR_SELECT, 55, PR2D_FILL );
      if Widget.Focus Then
        pr2d_Rect( X, Y, W, H, COLOR_SELECT, 155 );

      _clip( Widget, X + 2, Y + 2, W - 4, H - 2 );
      th := Y + Round( ( H - Font.MaxHeight ) / 2 ) + 1;
      text_Draw( Font, X + Font.CharDesc[ Byte( ' ' ) ].ShiftP, th, Text );

      if Widget.Focus Then
        begin
          tw := X + Font.CharDesc[ Byte( ' ' ) ].ShiftP + text_GetWidth( Font, Text );
          pr2d_Line( tw, th, tw, th + Font.MaxHeight, $FFFFFF, 255 * Byte( cursorAlpha < 25 ) );
        end;
      scissor_End();
    end;
end;

procedure gui_DrawListBox;
  var
    i, ty  : Integer;
    shiftY : Integer;
    iShift : Integer;
    subW   : Integer;
begin
  with zglTListBoxDesc( Widget.Desc^ ), Widget.Rect do
    begin
      subW := ( SCROLL_SIZE + 1 ) * Byte( Widget.part[ 0 ].Visible );
      pr2d_Rect( X, Y, W - subW, H, COLOR_EDIT, 255, PR2D_FILL );
      pr2d_Rect( X, Y, W - subW, H, COLOR_WIDGET, 255, 0 );
      pr2d_Rect( X + 1, Y + 1, W - 2 - subW, H - 2, $000000, 255, 0 );
      if Widget.Focus Then
        pr2d_Rect( X, Y, W - subW, H, COLOR_SELECT, 155 );

      _clip( Widget, X + 2, Y + 2, W - subW + 1 - 4, H - 4 );
      if Widget.part[ 0 ].Visible Then
        iShift := zglTScrollBarDesc( Widget.part[ 0 ].Desc^ ).Position
      else
        iShift := 0;
      shiftY := ( ItemHeight - Font.MaxHeight ) div 2 + 2;
      for i := 0 to List.Count - 1 do
        begin
          ty := Round( Y + i * ItemHeight - iShift + shiftY );
          if ( ty >= Y - ItemHeight ) and ( ty <= Y + H + ItemHeight ) Then
            text_Draw( Font, X + Font.CharDesc[ Byte( ' ' ) ].ShiftP, ty, List.Items[ i ] );
        end;

      if ItemIndex > -1 Then
        begin
          pr2d_Rect( X + 2, Y + 2 + ItemIndex * ItemHeight - iShift, W - 4 - subW, ItemHeight, COLOR_SELECT, 55, PR2D_FILL );
          pr2d_Rect( X + 2, Y + 2 + ItemIndex * ItemHeight - iShift, W - 4 - subW, ItemHeight, COLOR_SELECT, 155 );
        end;
      scissor_End();
    end;
end;

procedure gui_DrawComboBox;
  var
    i, tw  : Integer;
    shiftY : Integer;
    iShift : Integer;
    ty, th : Single;
    r      : zglTRect;
begin
  with zglTComboBoxDesc( Widget.Desc^ ), Widget^, Widget.Rect do
    begin
      pr2d_Rect( X, Y, W, H, COLOR_EDIT, 255, PR2D_FILL );
      pr2d_Rect( X, Y, W, H, COLOR_WIDGET, 255, 0 );
      pr2d_Rect( X + 1, Y + 1, W - 2, H - 2, $000000, 255, 0 );
      if ( not DropedDown ) and Widget.Focus Then
        pr2d_Rect( X, Y, W, H, COLOR_SELECT, 155 );

      _clip( Widget, X + 2, Y + 2, W - 4 - H, H - 2 );
      th := Y + Round( ( H - Font.MaxHeight ) / 2 ) + 1;
      if ItemIndex > -1 Then
        text_Draw( Font, X + Font.CharDesc[ Byte( ' ' ) ].ShiftP, th, List.Items[ ItemIndex ] );
      scissor_End;

      _scroll_draw( X + W - H + 2, Y + 2, H - 4, H - 4, SCROLL_DOWN, MouseIn and ( mouse_X > X + W - H + 2 ) and ( mouse_Y < Y + H ) and mouse_Down( M_BLEFT ) );

      if DropedDown Then
        begin
          if List.Count > DropDownCount Then
            th := DropDownCount * ItemHeight + 4
          else
            th := List.Count * ItemHeight + 4;
          pr2d_Rect( X, Y + H, W, th, COLOR_EDIT, 255, PR2D_FILL );
          pr2d_Rect( X, Y + H, W, th, COLOR_WIDGET, 255, 0 );
          pr2d_Rect( X + 1, Y + H, W - 2, th - 1, $000000, 255, 0 );

          ShiftY := ( ItemHeight - Font.MaxHeight ) div 2 + 2;
          if Widget.part[ 0 ].Visible Then
            begin
              iShift := zglTScrollBarDesc( Widget.part[ 0 ].Desc^ ).Position;
              tw     := Round( H - 4 );
            end else
              begin
                tw     := 0;
                iShift := 0;
              end;

          scissor_Begin( Round( X + 2 ), Round( Y + H ), Round( W - 4 - tw ), Round( th - 2 ) );
          for i := 0 to List.Count - 1 do
            begin
              ty := Round( Y + H + i * ItemHeight - iShift + shiftY );
              if ( ty >= Y - ItemHeight ) and ( ty <= Y + H + th ) Then
                text_Draw( Font, X + Font.CharDesc[ Byte( ' ' ) ].ShiftP, ty, List.Items[ i ] );

              r.X := X;
              r.Y := ty - 1;
              r.W := W - tw;
              r.H := ItemHeight + 1;
              if col2d_PointInRect( mouse_X, mouse_Y, r ) Then
                begin
                  pr2d_Rect( X + 2, ty - 2, W - 4 - tw, ItemHeight, COLOR_SELECT, 55, PR2D_FILL );
                  pr2d_Rect( X + 2, ty - 2, W - 4 - tw, ItemHeight, COLOR_SELECT, 155 );
                end;
             end;
          scissor_End();
        end;
    end;
end;

procedure gui_DrawGroupBox;
  var
    th : Integer;
begin
  with zglTGroupBoxDesc( Widget.Desc^ ), Widget.Rect do
    begin
      th := Trunc( Font.MaxHeight / 2 );
      pr2d_Rect( X, Y, W, H, COLOR_WINDOW, 255, PR2D_FILL );

      pr2d_Rect( X, Y, W - 1, H - 1, COLOR_WIDGET, 255, 0 );
      pr2d_Rect( X + 1, Y + 1, W - 1, H - 1, COLOR_LIGHT, 255, 0 );
      pr2d_Rect( X + Font.CharDesc[ Byte( ' ' ) ].ShiftP, Y, text_GetWidth( Font, Caption ), th, COLOR_WINDOW, 255, PR2D_FILL );

      text_Draw( Font, X + Font.CharDesc[ Byte( ' ' ) ].ShiftP, Y - th, Caption );
    end;
end;

procedure gui_DrawScrollBox;
begin
end;

procedure gui_DrawSpin;
begin
  with zglTSpinDesc( Widget.Desc^ ), Widget.Rect do
    begin
      _scroll_draw( X, Y,         W, H / 2, SCROLL_UP,   uPressed );
      _scroll_draw( X, Y + H / 2, W, H / 2, SCROLL_DOWN, dPressed );
    end;
end;

procedure gui_DrawScrollBar;
  var
    r : zglTRect;
begin
  with zglTScrollBarDesc( Widget.Desc^ ), Widget.Rect do
    begin
      pr2d_Rect( X, Y, W, H, COLOR_WIDGET, 255, PR2D_FILL );
      pr2d_Rect( X, Y, W, H, $000000, 255 );

      r := gui_GetScrollRect( Widget );
      _button_draw( r.X, r.Y, r.W, r.H, FALSE );

      if Kind = SCROLLBAR_VERTICAL Then
        begin
          _scroll_draw( X, Y,                   SCROLL_SIZE, SCROLL_SIZE, SCROLL_UP,   uPressed );
          _scroll_draw( X, Y + H - SCROLL_SIZE, SCROLL_SIZE, SCROLL_SIZE, SCROLL_DOWN, dPressed );
        end else
          begin
            _scroll_draw( X, Y,                   SCROLL_SIZE, SCROLL_SIZE, SCROLL_LEFT,  uPressed );
            _scroll_draw( X + W - SCROLL_SIZE, Y, SCROLL_SIZE, SCROLL_SIZE, SCROLL_RIGHT, dPressed );
          end;
    end;
end;

end.
