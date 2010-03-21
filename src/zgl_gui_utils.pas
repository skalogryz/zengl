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
unit zgl_gui_utils;

interface

uses
  zgl_gui_types,
  zgl_math_2d;

procedure _clip( const Widget : zglPWidget; const FullSize : Boolean = FALSE ); overload;
procedure _clip( const Widget : zglPWidget; const X, Y, W, H : Single ); overload;

procedure gui_AlignWidget( Widget : zglPWidget );
procedure gui_UpdateClient( Widget : zglPWidget );

type zglProcWidgetsCallback = procedure( Widget, Sender : zglPWidget; const Data : Pointer );

procedure gui_ProcCallback( Widget, Sender : zglPWidget; Callback : zglProcWidgetsCallback; const Data : Pointer );
procedure gui_ResetFocus( Widget, Sender : zglPWidget; const Data : Pointer );
procedure gui_ResetChecked( Widget, Sender : zglPWidget; const Data : Pointer );

function gui_GetListItemsPerPage( Widget : zglPWidget ) : Integer;
function gui_GetScrollRect( Widget : zglPWidget ) : zglTRect;

procedure gui_ScrollChange( Widget : zglPWidget; const Change : Integer );
procedure gui_ScrollXY2Pos( Widget : zglPWidget; const X, Y : Integer );

implementation
uses
  zgl_gui_main,
  zgl_gui_render,
  zgl_opengl_simple,
  zgl_mouse,
  zgl_collision_2d;

procedure _clip( const Widget : zglPWidget; const FullSize : Boolean = FALSE ); overload;
  var
    clip : zglTRect;
begin
  if FullSize Then
    clip := col2d_ClipRect( Widget.RectEx, Widget.parent.Client )
  else
    clip := col2d_ClipRect( Widget.Rect, Widget.parent.Client );
  scissor_Begin( Round( clip.X ), Round( clip.Y ), Round( clip.W ), Round( clip.H ) );
end;

procedure _clip( const Widget : zglPWidget; const X, Y, W, H : Single ); overload;
  var
    clip : zglTRect;
begin
  clip.X := X;
  clip.Y := Y;
  clip.W := W;
  clip.H := H;
  clip := col2d_ClipRect( clip, Widget.parent.Client );
  scissor_Begin( Round( clip.X ), Round( clip.Y ), Round( clip.W ), Round( clip.H ) );
end;

procedure gui_AlignWidget;
begin
  with Widget^ do
    case align of
      ALIGN_NONE:;
      ALIGN_CLIENT: Rect := parent.Client;
      ALIGN_LEFT:
        begin
          Rect.X := parent.Client.X;
          Rect.Y := parent.Client.Y;
          Rect.H := parent.Client.H;
        end;
      ALIGN_RIGHT:
        begin
          Rect.X := parent.Client.X + parent.Client.W - Rect.W;
          Rect.Y := parent.Client.Y;
          Rect.H := parent.Client.H;
        end;
      ALIGN_TOP:
        begin
          Rect.X := parent.Client.X;
          Rect.Y := parent.Client.Y;
          Rect.W := parent.Client.W;
        end;
      ALIGN_BOTTOM:
        begin
          Rect.X := parent.Client.X;
          Rect.Y := parent.Client.Y + parent.Client.H - Rect.H;
          Rect.W := parent.Client.W;
        end;
    end;
end;

procedure gui_UpdateClient;
begin
  Widget.Client.X := Widget.Rect.X + 2;
  Widget.Client.Y := Widget.Rect.Y + 2;
  Widget.Client.W := Widget.Rect.W - 4;
  Widget.Client.H := Widget.Rect.H - 4;
end;

procedure gui_ProcCallback;
  var
    i : Integer;
    w : zglPWidget;
begin
  if Assigned( Widget ) Then
    w := Widget
  else
    w := @managerGUI.Main;

  i := 0;
  while i < w.parts do
    begin
      gui_ProcCallback( w.part[ i ], Sender, Callback, Data );
      if Assigned( w.part[ i ] ) Then INC( i );
    end;

  i := 0;
  while i < w.children do
    begin
      gui_ProcCallback( w.child[ i ], Sender, Callback, data );
      if Assigned( w.child[ i ] ) Then INC( i );
    end;

  Callback( w, Sender, Data );
end;

procedure gui_ResetFocus;
begin
  if Widget = Data Then exit;
  if Widget.Focus Then
    begin
      gui_AddEvent( EVENT_FOCUS_OUT, Widget, Sender, nil );
      Widget.Focus := FALSE;
    end;
end;

procedure gui_ResetChecked;
begin
  if Widget._type = WIDGET_RADIOBUTTON Then
    if zglTRadioButtonDesc( Widget.Desc^ ).Group = PInteger( Data )^ Then
      zglTRadioButtonDesc( Widget.Desc^ ).Checked := FALSE;
end;

function gui_GetScrollRect;
begin
  with Widget.Rect, zglTScrollBarDesc( Widget.Desc^ ) do
    if Kind = SCROLLBAR_VERTICAL Then
      begin
        Result.W := SCROLL_SIZE;
        if PageSize > 0 Then
          Result.H := Round( ( H - SCROLL_SIZE * 2 ) / ( ( Max + PageSize ) / PageSize ) )
        else
          Result.H := 0;
        if Result.H < SCROLL_SIZE / 2 Then
          Result.H := Round( SCROLL_SIZE / 2 );

        Result.X := X;
        Result.Y := Round( Y + SCROLL_SIZE + ( ( H - SCROLL_SIZE * 2 - Result.H ) - ( H - SCROLL_SIZE * 2 - Result.H ) * ( ( Max - Position ) / Max ) ) );
      end else
        begin
          Result.H := SCROLL_SIZE;
          if PageSize > 0 Then
            Result.W := Round( ( W - SCROLL_SIZE * 2 ) / ( ( Max + PageSize ) / PageSize ) )
          else
            Result.W := 0;
          if Result.W < SCROLL_SIZE / 2 Then
            Result.W := Round( SCROLL_SIZE / 2 );

          Result.X := Round( X + SCROLL_SIZE + ( ( W - SCROLL_SIZE * 2 - Result.W ) - ( W - SCROLL_SIZE * 2 - Result.W ) * ( ( Max - Position ) / Max ) ) );
          Result.Y := Y;
        end;
end;

function gui_GetListItemsPerPage;
begin
  case Widget._type of
    WIDGET_LISTBOX: Result := Round( ( Widget.Rect.H - 3 * 2 ) / ( zglTListBoxDesc( Widget.Desc^ ).Font.MaxHeight + 3 ) );
    WIDGET_COMBOBOX: Result := Round( ( Widget.RectEx.H - 3 * 2 ) / ( zglTComboBoxDesc( Widget.Desc^ ).Font.MaxHeight + 3 ) );
  end;
end;

procedure gui_ScrollChange;
  var
    ch : Integer;
begin
  with zglTScrollBarDesc( Widget.Desc^ ) do
    begin
      ch := Change;
      if Position + ch < 0 Then
        ch := 0 - Position;
      if Position + ch > Max Then
        ch := Max - Position;
      Position := Position + ch;
      if Assigned( Widget.Events.OnChange ) Then
        Widget.Events.OnChange( Widget, Position, ch );
    end;
end;

procedure gui_ScrollXY2Pos;
  var
    r : zglTRect;
    P : Integer;
begin
  r := gui_GetScrollRect( Widget );
  with zglTScrollBarDesc( Widget.desc^ ) do
    if Kind = SCROLLBAR_VERTICAL Then
      begin
        P := Trunc( ( Max / ( Widget.Rect.H - SCROLL_SIZE * 2 - r.H ) ) * ( Y - Widget.Rect.Y - SCROLL_SIZE ) );
        if P - Position <> 0 Then
          gui_ScrollChange( Widget, P - Position );
      end else
        begin
          P := Round( ( Max / ( Widget.Rect.W - SCROLL_SIZE * 2 - r.W ) ) * ( X - Widget.Rect.X - SCROLL_SIZE ) );
          if P - Position <> 0 Then
            gui_ScrollChange( Widget, P - Position );
        end;
end;

end.
