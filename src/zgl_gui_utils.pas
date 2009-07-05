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

procedure _clip( const widget : zglPWidget ); overload;
procedure _clip( const widget : zglPWidget; const X, Y, W, H : Single ); overload;

procedure gui_AlignWidget( widget : zglPWidget );
procedure gui_UpdateClient( widget : zglPWidget );

type zglProcWidgetsCallback = procedure( widget : zglPWidget; const data : Pointer );

procedure gui_ProcCallback( widget : zglPWidget; callback : zglProcWidgetsCallback; const data : Pointer );
procedure gui_ResetFocus( widget : zglPWidget; const data : Pointer );
procedure gui_ResetChecked( widget : zglPWidget; const data : Pointer );

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

procedure _clip( const widget : zglPWidget ); overload;
  var
    clip : zglTRect;
begin
  clip := col2d_ClipRect( widget.rect, widget.parent.client );
  scissor_Begin( Round( clip.X ), Round( clip.Y ), Round( clip.W ), Round( clip.H ) );
end;

procedure _clip( const widget : zglPWidget; const X, Y, W, H : Single ); overload;
  var
    clip : zglTRect;
begin
  clip.X := X;
  clip.Y := Y;
  clip.W := W;
  clip.H := H;
  clip := col2d_ClipRect( clip, widget.parent.client );
  scissor_Begin( Round( clip.X ), Round( clip.Y ), Round( clip.W ), Round( clip.H ) );
end;

procedure gui_AlignWidget;
begin
  with widget^ do
    case align of
      ALIGN_NONE:;
      ALIGN_CLIENT: rect := parent.client;
      ALIGN_LEFT:
        begin
          rect.X := parent.client.X;
          rect.Y := parent.client.Y;
          rect.H := parent.client.H;
        end;
      ALIGN_RIGHT:
        begin
          rect.X := parent.client.X + parent.client.W - rect.W;
          rect.Y := parent.client.Y;
          rect.H := parent.client.H;
        end;
      ALIGN_TOP:
        begin
          rect.X := parent.client.X;
          rect.Y := parent.client.Y;
          rect.W := parent.client.W;
        end;
      ALIGN_BOTTOM:
        begin
          rect.X := parent.client.X;
          rect.Y := parent.client.Y + parent.client.H - rect.H;
          rect.W := parent.client.W;
        end;
    end;
end;

procedure gui_UpdateClient;
begin
  widget.client.X := widget.rect.X + 2;
  widget.client.Y := widget.rect.Y + 2;
  widget.client.W := widget.rect.W - 4;
  widget.client.H := widget.rect.H - 4;
end;

procedure gui_ProcCallback;
  var
    w : zglPWidget;
begin
  if not Assigned( widget ) Then
    w := managerGUI.First.Next
  else
    w := widget;
  while w <> nil do
    begin
      if Assigned( w.child ) Then
        gui_ProcCallback( w.child, callback, data );
      callback( w, data );
      w := w.Next;
    end;
end;

procedure gui_ResetFocus;
begin
  if widget.Focus Then
    gui_AddEvent( EVENT_FOCUS_OUT, widget, nil );
  widget.Focus := FALSE;
end;

procedure gui_ResetChecked;
begin
  if widget._type = WIDGET_RADIOBUTTON Then
    if zglTRadioButtonDesc( widget.desc^ ).Group = PInteger( data )^ Then
      zglTRadioButtonDesc( widget.desc^ ).Checked := FALSE;
end;

function gui_GetScrollRect;
begin
  with Widget.rect, zglTScrollBarDesc( Widget.desc^ ) do
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
  Result := Round( ( Widget.rect.H - 3 * 2 ) / ( zglTListBoxDesc( Widget.desc^ ).Font.MaxHeight + 3 ) );
end;

procedure gui_ScrollChange;
  var
    ch : Integer;
begin
  with zglTScrollBarDesc( Widget.desc^ ) do
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
        P := Trunc( ( Max / ( Widget.rect.H - SCROLL_SIZE * 2 - r.H ) ) * ( Y - Widget.rect.Y - SCROLL_SIZE ) );
        if P - Position <> 0 Then
          gui_ScrollChange( Widget, P - Position );
      end else
        begin
          P := Round( ( Max / ( Widget.rect.W - SCROLL_SIZE * 2 - r.W ) ) * ( X - Widget.rect.X - SCROLL_SIZE ) );
          if P - Position <> 0 Then
            gui_ScrollChange( Widget, P - Position );
        end;
end;

end.
