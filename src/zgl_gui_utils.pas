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
unit zgl_gui_utils;

interface

uses
  zgl_gui_types;

procedure _clip( const widget : zglPWidget ); overload;
procedure _clip( const widget : zglPWidget; const X, Y, W, H : Single ); overload;

type zglProcWidgetsCallback = procedure( const widget : zglPWidget; const data : Pointer );

procedure gui_ProcCallback( const callback : zglProcWidgetsCallback; const data : Pointer );
procedure gui_ResetFocus( const widget : zglPWidget; const data : Pointer );
procedure gui_ResetChecked( const widget : zglPWidget; const data : Pointer );

procedure gui_ScrollListBox( const Widget : zglPWidget; const Value, Change : Integer );

implementation
uses
  zgl_gui_main,
  zgl_opengl_simple,
  zgl_math_2d,
  zgl_collision_2d;

procedure _clip( const widget : zglPWidget ); overload;
  var
    clip : zglTRect;
begin
  clip := col2d_ClipRect( widget.rect, widget.parent.rect );
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
  clip := col2d_ClipRect( clip, widget.parent.rect );
  scissor_Begin( Round( clip.X ), Round( clip.Y ), Round( clip.W ), Round( clip.H ) );
end;

procedure gui_ProcCallback;
  var
    w, wc : zglPWidget;
begin
  w := managerGUI.First.Next;
  while w <> nil do
    begin
      if Assigned( w.child ) Then
        begin
          wc := w.child;
          repeat
            wc := wc.Next;
            callback( wc, data );
          until not Assigned( wc.Next );
        end;
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

procedure gui_ScrollListBox;
  var
    ch     : Integer;
    iShift : Integer;
    iCount : Integer;
begin
  iCount := Round( ( Widget.parent.rect.H - 3 ) / ( zglTListBoxDesc( Widget.parent.desc^ ).Font.MaxHeight + 3 ) );
  iShift := zglTScrollBarDesc( Widget.desc^ ).Position;

  with zglTListBoxDesc( Widget.parent.desc^ ) do
    begin
      ch := 0;
      if ItemIndex < iShift Then
        ch := 1;
      if ItemIndex > iShift + iCount - 1 Then
        ch := -1;
      if ch <> 0 Then
        begin
          ItemIndex := ItemIndex + ch;
          if Assigned( Widget.parent.Events.OnChange )  Then
            Widget.parent.Events.OnChange( Widget.parent, ItemIndex, ch );
        end;
    end;
end;

end.
