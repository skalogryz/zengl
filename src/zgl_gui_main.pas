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
unit zgl_gui_main;

{$I zgl_config.cfg}

interface
uses
  zgl_types,
  zgl_gui_types;

procedure gui_Init;
procedure gui_Draw;
procedure gui_Proc;

procedure gui_AddEvent( const _type : Integer; const Widget, Sender : zglPWidget; const EventData : Pointer );
procedure gui_DelEvent( var Event : zglPEvent );

function  gui_AddWidget( const _type : Integer; const X, Y, W, H : Single; const Focus, Visible : Boolean; const Desc, Data : Pointer; const Parent : zglPWidget; const Part : Boolean = FALSE ) : zglPWidget;
procedure gui_DelWidget( var Widget : zglPWidget );

var
  eventList   : zglTEventList;
  managerGUI  : zglTGUIManager;
  widgetTLast : DWORD;

  cursorAlpha : Integer;

implementation
uses
  zgl_main,
  zgl_opengl_all,
  zgl_window,
  zgl_math_2d,
  zgl_gui_process,
  zgl_gui_render,
  zgl_gui_utils;

procedure gui_Init;
begin
  managerGUI.Main.visible := TRUE;
  managerGUI.Main.parent  := @managerGUI.Main;
  managerGUI.Main.OnProc  := @gui_ProcEvents;

  // Button
  zgl_Reg( WIDGET_TYPE_ID,   Pointer( WIDGET_BUTTON ) );
  zgl_Reg( WIDGET_FILL_DESC, @gui_FillButtonDesc );
  zgl_Reg( WIDGET_ONDRAW,    @gui_DrawButton );
  zgl_Reg( WIDGET_ONPROC,    @gui_ProcButton );

  // CheckBox
  zgl_Reg( WIDGET_TYPE_ID,   Pointer( WIDGET_CHECKBOX ) );
  zgl_Reg( WIDGET_FILL_DESC, @gui_FillCheckBoxDesc );
  zgl_Reg( WIDGET_ONDRAW,    @gui_DrawCheckBox );
  zgl_Reg( WIDGET_ONPROC,    @gui_ProcCheckBox );

  // RadioButton
  zgl_Reg( WIDGET_TYPE_ID,   Pointer( WIDGET_RADIOBUTTON ) );
  zgl_Reg( WIDGET_FILL_DESC, @gui_FillRadioButtonDesc );
  zgl_Reg( WIDGET_ONDRAW,    @gui_DrawRadioButton );
  zgl_Reg( WIDGET_ONPROC,    @gui_ProcRadioButton );

  // Label
  zgl_Reg( WIDGET_TYPE_ID,   Pointer( WIDGET_LABEL ) );
  zgl_Reg( WIDGET_FILL_DESC, @gui_FillLabelDesc );
  zgl_Reg( WIDGET_ONDRAW,    @gui_DrawLabel );
  zgl_Reg( WIDGET_ONPROC,    @gui_ProcLabel );

  // EditBox
  zgl_Reg( WIDGET_TYPE_ID,   Pointer( WIDGET_EDITBOX ) );
  zgl_Reg( WIDGET_FILL_DESC, @gui_FillEditBoxDesc );
  zgl_Reg( WIDGET_ONDRAW,    @gui_DrawEditBox );
  zgl_Reg( WIDGET_ONPROC,    @gui_ProcEditBox );

  // ListBox
  zgl_Reg( WIDGET_TYPE_ID,   Pointer( WIDGET_LISTBOX ) );
  zgl_Reg( WIDGET_FILL_DESC, @gui_FillListBoxDesc );
  zgl_Reg( WIDGET_ONDRAW,    @gui_DrawListBox );
  zgl_Reg( WIDGET_ONPROC,    @gui_ProcListBox );

  // ComboBox
  zgl_Reg( WIDGET_TYPE_ID,   Pointer( WIDGET_COMBOBOX ) );
  zgl_Reg( WIDGET_FILL_DESC, @gui_FillComboBoxDesc );
  zgl_Reg( WIDGET_ONDRAW,    @gui_DrawComboBox );
  zgl_Reg( WIDGET_ONPROC,    @gui_ProcComboBox );

  // GroupBox
  zgl_Reg( WIDGET_TYPE_ID,   Pointer( WIDGET_GROUPBOX ) );
  zgl_Reg( WIDGET_FILL_DESC, @gui_FillGroupBoxDesc );
  zgl_Reg( WIDGET_ONDRAW,    @gui_DrawGroupBox );
  zgl_Reg( WIDGET_ONPROC,    @gui_ProcGroupBox );

  // ScrollBox
  zgl_Reg( WIDGET_TYPE_ID,   Pointer( WIDGET_SCROLLBOX ) );
  zgl_Reg( WIDGET_FILL_DESC, @gui_FillScrollBoxDesc );
  zgl_Reg( WIDGET_ONDRAW,    @gui_DrawScrollBox );
  zgl_Reg( WIDGET_ONPROC,    @gui_ProcScrollBox );

  // Spin
  zgl_Reg( WIDGET_TYPE_ID,   Pointer( WIDGET_SPIN ) );
  zgl_Reg( WIDGET_FILL_DESC, @gui_FillSpinDesc );
  zgl_Reg( WIDGET_ONDRAW,    @gui_DrawSpin );
  zgl_Reg( WIDGET_ONPROC,    @gui_ProcSpin );

  // ScrollBar
  zgl_Reg( WIDGET_TYPE_ID,   Pointer( WIDGET_SCROLLBAR ) );
  zgl_Reg( WIDGET_FILL_DESC, @gui_FillScrollBarDesc );
  zgl_Reg( WIDGET_ONDRAW,    @gui_DrawScrollBar );
  zgl_Reg( WIDGET_ONPROC,    @gui_ProcScrollBar );
end;

procedure gui_Draw;
  var
    i     : Integer;
    Event : zglPEvent;
    ToDel : zglPEvent;
begin
  i := 0;
  while i < managerGUI.Main.children do
    begin
      gui_DrawWidget( managerGUI.Main.child[ i ] );
      if Assigned( managerGUI.Main.child[ i ] ) Then
        INC( i );
    end;

  ToDel := nil;
  Event := eventList.First.Next;
  while Event <> nil do
    begin
      if Event._type = EVENT_DRAW_MODAL Then
        begin
          Event.Widget.modal := FALSE;
          gui_DrawWidget( Event.Widget );
          Event.Widget.modal := TRUE;
          ToDel := Event;
        end;
      Event := Event.Next;
      if Assigned( ToDel ) Then
        gui_DelEvent( ToDel );
    end;
end;

procedure gui_Proc;
  var
    Event : zglPEvent;
    p     : Pointer;
begin
  INC( cursorAlpha );
  if cursorAlpha > 50 Then
    cursorAlpha := 0;

  managerGUI.Main.rect.W := wnd_Width;
  managerGUI.Main.rect.H := wnd_Height;
  managerGUI.Main.client := managerGUI.Main.rect;

  gui_ProcWidget( @managerGUI.Main );

  Event := eventList.First.Next;
  while Event <> nil do
    begin
      if Assigned( Event.Widget.OnProc ) Then
        Event.Widget.OnProc( Event );
      Event := Event.Next;
    end;
  while eventList.Count > 0 do
    begin
      p := eventList.First.Next;
      gui_DelEvent( zglPEvent( p ) );
    end;
end;

procedure gui_AddEvent;
  var
    newEvent : zglPEvent;
begin
  newEvent := @eventList.First;
  while Assigned( newEvent.Next ) do
    newEvent := newEvent.Next;

  zgl_GetMem( Pointer( newEvent.Next ), SizeOf( zglTEvent ) );
  case _type of
    EVENT_DRAG_MOVE: Move( EventData^, newEvent.Next.drag_pos, SizeOf( zglTPoint2D ) );
    EVENT_MOUSE_MOVE: Move( EventData^, newEvent.Next.mouse_pos, SizeOf( zglTPoint2D ) );
    EVENT_MOUSE_DOWN,
    EVENT_MOUSE_UP,
    EVENT_MOUSE_CLICK: Move( EventData^, newEvent.Next.mouse_button, 1 );
    EVENT_MOUSE_WHEEL: Move( EventData^, newEvent.Next.mouse_wheel, 1 );

    EVENT_KEY_DOWN,
    EVENT_KEY_UP    : Move( EventData^, newEvent.Next.key_code, 1 );
  end;
  newEvent.Next._type  := _type;
  newEvent.Next.Widget := Widget;
  newEvent.Next.Sender := Sender;
  newEvent.Next.Prev   := newEvent;
  newEvent             := newEvent.Next;
  INC( eventList.Count );
end;

procedure gui_DelEvent;
begin
  if Assigned( Event.Prev ) Then
    Event.Prev.Next := Event.Next;
  if Assigned( Event.Next ) Then
    Event.Next.Prev := Event.Prev;
  FreeMem( Event );
  DEC( eventList.Count );

  Event := nil;
end;

function gui_AddWidget;
  var
    i : Integer;
    p : zglPWidget;
    e : zglTEvent;
begin
  if not Assigned( Parent ) Then
    p := @managerGUI.Main
  else
    p := Parent;

  if not Part Then
    begin
      INC( p.children );
      SetLength( p.child, p.children );
      zgl_GetMem( Pointer( p.child[ p.children - 1 ] ), SizeOf( zglTWidget ) );
      Result     := p.child[ p.children - 1 ];
      Result._id := p.children - 1;
    end else
      begin
        INC( p.parts );
        SetLength( p.part, p.parts );
        zgl_GetMem( Pointer( p.part[ p.parts - 1 ] ), SizeOf( zglTWidget ) );
        Result     := p.part[ p.parts - 1 ];
        Result._id := p.parts - 1;
      end;

  Result._type  := _type;
  managerGUI.Types[ _type - 1 ].FillDesc( Desc, Result.desc );
  Result.data   := Data;
  Result.parent := p;
  Result.rect.X := p.rect.X + X;
  Result.rect.Y := p.rect.Y + Y;
  Result.rect.W := W;
  Result.rect.H := H;
  if Focus Then
    begin
      gui_ProcCallback( nil, nil, gui_ResetFocus, nil );
      gui_AddEvent( EVENT_FOCUS_IN, Result, nil, nil );
    end;
  Result.focus   := Focus;
  Result.visible := Visible;
  for i := High( managerGUI.Types ) downto 0 do
    if Result._type = managerGUI.Types[ i ]._type Then
      begin
        Result.OnDraw := managerGUI.Types[ i ].OnDraw;
        Result.OnProc := managerGUI.Types[ i ].OnProc;
      end;

  gui_AlignWidget( Result );
  gui_UpdateClient( Result );

  e._type  := EVENT_CREATE;
  e.sender := nil;
  e.widget := Result;
  if Assigned( Result.OnProc ) Then
    Result.OnProc( @e );
end;

procedure gui_DelWidget;
  var
    i : Integer;
begin
  // Children
  while Widget.children > 0 do
    gui_DelWidget( Widget.child[ 0 ] );

  if Widget.parent.children > 0 Then
    begin
      for i := Widget._id to Widget.parent.children - 2 do
        Widget.parent.child[ i ] := Widget.parent.child[ i + 1 ];
      DEC( Widget.parent.children );
      SetLength( Widget.parent.child, Widget.parent.children );
    end;

  // Parts
  while Widget.parts > 0 do
    gui_DelWidget( Widget.part[ 0 ] );

  if Widget.parent.parts > 0 Then
    begin
      for i := Widget._id to Widget.parent.parts - 2 do
        Widget.parent.part[ i ] := Widget.parent.part[ i + 1 ];
      DEC( Widget.parent.parts );
      SetLength( Widget.parent.part, Widget.parent.parts );
    end;

  FreeMem( Widget.desc );
  FreeMem( Widget );
  Widget := nil;
end;

end.
