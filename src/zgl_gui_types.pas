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
unit zgl_gui_types;

{$I zgl_config.cfg}

interface
uses
  zgl_types,
  zgl_font,
  zgl_math_2d;

const
  // Widgets
  WIDGET_UNKNOWN     = 0;
  WIDGET_BUTTON      = 1;
  WIDGET_CHECKBOX    = 2;
  WIDGET_RADIOBUTTON = 3;
  WIDGET_LABEL       = 4;
  WIDGET_EDITBOX     = 5;
  WIDGET_LISTBOX     = 6;
  WIDGET_COMBOBOX    = 7;
  WIDGET_GROUPBOX    = 8;
  WIDGET_SCROLLBOX   = 9;
  WIDGET_SPIN        = 10;
  WIDGET_SCROLLBAR   = 11;

  // ScrollBar
  SCROLLBAR_VERTICAL   = 0;
  SCROLLBAR_HORIZONTAL = 1;

  // Select Mode
  SELECT_BY_CLICK = 0;
  SELECT_BY_DOWN  = 1;

  // Align
  ALIGN_NONE    = 0;
  ALIGN_CLIENT  = 1;
  ALIGN_LEFT    = 2;
  ALIGN_RIGHT   = 3;
  ALIGN_TOP     = 4;
  ALIGN_BOTTOM  = 5;

  // Events
  EVENT_CREATE      = 1;

  EVENT_FOCUS_IN    = 2;
  EVENT_FOCUS_OUT   = 3;

  EVENT_DRAG_START  = 4;
  EVENT_DRAG_MOVE   = 5;
  EVENT_DRAG_END    = 6;

  EVENT_MOUSE_MOVE  = 7;
  EVENT_MOUSE_ENTER = 8;
  EVENT_MOUSE_LEAVE = 9;
  EVENT_MOUSE_DOWN  = 10;
  EVENT_MOUSE_UP    = 11;
  EVENT_MOUSE_CLICK = 12;
  EVENT_MOUSE_WHEEL = 13;

  EVENT_KEY_DOWN    = 14;
  EVENT_KEY_UP      = 15;
  EVENT_KEY_CHAR    = 16;

  EVENT_DRAW_MODAL  = 17;

type
  zglPEvent  = ^zglTEvent;
  zglPWidget = ^zglTWidget;

  //Events
  zglTEvents = record
    OnCreate     : procedure( Widget : zglPWidget );
    OnFocus      : procedure( Widget : zglPWidget; const Focus : Boolean );
    OnStartDrag  : procedure( Widget : zglPWidget );
    OnDrag       : procedure( Widget : zglPWidget; const X, Y : Single );
    OnEndDrag    : procedure( Widget : zglPWidget; const X, Y : Single );
    OnClick      : procedure( Widget : zglPWidget );
    OnMouseUp    : procedure( Widget : zglPWidget );
    OnMouseMove  : procedure( Widget : zglPWidget; const X, Y : Single );
    OnMouseEnter : procedure( Widget : zglPWidget );
    OnMouseLeave : procedure( Widget : zglPWidget );
    OnMouseWheel : procedure( Widget : zglPWidget; const Axis : Byte );
    OnKeyDown    : procedure( Widget : zglPWidget; const KeyCode : Byte );
    OnKeyUp      : procedure( Widget : zglPWidget; const KeyCode : Byte );
    OnChange     : procedure( Widget : zglPWidget; const Value, Change : Integer );
end;

  //Widget
  zglTWidget = record
    _id      : Integer;
    _type    : Integer;
    desc     : Pointer;
    data     : Pointer;
    rect     : zglTRect;
    rectEx   : zglTRect;
    client   : zglTRect;
    align    : DWORD;
    layer    : Integer;
    focus    : Boolean;
    modal    : Boolean;
    visible  : Boolean;
    mousein  : Boolean;
    draged   : Boolean;

    OnDraw   : procedure( const Widget : zglPWidget );
    OnProc   : procedure( const Event  : zglPEvent );
    Events   : zglTEvents;

    parent   : zglPWidget;
    children : Integer;
    child    : array of zglPWidget;
    parts    : Integer;
    part     : array of zglPWidget;
end;

  zglTWidgetType = record
    _type    : Integer;
    FillDesc : procedure( Src : Pointer; var Desc : Pointer );

    OnDraw : procedure( const Widget : zglPWidget );
    OnProc : procedure( const Event  : zglPEvent );
end;

  //GUI Manager
  zglPGUIManager = ^zglTGUIManager;
  zglTGUIManager = record
    Main  : zglTWidget;
    Types : array of zglTWidgetType;
end;

  //Event
  zglTEvent = record
    _type      : Integer;
    sender     : zglPWidget;
    widget     : zglPWidget;
    Next, Prev : zglPEvent;
    case byte of
      0: ( drag_pos     : zglTPoint2D );
      1: ( mouse_pos    : zglTPoint2D );
      2: ( mouse_button : Byte );
      3: ( mouse_wheel  : Byte );
      4: ( key_code     : Byte );
      5: ( key_char     : PChar );
end;

  //Event list
  zglTEventList = record
    Count : DWORD;
    First : zglTEvent;
end;

  zglPButtonDesc = ^zglTButtonDesc;
  zglTButtonDesc = record
    Font    : zglPFont;
    Caption : String;

    Pressed : Boolean;
end;

  zglPCheckBoxDesc = ^zglTCheckBoxDesc;
  zglTCheckBoxDesc = record
    Font    : zglPFont;
    Caption : String;

    Checked : Boolean;
end;

  zglPRadioButtonDesc = ^zglTRadioButtonDesc;
  zglTRadioButtonDesc = record
    Font    : zglPFont;
    Caption : String;

    Checked : Boolean;
    Group   : Integer;
end;

  zglPLabelDesc = ^zglTLabelDesc;
  zglTLabelDesc = record
    Font    : zglPFont;
    Caption : String;
end;

  zglPEditBoxDesc = ^zglTEditBoxDesc;
  zglTEditBoxDesc = record
    Font : zglPFont;
    Text : String;

    Max      : Integer;
    ReadOnly : Boolean;
end;

  zglPListBoxDesc = ^zglTListBoxDesc;
  zglTListBoxDesc = record
    Font       : zglPFont;
    List       : zglTStringList;
    ItemIndex  : Integer;
    ItemHeight : Integer;
    SelectMode : Integer;
end;

  zglPComboBoxDesc = ^zglTComboBoxDesc;
  zglTComboBoxDesc = record
    Font          : zglPFont;
    List          : zglTStringList;
    ItemIndex     : Integer;
    ItemHeight    : Integer;
    DropDownCount : Integer;

    DropedDown    : Boolean;
end;

  zglPGroupBoxDesc = ^zglTGroupBoxDesc;
  zglTGroupBoxDesc = record
    Font    : zglPFont;
    Caption : String;
end;

  zglPScrollBoxDesc = ^zglTScrollBoxDesc;
  zglTScrollBoxDesc = record
end;

  zglPSpinDesc = ^zglTSpinDesc;
  zglTSpinDesc = record
    Value    : Integer;
    Max      : Integer;
    Min      : Integer;

    UPressed : Boolean;
    DPressed : Boolean;
end;

  zglPScrollBarDesc = ^zglTScrollBarDesc;
  zglTScrollBarDesc = record
    Kind     : Byte;
    Step     : Integer;
    Position : Integer;
    PageSize : Integer;
    Max      : Integer;

    UPressed : Boolean;
    DPressed : Boolean;
    SDraged  : Boolean;
end;

procedure gui_FillButtonDesc     ( Src : Pointer; var Desc : Pointer );
procedure gui_FillCheckBoxDesc   ( Src : Pointer; var Desc : Pointer );
procedure gui_FillRadioButtonDesc( Src : Pointer; var Desc : Pointer );
procedure gui_FillLabelDesc      ( Src : Pointer; var Desc : Pointer );
procedure gui_FillEditBoxDesc    ( Src : Pointer; var Desc : Pointer );
procedure gui_FillListBoxDesc    ( Src : Pointer; var Desc : Pointer );
procedure gui_FillComboBoxDesc   ( Src : Pointer; var Desc : Pointer );
procedure gui_FillGroupBoxDesc   ( Src : Pointer; var Desc : Pointer );
procedure gui_FillScrollBoxDesc  ( Src : Pointer; var Desc : Pointer );
procedure gui_FillSpinDesc       ( Src : Pointer; var Desc : Pointer );
procedure gui_FillScrollBarDesc  ( Src : Pointer; var Desc : Pointer );

implementation
uses
  zgl_main;

procedure gui_FillButtonDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTButtonDesc ) );

  if Assigned( Src ) Then
  with zglTButtonDesc( Desc^ ) do
    begin
      Font    := zglTButtonDesc( Src^ ).Font;
      Caption := zglTButtonDesc( Src^ ).Caption;
      Pressed := zglTButtonDesc( Src^ ).Pressed;
    end;
end;

procedure gui_FillCheckBoxDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTCheckBoxDesc ) );

  if Assigned( Src ) Then
  with zglTCheckBoxDesc( Desc^ ) do
    begin
      Font    := zglTCheckBoxDesc( Src^ ).Font;
      Caption := zglTCheckBoxDesc( Src^ ).Caption;
      Checked := zglTCheckBoxDesc( Src^ ).Checked;
    end;
end;

procedure gui_FillRadioButtonDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTRadioButtonDesc ) );

  if Assigned( Src ) Then
  with zglTRadioButtonDesc( Desc^ ) do
    begin
      Font    := zglTRadioButtonDesc( Src^ ).Font;
      Caption := zglTRadioButtonDesc( Src^ ).Caption;
      Checked := zglTRadioButtonDesc( Src^ ).Checked;
      Group   := zglTRadioButtonDesc( Src^ ).Group;
    end;
end;

procedure gui_FillLabelDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTLabelDesc ) );

  if Assigned( Src ) Then
  with zglTLabelDesc( Desc^ ) do
    begin
      Font    := zglTLabelDesc( Src^ ).Font;
      Caption := zglTLabelDesc( Src^ ).Caption;
    end;
end;

procedure gui_FillEditBoxDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTEditBoxDesc ) );

  if Assigned( Src ) Then
  with zglTEditBoxDesc( Desc^ ) do
    begin
      Font     := zglTEditBoxDesc( Src^ ).Font;
      Text     := zglTEditBoxDesc( Src^ ).Text;
      Max      := zglTEditBoxDesc( Src^ ).Max;
      ReadOnly := zglTEditBoxDesc( Src^ ).ReadOnly;
    end;
end;

procedure gui_FillListBoxDesc;
  var
    i : Integer;
begin
  zgl_GetMem( Desc, SizeOf( zglTListBoxDesc ) );

  if Assigned( Src ) Then
  with zglTListBoxDesc( Desc^ ) do
    begin
      Font       := zglTListBoxDesc( Src^ ).Font;
      SelectMode := zglTListBoxDesc( Src^ ).SelectMode;
      ItemIndex  := zglTListBoxDesc( Src^ ).ItemIndex;
      ItemHeight := zglTListBoxDesc( Src^ ).ItemHeight;
      if ItemHeight = 0 Then
        ItemHeight := Font.MaxHeight + 4;
      List.Count := zglTListBoxDesc( Src^ ).List.Count;
      SetLength( List.Items, List.Count );
      for i := 0 to List.Count - 1 do
        List.Items[ i ] := zglTListBoxDesc( Src^ ).List.Items[ i ];
    end;
end;

procedure gui_FillComboBoxDesc;
  var
    i : Integer;
begin
  zgl_GetMem( Desc, SizeOf( zglTComboBoxDesc ) );

  if Assigned( Src ) Then
  with zglTComboBoxDesc( Desc^ ) do
    begin
      Font          := zglTComboBoxDesc( Src^ ).Font;
      ItemIndex     := zglTComboBoxDesc( Src^ ).ItemIndex;
      ItemHeight    := zglTComboBoxDesc( Src^ ).ItemHeight;
      if ItemHeight = 0 Then
        ItemHeight := Font.MaxHeight + 4;
      DropDownCount := zglTComboBoxDesc( Src^ ).DropDownCount;
      List.Count := zglTListBoxDesc( Src^ ).List.Count;
      SetLength( List.Items, List.Count );
      for i := 0 to List.Count - 1 do
        List.Items[ i ] := zglTComboBoxDesc( Src^ ).List.Items[ i ];
    end;
end;

procedure gui_FillGroupBoxDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTGroupBoxDesc ) );

  if Assigned( Src ) Then
  with zglTGroupBoxDesc( Desc^ ) do
    begin
      Font    := zglTGroupBoxDesc( Src^ ).Font;
      Caption := zglTGroupBoxDesc( Src^ ).Caption;
    end;
end;

procedure gui_FillScrollBoxDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTScrollBoxDesc ) );

  if Assigned( Src ) Then
  with zglTScrollBoxDesc( Desc^ ) do
    begin
    end;
end;

procedure gui_FillSpinDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTSpinDesc ) );

  if Assigned( Src ) Then
  with zglTSpinDesc( Desc^ ) do
    begin
      Value    := zglTSpinDesc( Src^ ).Value;
      Max      := zglTSpinDesc( Src^ ).Max;
      Min      := zglTSpinDesc( Src^ ).Min;
      UPressed := zglTSpinDesc( Src^ ).UPressed;
      DPressed := zglTSpinDesc( Src^ ).DPressed;
    end;
end;

procedure gui_FillScrollBarDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTScrollBarDesc ) );

  if Assigned( Src ) Then
  with zglTScrollBarDesc( Desc^ ) do
    begin
      Kind     := zglTScrollBarDesc( Src^ ).Kind;
      Step     := zglTScrollBarDesc( Src^ ).Step;
      Position := zglTScrollBarDesc( Src^ ).Position;
      PageSize := zglTScrollBarDesc( Src^ ).PageSize;
      Max      := zglTScrollBarDesc( Src^ ).Max;
      UPressed := zglTScrollBarDesc( Src^ ).UPressed;
      DPressed := zglTScrollBarDesc( Src^ ).DPressed;
      SDraged  := zglTScrollBarDesc( Src^ ).SDraged;
    end;
end;

end.
