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
    _id        : Integer;
    _type      : Integer;

    Desc       : Pointer;
    Data       : Pointer;
    Rect       : zglTRect;
    RectEx     : zglTRect;
    Client     : zglTRect;

    Align      : LongWord;
    Layer      : Integer;

    Visible    : Boolean;
    Focus      : Boolean;
    Modal      : Boolean;
    MouseIn    : Boolean;
    Draged     : Boolean;

    OnDraw     : procedure( const Widget : zglPWidget );
    OnProc     : procedure( const Event : zglPEvent );
    OnFreeDesc : procedure( var Desc : Pointer );
    OnFreeData : procedure( var Data : Pointer );
    Events     : zglTEvents;

    parent     : zglPWidget;
    children   : Integer;
    child      : array of zglPWidget;
    parts      : Integer;
    part       : array of zglPWidget;
end;

  zglTWidgetType = record
    _type    : Integer;
    FillDesc : procedure( Src : Pointer; var Desc : Pointer );

    OnDraw     : procedure( const Widget : zglPWidget );
    OnProc     : procedure( const Event : zglPEvent );
    OnFreeDesc : procedure( var Desc : Pointer );
    OnFreeData : procedure( var Data : Pointer );
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

    Sender     : zglPWidget;
    Widget     : zglPWidget;

    next, prev : zglPEvent;
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
    Count : LongWord;
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
    Font     : zglPFont;
    Text     : String;
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

    uPressed : Boolean;
    dPressed : Boolean;
end;

  zglPScrollBarDesc = ^zglTScrollBarDesc;
  zglTScrollBarDesc = record
    Kind     : Byte;
    Step     : Integer;
    Position : Integer;
    PageSize : Integer;
    Max      : Integer;

    uPressed : Boolean;
    dPressed : Boolean;
    sDraged  : Boolean;
end;

procedure gui_FillButtonDesc( Src : zglPButtonDesc; var Desc : zglPButtonDesc );
procedure gui_FillCheckBoxDesc( Src : zglPCheckBoxDesc; var Desc : zglPCheckBoxDesc );
procedure gui_FillRadioButtonDesc( Src : zglPRadioButtonDesc; var Desc : zglPRadioButtonDesc );
procedure gui_FillLabelDesc( Src : zglPLabelDesc; var Desc : zglPLabelDesc );
procedure gui_FillEditBoxDesc( Src : zglPEditBoxDesc; var Desc : zglPEditBoxDesc );
procedure gui_FillListBoxDesc( Src : zglPListBoxDesc; var Desc : zglPListBoxDesc );
procedure gui_FillComboBoxDesc( Src : zglPComboBoxDesc; var Desc : zglPComboBoxDesc );
procedure gui_FillGroupBoxDesc( Src : zglPGroupBoxDesc; var Desc : zglPGroupBoxDesc );
procedure gui_FillScrollBoxDesc( Src : zglPScrollBoxDesc; var Desc : zglPScrollBoxDesc );
procedure gui_FillSpinDesc( Src : zglPSpinDesc; var Desc : zglPSpinDesc );
procedure gui_FillScrollBarDesc( Src : zglPScrollBarDesc; var Desc : zglPScrollBarDesc );

procedure gui_FreeListBoxDesc( var Desc : zglPListBoxDesc );
procedure gui_FreeComboBoxDesc( var Desc : zglPComboBoxDesc );

implementation
uses
  zgl_main;

procedure gui_FillButtonDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTButtonDesc ) );

  if Assigned( Src ) Then
    with Desc^ do
      begin
        Font    := Src.Font;
        Caption := Src.Caption;
        Pressed := Src.Pressed;
      end;
end;

procedure gui_FillCheckBoxDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTCheckBoxDesc ) );

  if Assigned( Src ) Then
    with Desc^ do
      begin
        Font    := Src.Font;
        Caption := Src.Caption;
        Checked := Src.Checked;
      end;
end;

procedure gui_FillRadioButtonDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTRadioButtonDesc ) );

  if Assigned( Src ) Then
    with Desc^ do
      begin
        Font    := Src.Font;
        Caption := Src.Caption;
        Checked := Src.Checked;
        Group   := Src.Group;
      end;
end;

procedure gui_FillLabelDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTLabelDesc ) );

  if Assigned( Src ) Then
    begin
      Desc.Font    := Src.Font;
      Desc.Caption := Src.Caption;
    end;
end;

procedure gui_FillEditBoxDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTEditBoxDesc ) );

  if Assigned( Src ) Then
    with Desc^ do
      begin
        Font     := Src.Font;
        Text     := Src.Text;
        Max      := Src.Max;
        ReadOnly := Src.ReadOnly;
      end;
end;

procedure gui_FillListBoxDesc;
  var
    i : Integer;
begin
  zgl_GetMem( Desc, SizeOf( zglTListBoxDesc ) );

  if Assigned( Src ) Then
    with Desc^ do
      begin
        Font       := Src.Font;
        SelectMode := Src.SelectMode;
        ItemIndex  := Src.ItemIndex;
        ItemHeight := Src.ItemHeight;
        if ItemHeight = 0 Then
          ItemHeight := Font.MaxHeight + 4;
        List.Count := Src.List.Count;
        SetLength( List.Items, List.Count );
        for i := 0 to List.Count - 1 do
          List.Items[ i ] := Src.List.Items[ i ];
      end;
end;

procedure gui_FillComboBoxDesc;
  var
    i : Integer;
begin
  zgl_GetMem( Desc, SizeOf( zglTComboBoxDesc ) );

  if Assigned( Src ) Then
    with Desc^ do
      begin
        Font       := Src.Font;
        ItemIndex  := Src.ItemIndex;
        ItemHeight := Src.ItemHeight;
        if ItemHeight = 0 Then
          ItemHeight := Font.MaxHeight + 4;
        DropDownCount := Src.DropDownCount;
        List.Count := Src.List.Count;
        SetLength( List.Items, List.Count );
        for i := 0 to List.Count - 1 do
          List.Items[ i ] := Src.List.Items[ i ];
      end;
end;

procedure gui_FillGroupBoxDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTGroupBoxDesc ) );

  if Assigned( Src ) Then
    with Desc^ do
      begin
        Font    := Src.Font;
        Caption := Src.Caption;
      end;
end;

procedure gui_FillScrollBoxDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTScrollBoxDesc ) );

  if Assigned( Src ) Then
    with Desc^ do
      begin
      end;
end;

procedure gui_FillSpinDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTSpinDesc ) );

  if Assigned( Src ) Then
    with Desc^ do
      begin
        Value := Src.Value;
        Max   := Src.Max;
        Min   := Src.Min;
      end;
end;

procedure gui_FillScrollBarDesc;
begin
  zgl_GetMem( Desc, SizeOf( zglTScrollBarDesc ) );

  if Assigned( Src ) Then
    with Desc^ do
      begin
        Kind     := Src.Kind;
        Step     := Src.Step;
        Position := Src.Position;
        PageSize := Src.PageSize;
        Max      := Src.Max;
      end;
end;

procedure gui_FreeListBoxDesc;
  var
    i : Integer;
begin
  with Desc^ do
    begin
      for i := 0 to List.Count - 1 do
        List.Items[ i ] := '';
      SetLength( List.Items, 0 );
    end;

  FreeMem( Desc );
end;

procedure gui_FreeComboBoxDesc;
  var
    i : Integer;
begin
  with Desc^ do
    begin
      for i := 0 to List.Count - 1 do
        List.Items[ i ] := '';
      SetLength( List.Items, 0 );
    end;

  FreeMem( Desc );
end;

end.
