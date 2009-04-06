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
  WIDGET_GROUPBOX    = 7;
  WIDGET_SPIN        = 8;
  WIDGET_SCROLLBAR   = 9;

  // ScrollBar
  SCROLLBAR_VERTICAL   = 0;
  SCROLLBAR_HORIZONTAL = 1;

  // Events
  EVENT_CREATE      = 1;

  EVENT_FOCUS_IN    = 2;
  EVENT_FOCUS_OUT   = 3;

  EVENT_MOUSE_MOVE  = 4;
  EVENT_MOUSE_ENTER = 5;
  EVENT_MOUSE_LEAVE = 6;
  EVENT_MOUSE_DOWN  = 7;
  EVENT_MOUSE_UP    = 8;
  EVENT_MOUSE_CLICK = 9;
  EVENT_MOUSE_WHEEL = 10;

  EVENT_KEY_DOWN    = 11;
  EVENT_KEY_UP      = 12;
  EVENT_KEY_CHAR    = 13;

type
  zglPEvent  = ^zglTEvent;
  zglPWidget = ^zglTWidget;

  //Events
  zglTEvents = record
    OnFocus      : procedure( const Widget : zglPWidget; const Focus : Boolean );
    OnClick      : procedure( const Widget : zglPWidget );
    OnMouseUp    : procedure( const Widget : zglPWidget );
    OnMouseMove  : procedure( const Widget : zglPWidget; const X, Y : Single );
    OnMouseEnter : procedure( const Widget : zglPWidget );
    OnMouseLeave : procedure( const Widget : zglPWidget );
    OnKeyDown    : procedure( const Widget : zglPWidget; const KeyCode : Byte );
    OnKeyUp      : procedure( const Widget : zglPWidget; const KeyCode : Byte );
    OnSelectItem : procedure( const Widget : zglPWidget; const ItemIndex : Integer );
end;

  //Widget
  zglTWidget = record
    _type      : Integer;
    desc       : Pointer;
    data       : Pointer;
    rect       : zglTRect;
    focus      : Boolean;
    mousein    : Boolean;

    OnDraw     : procedure( const Widget : zglPWidget );
    OnProc     : procedure( const Event  : zglPEvent );
    Events     : zglTEvents;

    parent     : zglPWidget;
    child      : zglPWidget;
    Next, Prev : zglPWidget;
end;

  zglTWidgetType = record
    _type    : Integer;
    DescSize : DWORD;

    OnDraw : procedure( const Widget : zglPWidget );
    OnProc : procedure( const Event  : zglPEvent );
end;

  //GUI Manager
  zglPGUIManager = ^zglTGUIManager;
  zglTGUIManager = record
    Count : record
              Items : DWORD;
              Types : DWORD;
            end;
    First : zglTWidget;
    Types : array of zglTWidgetType;
end;

  //Event
  zglTEvent = record
    _type      : Integer;
    widget     : zglPWidget;
    Next, Prev : zglPEvent;
    case byte of
      1: ( mouse_pos    : zglTPoint2D );
      2: ( mouse_button : Byte );
      3: ( key_code     : Byte );
      4: ( key_char     : PChar );
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

    Max  : Integer;
end;

  zglPListBoxDesc = ^zglTListBoxDesc;
  zglTListBoxDesc = record
    Font      : zglPFont;
    List      : zglTStringList;
    ItemIndex : Integer;
end;

  zglPGroupBoxDesc = ^zglTGroupBoxDesc;
  zglTGroupBoxDesc = record
    Font    : zglPFont;
    Caption : String;
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
end;

implementation

end.
