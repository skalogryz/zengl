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
unit zgl_gui_types;

{$I define.inc}

interface
uses
  zgl_types;

const
  WIDGET_BUTTON     = 1;
  WIDGET_CHECKBOX   = 2;

  EVENT_FOCUS_IN    = 1;
  EVENT_FOCUS_OUT   = 2;

  EVENT_MOUSE_MOVE  = 3;
  EVENT_MOUSE_ENTER = 4;
  EVENT_MOUSE_LEAVE = 5;
  EVENT_MOUSE_DOWN  = 6;
  EVENT_MOUSE_UP    = 7;
  EVENT_MOUSE_CLICK = 8;
  EVENT_MOUSE_WHEEL = 9;

  EVENT_KEY_DOWN    = 10;
  EVENT_KEY_UP      = 11;
  EVENT_KEY_CHAR    = 12;

type
  zglPEvent  = ^zglTEvent;
  zglPWidget = ^zglTWidget;

  //Events
  zglTEvents = record
  case byte of
    0: ( OnClick      : procedure( const Widget : zglPWidget ) );
    1: ( OnMouseUp    : procedure( const Widget : zglPWidget ) );
    2: ( OnMouseMove  : procedure( const Widget : zglPWidget; const X, Y : Single ) );
    3: ( OnMouseEnter : procedure( const Widget : zglPWidget ) );
    4: ( OnMouseLeave : procedure( const Widget : zglPWidget ) );
    5: ( OnKeyDown    : procedure( const Widget : zglPWidget; const KeyCode : Byte ) );
    6: ( OnKeyUp      : procedure( const Widget : zglPWidget; const KeyCode : Byte ) );
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
    Next, Prev : zglPWidget;
end;

  //GUI Manager
  zglPGUIManager = ^zglTGUIManager;
  zglTGUIManager = record
    Count : DWORD;
    First : zglTWidget;
end;

  zglTWidgetType = record
    _type  : Integer;

    OnDraw : procedure( const Widget : zglPWidget );
    OnProc : procedure( const Event  : zglPEvent );
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
    Caption : PChar;
    Font    : zglPFont;

    Pressed : Boolean;
end;

  zglPCheckBoxDesc = ^zglTCheckBoxDesc;
  zglTCheckBoxDesc = record
    Caption : PChar;
    Font    : zglPFont;

    Checked : Boolean;
end;

  zglPEditDesc = ^zglTEditDesc;
  zglTEditDesc = record
    Text : PChar;
    Font : zglPFont;
    Max  : Integer;
end;

implementation

end.
