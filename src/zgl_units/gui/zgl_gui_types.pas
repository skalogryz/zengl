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

const
  EVENT_MOUSE_MOVE  = 1;
  EVENT_MOUSE_DOWN  = 2;
  EVENT_MOUSE_CLICK = 3;
  EVENT_MOUSE_ENTER = 4;
  EVENT_MOUSE_LEAVE = 5;

type
  zglPEvent  = ^zglTEvent;
  zglPWidget = ^zglTWidget;

  //Widget
  zglTWidget = record
    _type      : Integer;
    desc       : Pointer;
    data       : Pointer;

    events     : zglPEvent;
    parent     : zglPWidget;
    Next, Prev : zglPWidget;
end;

  //Event
  zglTEvent = record
    _type      : Integer;
    data       : Pointer;
    OnEvent    : procedure( Sender : zglPWidget; Desc, Data : Pointer ); extdecl;
    Next, Prev : zglPEvent;
end;

  zglPButtonDesc = ^zglTButtonDesc;
  zglTButtonDesc = record
    X, Y, W, H : Single;
    Caption    : PChar;
end;

  zglPCheckBoxDesc = ^zglTCheckBoxDesc;
  zglTCheckBoxDesc = record
    X, Y, W, H : Single;
    Caption    : PChar;
    Checked    : Boolean;
end;

implementation

end.
