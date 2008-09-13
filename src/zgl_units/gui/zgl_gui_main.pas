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
unit zgl_gui_main;

{$I define.inc}

interface
uses
  zgl_gui_types;

procedure gui_Draw; extdecl;
procedure gui_Proc; extdecl;

procedure gui_AddWidget; extdecl;
procedure gui_AddButton; extdecl;

var
  widgets : zglTWidget;

implementation

procedure gui_Draw;
begin
end;

procedure gui_Proc;
begin
end;

procedure gui_AddWidget;
begin
end;

procedure gui_AddButton;
begin
end;

end.
