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
unit zgl_gui_std;

interface

uses
  zgl_gui_types;

procedure gui_ScrollListBox( const Widget : zglPWidget; const Value, Change : Integer );

implementation

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
