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
unit zgl_gui_process;

{$I define.inc}

interface
uses
  zgl_types,
  zgl_global_var,
  zgl_gui_types,
  zgl_collision_2d,
  zgl_mouse,
  zgl_keyboard,
  zgl_math;

procedure gui_ProcWidget( const Widget : zglPWidget );

procedure gui_ProcEvents  ( const Event : zglPEvent );
procedure gui_ProcButton  ( const Event : zglPEvent );
procedure gui_ProcCheckBox( const Event : zglPEvent );

implementation
uses
  zgl_gui_main;

procedure gui_ProcWidget;
  var
    i     : Integer;
    Event : zglTEvent;
begin
  if col2d_PointInRect( mouse_X, mouse_Y, Widget.rect ) Then
    begin
      Event.mouse_pos.X := Widget.rect.X - mouse_X;
      Event.mouse_pos.Y := Widget.rect.Y - mouse_Y;
      gui_AddEvent( EVENT_MOUSE_MOVE, Widget, @Event.mouse_pos );

      if not Widget.mousein Then
        begin
          Widget.mousein := TRUE;
          gui_AddEvent( EVENT_MOUSE_ENTER, Widget, nil );
        end;

      if mouse_Click( M_BLEFT ) Then
        begin
          Event.mouse_button := M_BLEFT;
          gui_AddEvent( EVENT_MOUSE_CLICK, Widget, @Event.mouse_button );
        end;
      if mouse_Click( M_BRIGHT ) Then
        begin
          Event.mouse_button := M_BRIGHT;
          gui_AddEvent( EVENT_MOUSE_CLICK, Widget, @Event.mouse_button );
        end;
      if mouse_Click( M_BMIDLE ) Then
        begin
          Event.mouse_button := M_BMIDLE;
          gui_AddEvent( EVENT_MOUSE_CLICK, Widget, @Event.mouse_button );
        end;

      if mouse_Up( M_BLEFT ) Then
        begin
          Event.mouse_button := M_BLEFT;
          gui_AddEvent( EVENT_MOUSE_UP, Widget, @Event.mouse_button );
        end;
      if mouse_Up( M_BRIGHT ) Then
        begin
          Event.mouse_button := M_BRIGHT;
          gui_AddEvent( EVENT_MOUSE_UP, Widget, @Event.mouse_button );
        end;
      if mouse_Up( M_BMIDLE ) Then
        begin
          Event.mouse_button := M_BMIDLE;
          gui_AddEvent( EVENT_MOUSE_UP, Widget, @Event.mouse_button );
        end;
    end else
      begin
        if Widget.mousein Then
          gui_AddEvent( EVENT_MOUSE_LEAVE, Widget, nil );
        Widget.mousein := FALSE;
      end;

  if Widget.focus Then
    begin
      if key_Last( KA_DOWN ) <> 0 Then
        begin
          Event.key_code := key_Last( KA_DOWN );
          gui_AddEvent( EVENT_KEY_DOWN, Widget, @Event.key_code );
        end;
      if key_Last( KA_UP ) <> 0 Then
        begin
          Event.key_code := key_Last( KA_UP );
          gui_AddEvent( EVENT_KEY_UP, Widget, @Event.key_code );
        end;
    end;
end;

procedure gui_ProcEvents;
begin
  with Event^ do
    case _type of
      EVENT_MOUSE_MOVE:
        begin
          if Assigned( Widget.Events.OnMouseMove ) Then
            Widget.Events.OnMouseMove( Widget, Event.mouse_pos.X, Event.mouse_pos.Y );
        end;
      EVENT_MOUSE_ENTER:
        begin
          if Assigned( Widget.Events.OnMouseEnter ) Then
            Widget.Events.OnMouseEnter( Widget );
        end;
      EVENT_MOUSE_LEAVE:
        begin
          if Assigned( Widget.Events.OnMouseLeave ) Then
            Widget.Events.OnMouseLeave( Widget );
        end;
      EVENT_MOUSE_CLICK:
        begin
          if Event.mouse_button = M_BLEFT Then
            Widget.focus := TRUE;
          if Assigned( Widget.Events.OnClick ) Then
            Widget.Events.OnClick( Widget );
        end;
      EVENT_KEY_DOWN:
        begin
          if Assigned( Widget.Events.OnKeyDown ) Then
            Widget.Events.OnKeyDown( Widget, key_code );
        end;
      EVENT_KEY_UP:
        begin
          if Event.key_code = K_TAB Then
            Widget.focus := FALSE;
          if Assigned( Widget.Events.OnKeyUp ) Then
            Widget.Events.OnKeyUp( Widget, key_code );
        end;
    end;
end;

procedure gui_ProcButton;
begin
  gui_ProcEvents( Event );
  with Event^ do
    case _type of
      EVENT_MOUSE_CLICK:
        begin
          if mouse_button = M_BLEFT Then
            zglTButtonDesc( Widget.desc^ ).Pressed := TRUE;
        end;
      EVENT_MOUSE_UP:
        begin
          if mouse_button = M_BLEFT Then
            zglTButtonDesc( Widget.desc^ ).Pressed := FALSE;
        end;
      EVENT_MOUSE_LEAVE:
        begin
          zglTButtonDesc( Widget.desc^ ).Pressed := FALSE;
        end;
      EVENT_KEY_DOWN:
        begin
          if ( key_code = K_SPACE ) or ( key_code = K_ENTER ) Then
            zglTButtonDesc( Widget.desc^ ).Pressed := TRUE;
        end;
      EVENT_KEY_UP:
        begin
          if ( key_code = K_ENTER ) or ( key_code = K_SPACE ) Then
            begin
              if Assigned( Widget.Events.OnClick ) Then
                Widget.Events.OnClick( Widget );
              zglTButtonDesc( Widget.desc^ ).Pressed := FALSE;
            end;
        end;
    end;
end;

procedure gui_ProcCheckBox;
begin
  gui_ProcEvents( Event );
  with Event^ do
    case _type of
      EVENT_MOUSE_CLICK:
        begin
          if mouse_button = M_BLEFT Then
            zglTCheckBoxDesc( Widget.desc^ ).Checked := not zglTCheckBoxDesc( Widget.desc^ ).Checked;
        end;
      EVENT_KEY_UP:
        begin
          if key_code = K_SPACE Then
            begin
              if Assigned( Widget.Events.OnClick ) Then
                Widget.Events.OnClick( Widget );
              zglTCheckBoxDesc( Widget.desc^ ).Checked := not zglTCheckBoxDesc( Widget.desc^ ).Checked;
            end;
        end;
    end;
end;

end.
