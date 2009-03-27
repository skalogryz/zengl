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
unit zgl_gui_process;

{$I zgl_config.cfg}

interface
uses
  zgl_gui_types;

procedure gui_ProcWidget( const Widget : zglPWidget );

procedure gui_ProcEvents     ( const Event : zglPEvent );
procedure gui_ProcButton     ( const Event : zglPEvent );
procedure gui_ProcCheckBox   ( const Event : zglPEvent );
procedure gui_ProcRadioButton( const Event : zglPEvent );
procedure gui_ProcLabel      ( const Event : zglPEvent );
procedure gui_ProcEditBox    ( const Event : zglPEvent );
procedure gui_ProcListBox    ( const Event : zglPEvent );
procedure gui_ProcGroupBox   ( const Event : zglPEvent );
procedure gui_ProcSpin       ( const Event : zglPEvent );

type zglProcWidgetsCallback = procedure( const widget : zglPWidget; const data : Pointer );

implementation
uses
  zgl_gui_main,
  zgl_mouse,
  zgl_keyboard,
  zgl_collision_2d;

var
  mouseTimeDown : Integer;

procedure _proc( const callback : zglProcWidgetsCallback; const data : Pointer );
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

procedure _focus_reset( const widget : zglPWidget; const data : Pointer );
begin
  if widget.Focus Then
    gui_AddEvent( EVENT_FOCUS_OUT, widget, nil );
  widget.Focus := FALSE;
end;

procedure _rbutton_reset( const widget : zglPWidget; const data : Pointer );
begin
  if widget._type = WIDGET_RADIOBUTTON Then
    if zglTRadioButtonDesc( widget.desc^ ).Group = PInteger( data )^ Then
      zglTRadioButtonDesc( widget.desc^ ).Checked := FALSE;
end;

procedure gui_ProcWidget;
  var
    i     : Integer;
    Event : zglTEvent;
    w     : zglPWidget;
begin
  if not Assigned( Widget ) Then exit;

  if Assigned( Widget.child ) Then
    begin
      w := Widget.child;
      repeat
        w := w.Next;
        gui_ProcWidget( w );
      until not Assigned( w.Next );
    end;
  if col2d_PointInRect( mouse_X, mouse_Y, Widget.rect ) and col2d_PointInRect( mouse_X, mouse_Y, Widget.parent.rect ) Then
    begin
      Event.mouse_pos.X := Widget.rect.X - mouse_X;
      Event.mouse_pos.Y := Widget.rect.Y - mouse_Y;
      gui_AddEvent( EVENT_MOUSE_MOVE, Widget, @Event.mouse_pos );

      if not Widget.mousein Then
        begin
          Widget.mousein := TRUE;
          gui_AddEvent( EVENT_MOUSE_ENTER, Widget, nil );
        end;

      if mouse_Down( M_BLEFT ) Then
        begin
          INC( mouseTimeDown );
          Event.mouse_button := M_BLEFT;
          gui_AddEvent( EVENT_MOUSE_DOWN, Widget, @Event.mouse_button );
        end;
      if mouse_Down( M_BRIGHT ) Then
        begin
          INC( mouseTimeDown );
          Event.mouse_button := M_BRIGHT;
          gui_AddEvent( EVENT_MOUSE_DOWN, Widget, @Event.mouse_button );
        end;
      if mouse_Down( M_BMIDLE ) Then
        begin
          INC( mouseTimeDown );
          Event.mouse_button := M_BMIDLE;
          gui_AddEvent( EVENT_MOUSE_DOWN, Widget, @Event.mouse_button );
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
          mouseTimeDown := 0;
          Event.mouse_button := M_BLEFT;
          gui_AddEvent( EVENT_MOUSE_UP, Widget, @Event.mouse_button );
        end;
      if mouse_Up( M_BRIGHT ) Then
        begin
          mouseTimeDown := 0;
          Event.mouse_button := M_BRIGHT;
          gui_AddEvent( EVENT_MOUSE_UP, Widget, @Event.mouse_button );
        end;
      if mouse_Up( M_BMIDLE ) Then
        begin
          mouseTimeDown := 0;
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
      EVENT_FOCUS_IN:
        begin
          if Assigned( Widget.Events.OnFocus ) Then
            Widget.Events.OnFocus( Widget, TRUE );
        end;
      EVENT_FOCUS_OUT:
        begin
          if Assigned( Widget.Events.OnFocus ) Then
            Widget.Events.OnFocus( Widget, FALSE );
        end;
      EVENT_MOUSE_MOVE:
        begin
          if Assigned( Widget.Events.OnMouseMove ) Then
            Widget.Events.OnMouseMove( Widget, mouse_pos.X, mouse_pos.Y );
        end;
      EVENT_MOUSE_ENTER:
        begin
          if Assigned( Widget.Events.OnMouseEnter ) Then
            Widget.Events.OnMouseEnter( Widget );
        end;
      EVENT_MOUSE_LEAVE:
        begin
          mouseTimeDown := 0;
          if Assigned( Widget.Events.OnMouseLeave ) Then
            Widget.Events.OnMouseLeave( Widget );
        end;
      EVENT_MOUSE_CLICK:
        begin
          _proc( _focus_reset, nil );
          if not Widget.focus Then
            begin
              gui_AddEvent( EVENT_FOCUS_IN, Widget, nil );
              Widget.focus := TRUE;
            end;

          if Assigned( Widget.Events.OnClick ) and ( Widget._type <> WIDGET_BUTTON ) Then
            Widget.Events.OnClick( Widget );
        end;
      EVENT_KEY_DOWN:
        begin
          if Assigned( Widget.Events.OnKeyDown ) Then
            Widget.Events.OnKeyDown( Widget, key_code );
        end;
      EVENT_KEY_UP:
        begin
          if key_code = K_TAB Then
            begin
              _proc( _focus_reset, nil );
              if Assigned( Widget.Next ) Then
                begin
                  gui_AddEvent( EVENT_FOCUS_IN, Widget.Next, nil );
                  Widget.Next.focus := TRUE
                end;
            end;
          if Assigned( Widget.Events.OnKeyUp ) Then
            Widget.Events.OnKeyUp( Widget, key_code );
        end;
    end;
end;

procedure gui_ProcButton;
begin
  with Event^, zglTButtonDesc( Widget.desc^ ) do
    case _type of
      EVENT_MOUSE_CLICK:
        begin
          Pressed := ( mouse_button = M_BLEFT );
        end;
      EVENT_MOUSE_UP:
        begin
          if mouse_button = M_BLEFT Then
            begin
              if Assigned( Widget.Events.OnClick ) and Pressed Then
                Widget.Events.OnClick( Widget );
              Pressed := FALSE;
            end;
        end;
      EVENT_MOUSE_LEAVE:
        begin
          Pressed := FALSE;
        end;
      EVENT_KEY_DOWN:
        begin
          Pressed := ( ( key_code = K_SPACE ) or ( key_code = K_ENTER ) );
        end;
      EVENT_KEY_UP:
        begin
          if ( key_code = K_ENTER ) or ( key_code = K_SPACE ) Then
            begin
              if Assigned( Widget.Events.OnClick ) Then
                Widget.Events.OnClick( Widget );
              Pressed := FALSE;
            end;
        end;
    end;
  gui_ProcEvents( Event );
end;

procedure gui_ProcCheckBox;
begin
  with Event^, zglTCheckBoxDesc( Widget.desc^ ) do
    case _type of
      EVENT_MOUSE_CLICK:
        begin
          if mouse_button = M_BLEFT Then
            Checked := not Checked;
        end;
      EVENT_KEY_UP:
        begin
          if key_code = K_SPACE Then
            begin
              if Assigned( Widget.Events.OnClick ) Then
                Widget.Events.OnClick( Widget );
              Checked := not Checked;
            end;
        end;
    end;
  gui_ProcEvents( Event );
end;

procedure gui_ProcRadioButton;
begin
  with Event^, zglTRadioButtonDesc( Widget.desc^ ) do
    case _type of
      EVENT_MOUSE_UP:
        begin
          if mouse_button = M_BLEFT Then
            begin
              _proc( _rbutton_reset, @Group );
              Checked := TRUE;
            end;
        end;
    end;
  gui_ProcEvents( Event );
end;

procedure gui_ProcLabel;
begin
  gui_ProcEvents( Event );
end;

procedure gui_ProcEditBox;
begin
  with Event^, zglTEditBoxDesc( Widget.desc^ ) do
    case _type of
      EVENT_FOCUS_IN:
        begin
          key_BeginReadText( Text, Max );
        end;
      EVENT_KEY_DOWN:
        begin
          key_EndReadText( Text );
        end;
    end;
  gui_ProcEvents( Event );
end;

procedure gui_ProcListBox;
begin
  gui_ProcEvents( Event );
end;

procedure gui_ProcGroupBox;
begin
end;

procedure gui_ProcSpin;
begin
  with Event^, Widget.rect, zglTSpinDesc( Widget.desc^ ) do
    case _type of
      EVENT_MOUSE_MOVE:
        begin
          if mouse_Y < Y + H / 2 Then DPressed := FALSE;
          if mouse_Y > Y + H / 2 Then UPressed := FALSE;
        end;
      EVENT_MOUSE_DOWN:
        begin
          if ( mouseTimeDown > 50 ) and ( mouse_button = M_BLEFT ) Then
            begin
              DEC( mouseTimeDown, 15 );
              gui_AddEvent( EVENT_MOUSE_CLICK, Widget, @Event.mouse_button );
            end;
        end;
      EVENT_MOUSE_CLICK:
        begin
          if mouse_button = M_BLEFT Then
            begin
              if mouse_Y < Y + H / 2 Then
                begin
                  UPressed := TRUE;
                  if Value < Max Then
                    INC( Value );
                end;
              if mouse_Y > Y + H / 2 Then
                begin
                  DPressed := TRUE;
                  if Value > Min Then
                  DEC( Value );
                end;
            end;
        end;
      EVENT_MOUSE_UP:
        begin
          if mouse_button = M_BLEFT Then
            begin
              if mouse_Y < Y + H / 2 Then
                UPressed := FALSE;
              if mouse_Y > Y + H / 2 Then
                DPressed := FALSE;
            end;
        end;
      EVENT_MOUSE_LEAVE:
        begin
          UPressed := FALSE;
          DPressed := FALSE;
        end;
    end;
  gui_ProcEvents( Event );
end;

end.
