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
unit zgl_gui_process;

{$I zgl_config.cfg}

interface
uses
  zgl_gui_types;

function gui_ProcWidget( const Widget : zglPWidget ) : Boolean;

procedure gui_ProcEvents     ( const Event : zglPEvent );
procedure gui_ProcButton     ( const Event : zglPEvent );
procedure gui_ProcCheckBox   ( const Event : zglPEvent );
procedure gui_ProcRadioButton( const Event : zglPEvent );
procedure gui_ProcLabel      ( const Event : zglPEvent );
procedure gui_ProcEditBox    ( const Event : zglPEvent );
procedure gui_ProcListBox    ( const Event : zglPEvent );
procedure gui_ProcComboBox   ( const Event : zglPEvent );
procedure gui_ProcGroupBox   ( const Event : zglPEvent );
procedure gui_ProcScrollBox  ( const Event : zglPEvent );
procedure gui_ProcSpin       ( const Event : zglPEvent );
procedure gui_ProcScrollBar  ( const Event : zglPEvent );

implementation
uses
  zgl_gui_main,
  zgl_gui_render,
  zgl_gui_utils,
  zgl_mouse,
  zgl_keyboard,
  zgl_math_2d,
  zgl_collision_2d;

var
  mouseTimeDown : Integer;
  dragBegin     : Boolean;
  dragMove      : Boolean;
  dragWidget    : zglPWidget;
  mouseShiftX,
  mouseShiftY   : Integer;

function gui_ProcWidget;
  var
    i     : Integer;
    Event : zglTEvent;
begin
  Result := FALSE;
  if ( not Assigned( Widget ) ) or ( not Widget.visible ) Then exit;

  gui_AlignWidget( Widget );
  gui_UpdateClient( Widget );

  if mouse_Up( M_BLEFT ) Then
    begin
      dragBegin     := FALSE;
      dragMove      := FALSE;
      dragWidget    := nil;
      Widget.draged := FALSE;
      Event.drag_pos.X := mouse_X - mouseShiftX;
      Event.drag_pos.Y := mouse_Y - mouseShiftY;
      gui_AddEvent( EVENT_DRAG_END, Widget, nil, @Event.drag_pos );
    end;
  if Widget.draged Then
    begin
      Result           := TRUE;
      Event.drag_pos.X := mouse_X - mouseShiftX;
      Event.drag_pos.Y := mouse_Y - mouseShiftY;
      gui_AddEvent( EVENT_DRAG_MOVE, Widget, nil, @Event.drag_pos );
    end;

  i := 0;
  while i < Widget.parts do
    begin
      if gui_ProcWidget( Widget.part[ i ] ) Then exit;
      if Assigned( Widget.part[ i ] ) Then INC( i );
    end;

  i := 0;
  while i < Widget.children do
    begin
      if Widget.child[ i ].modal Then
        if gui_ProcWidget( Widget.child[ i ] ) Then exit;
      if Assigned( Widget.child[ i ] ) Then INC( i );
    end;

  i := 0;
  while i < Widget.children do
    begin
      if not Widget.child[ i ].modal Then
        if gui_ProcWidget( Widget.child[ i ] ) Then exit;
      if Assigned( Widget.child[ i ] ) Then INC( i );
    end;

  if ( col2d_PointInRect( mouse_X, mouse_Y, Widget.rect ) or col2d_PointInRect( mouse_X, mouse_Y, Widget.rectEx ) ) and
     ( col2d_PointInRect( mouse_X, mouse_Y, Widget.parent.rect ) or col2d_PointInRect( mouse_X, mouse_Y, Widget.parent.rectEx ) ) Then
    begin
      Result := TRUE;

      Event.mouse_pos.X := Widget.rect.X - mouse_X;
      Event.mouse_pos.Y := Widget.rect.Y - mouse_Y;
      gui_AddEvent( EVENT_MOUSE_MOVE, Widget, nil, @Event.mouse_pos );

      if not Widget.mousein Then
        begin
          Widget.mousein := TRUE;
          gui_AddEvent( EVENT_MOUSE_ENTER, Widget, nil, nil );
        end;

      if dragBegin Then
        if ( ( mouseShiftX <> mouse_X - Round( Widget.rect.X ) ) or
             ( mouseShiftY <> mouse_Y - Round( Widget.rect.Y ) ) ) and ( not Widget.draged ) Then
          begin
            Widget.draged := TRUE;
            dragMove      := TRUE;
            if Assigned( Widget.Events.OnStartDrag ) Then
              Widget.Events.OnStartDrag( Widget );
            gui_AddEvent( EVENT_DRAG_START, Widget, nil, nil );
          end;
      if dragMove and ( Widget <> dragWidget ) Then exit;

      if mouse_Click( M_BLEFT ) Then
        begin
          if not dragBegin Then
            begin
              mouseShiftX := mouse_X - Round( Widget.rect.X );
              mouseShiftY := mouse_Y - Round( Widget.rect.Y );
              dragBegin   := TRUE;
              dragWidget  := Widget;
            end;

          Event.mouse_button := M_BLEFT;
          gui_AddEvent( EVENT_MOUSE_CLICK, Widget, nil, @Event.mouse_button );
        end;
      if mouse_Click( M_BRIGHT ) Then
        begin
          Event.mouse_button := M_BRIGHT;
          gui_AddEvent( EVENT_MOUSE_CLICK, Widget, nil, @Event.mouse_button );
        end;
      if mouse_Click( M_BMIDLE ) Then
        begin
          Event.mouse_button := M_BMIDLE;
          gui_AddEvent( EVENT_MOUSE_CLICK, Widget, nil, @Event.mouse_button );
        end;

      if mouse_Down( M_BLEFT ) Then
        begin
          INC( mouseTimeDown );
          Event.mouse_button := M_BLEFT;
          gui_AddEvent( EVENT_MOUSE_DOWN, Widget, nil, @Event.mouse_button );
        end;
      if mouse_Down( M_BRIGHT ) Then
        begin
          INC( mouseTimeDown );
          Event.mouse_button := M_BRIGHT;
          gui_AddEvent( EVENT_MOUSE_DOWN, Widget, nil, @Event.mouse_button );
        end;
      if mouse_Down( M_BMIDLE ) Then
        begin
          INC( mouseTimeDown );
          Event.mouse_button := M_BMIDLE;
          gui_AddEvent( EVENT_MOUSE_DOWN, Widget, nil, @Event.mouse_button );
        end;

      if mouse_Up( M_BLEFT ) Then
        begin
          mouseTimeDown := 0;
          Event.mouse_button := M_BLEFT;
          gui_AddEvent( EVENT_MOUSE_UP, Widget, nil, @Event.mouse_button );
        end;
      if mouse_Up( M_BRIGHT ) Then
        begin
          mouseTimeDown := 0;
          Event.mouse_button := M_BRIGHT;
          gui_AddEvent( EVENT_MOUSE_UP, Widget, nil, @Event.mouse_button );
        end;
      if mouse_Up( M_BMIDLE ) Then
        begin
          mouseTimeDown := 0;
          Event.mouse_button := M_BMIDLE;
          gui_AddEvent( EVENT_MOUSE_UP, Widget, nil, @Event.mouse_button );
        end;

      if mouse_Wheel( M_WUP ) Then
        begin
          Event.mouse_wheel := M_WUP;
          gui_AddEvent( EVENT_MOUSE_WHEEL, Widget, nil, @Event.mouse_wheel );
        end;
      if mouse_Wheel( M_WDOWN ) Then
        begin
          Event.mouse_wheel := M_WDOWN;
          gui_AddEvent( EVENT_MOUSE_WHEEL, Widget, nil, @Event.mouse_wheel );
        end;
    end else
      begin
        if Widget.mousein Then
          gui_AddEvent( EVENT_MOUSE_LEAVE, Widget, nil, nil );
        Widget.mousein := FALSE;
      end;

  if Widget.focus Then
    begin
      if key_Last( KA_DOWN ) <> 0 Then
        begin
          Result := TRUE;
          Event.key_code := key_Last( KA_DOWN );
          gui_AddEvent( EVENT_KEY_DOWN, Widget, nil, @Event.key_code );
        end;
      if key_Last( KA_UP ) <> 0 Then
        begin
          Result := TRUE;
          Event.key_code := key_Last( KA_UP );
          gui_AddEvent( EVENT_KEY_UP, Widget, nil, @Event.key_code );
        end;
    end;
end;

procedure gui_ProcEvents;
begin
  with Event^ do
    case _type of
      EVENT_CREATE:
        begin
          if Assigned( Widget.Events.OnCreate ) Then
            Widget.Events.OnCreate( Widget );
        end;
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
      EVENT_DRAG_START:
        begin
          if Assigned( Widget.Events.OnStartDrag ) Then
            Widget.Events.OnStartDrag( Widget );
        end;
      EVENT_DRAG_MOVE:
        begin
          if Assigned( Widget.Events.OnDrag ) Then
            Widget.Events.OnDrag( Widget, drag_pos.X, drag_pos.Y );
        end;
      EVENT_DRAG_END:
        begin
          if Assigned( Widget.Events.OnEndDrag ) Then
            Widget.Events.OnEndDrag( Widget, drag_pos.X, drag_pos.Y );
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
          gui_ProcCallback( nil, Widget, gui_ResetFocus, Widget );
          if not Widget.focus Then
            begin
              gui_AddEvent( EVENT_FOCUS_IN, Widget, nil, nil );
              Widget.focus := TRUE;
            end;

          if Assigned( Widget.Events.OnClick ) and ( Widget._type <> WIDGET_BUTTON ) Then
            Widget.Events.OnClick( Widget );
        end;
      EVENT_MOUSE_WHEEL:
        begin
          if Assigned( Widget.Events.OnMouseWheel ) Then
            Widget.Events.OnMouseWheel( Widget, mouse_wheel );
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
              gui_ProcCallback( nil, Widget, gui_ResetFocus, nil );
              if Widget._id + 1 < Widget.Parent.children Then
                begin
                  gui_AddEvent( EVENT_FOCUS_IN, Widget.Parent.child[ Widget._id + 1 ], nil, nil );
                  Widget.Parent.child[ Widget._id + 1 ].focus := TRUE
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
      EVENT_MOUSE_CLICK:
        begin
          if mouse_button = M_BLEFT Then
            begin
              gui_ProcCallback( nil, Widget, gui_ResetChecked, @Group );
              Checked := TRUE;
            end;
        end;
      EVENT_KEY_UP:
        begin
          if key_code = K_SPACE Then
            begin
              if Assigned( Widget.Events.OnClick ) Then
                Widget.Events.OnClick( Widget );
              gui_ProcCallback( nil, Widget, gui_ResetChecked, @Group );
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
          if not ReadOnly Then
            key_BeginReadText( Text, Max );
        end;
      EVENT_KEY_DOWN:
        begin
          if not ReadOnly Then
            key_EndReadText( Text );
        end;
    end;
  gui_ProcEvents( Event );
end;

procedure gui_ProcListBox;
  var
    li     : Integer;
    iShift : Integer;
    iCount : Integer;
    Scroll : zglPScrollBarDesc;
begin
  with Event^, zglTListBoxDesc( Widget.desc^ ) do
    begin
      li := -1;
      case _type of
        EVENT_CREATE:
          begin
            gui_AddWidget( WIDGET_SCROLLBAR, Widget.rect.W - SCROLL_SIZE, 0, SCROLL_SIZE, Widget.rect.H, FALSE, TRUE, nil, nil, Widget, TRUE );
          end;
        EVENT_MOUSE_CLICK:
          begin
            if mouse_button = M_BLEFT Then
              begin
                Scroll := Widget.part[ 0 ].desc;
                if not Widget.part[ 0 ].visible Then Scroll.Position := 0;
                li := Round( mouse_Y - Widget.rect.Y - 4 + Scroll.Position ) div ItemHeight;
              end;
          end;
        EVENT_MOUSE_DOWN:
          begin
            if SelectMode = SELECT_BY_DOWN Then
              gui_AddEvent( EVENT_MOUSE_CLICK, Widget, nil, @Event.mouse_button );
          end;
        EVENT_MOUSE_WHEEL:
          begin
            if mouse_X < Widget.rect.X + Widget.rect.W - SCROLL_SIZE - 2 Then
              gui_AddEvent( EVENT_MOUSE_WHEEL, Widget.part[ 0 ], Widget, @Event.mouse_wheel );
          end;
        EVENT_KEY_UP:
          begin
            li := ItemIndex;
            if key_code = K_UP   Then DEC( li );
            if key_code = K_DOWN Then INC( li );
          end;
      end;

      if ( li < List.Count ) and ( li <> -1 ) Then
        begin
          if ( ItemIndex <> li ) and Assigned( Widget.Events.OnChange ) Then
            Widget.Events.OnChange( Widget, li, li - ItemIndex );
          ItemIndex := li;
        end;

      iCount := gui_GetListItemsPerPage( Widget );
      if List.Count > iCount Then
        begin
          Widget.part[ 0 ].visible := TRUE;
          Scroll := Widget.part[ 0 ].desc;
          iShift := Scroll.Position div ItemHeight;

          if ( li < List.Count ) and ( li <> -1 ) and ( Event._type <> EVENT_MOUSE_DOWN ) and ( Event._type <> EVENT_MOUSE_CLICK ) Then
            begin
              if ItemIndex < iShift Then
                Scroll.Position := ItemIndex * ItemHeight;
              if ItemIndex > iShift + iCount - 2 Then
                Scroll.Position := ( ItemIndex - iCount + 1 ) * ItemHeight;
            end;

          Scroll.Max      := List.Count * ItemHeight - Round( Widget.client.H );
          Scroll.PageSize := iCount * ItemHeight;
          Scroll.Step     := ItemHeight;
          if Scroll.Position > Scroll.Max Then
            Scroll.Position := Scroll.Max;
        end else
          Widget.part[ 0 ].visible := FALSE;
    end;
  gui_ProcEvents( Event );
end;

procedure gui_ProcComboBox;
  var
    li     : Integer;
    iShift : Integer;
    iCount : Integer;
    Scroll : zglPScrollBarDesc;
begin
  with Event^, zglTComboBoxDesc( Widget.desc^ ), Widget.rect do
    begin
      li := -1;
      case _type of
        EVENT_CREATE:
          begin
            gui_AddWidget( WIDGET_SCROLLBAR, W - SCROLL_SIZE - 2, H, SCROLL_SIZE, DropDownCount * ItemHeight + 3, FALSE, TRUE, nil, nil, Widget, TRUE );
          end;
        EVENT_FOCUS_OUT:
          begin
            if sender <> Widget.part[ 0 ] Then
              begin
                Widget.modal := FALSE;
                DropedDown   := FALSE;
              end else
                Widget.focus := TRUE;
          end;
        EVENT_MOUSE_CLICK:
          begin
            if DropedDown and col2d_PointInRect( mouse_X, mouse_Y, Widget.rectEx ) Then
              begin
                Scroll := Widget.part[ 0 ].desc;
                if not Widget.part[ 0 ].visible Then Scroll.Position := 0;
                li := Round( mouse_Y - Widget.rect.Y - H - 4 + Scroll.Position ) div ItemHeight;
                if mouse_X < Widget.rect.X + Widget.rect.W - SCROLL_SIZE - 2 Then
                  begin
                    Widget.modal    := FALSE;
                    DropedDown      := FALSE;
                    Scroll.Position := li * ItemHeight;
                  end;
              end;

            if col2d_PointInRect( mouse_X, mouse_Y, Widget.rect ) Then
              begin
                Widget.modal := not Widget.modal;
                DropedDown   := not DropedDown;
              end;
          end;
        EVENT_MOUSE_WHEEL:
          begin
            if Widget.focus and ( not DropedDown ) and col2d_PointInRect( mouse_X, mouse_Y, Widget.rect ) Then
              begin
                li := ItemIndex;
                case Event.mouse_wheel of
                  M_WUP: if li > 0 Then DEC( li );
                  M_WDOWN: if li < List.Count Then INC( li );
                end;
              end;

            if mouse_X < Widget.rect.X + Widget.rect.W - SCROLL_SIZE - 2 Then
              gui_AddEvent( EVENT_MOUSE_WHEEL, Widget.part[ 0 ], Widget, @Event.mouse_wheel );
          end;
        EVENT_KEY_UP:
          begin
            if Widget.focus Then
              begin
                li := ItemIndex;
                if key_code = K_UP   Then DEC( li );
                if key_code = K_DOWN Then INC( li );
              end;
          end;
      end;

      if DropedDown Then
        begin
          Widget.rectEx.X := X;
          Widget.rectEx.Y := Y + H;
          Widget.rectEx.W := W;
          Widget.rectEx.H := DropDownCount * ItemHeight + 4;
        end else
          Widget.rectEx := Widget.rect;

      if ( li < List.Count ) and ( li <> -1 ) Then
        begin
          if ( ItemIndex <> li ) and Assigned( Widget.Events.OnChange ) Then
            Widget.Events.OnChange( Widget, li, li - ItemIndex );
          ItemIndex := li;
        end;

      iCount := gui_GetListItemsPerPage( Widget );
      if ( DropedDown ) and ( List.Count > iCount ) Then
        begin
          Widget.part[ 0 ].visible := TRUE;
          Scroll := Widget.part[ 0 ].desc;
          iShift := Scroll.Position div ItemHeight;

          if ( li < List.Count ) and ( li <> -1 ) and ( Event._type <> EVENT_MOUSE_DOWN ) and ( Event._type <> EVENT_MOUSE_CLICK ) Then
            begin
              if ItemIndex < iShift Then
                Scroll.Position := ItemIndex * ItemHeight;
              if ItemIndex > iShift + iCount - 2 Then
                Scroll.Position := ( ItemIndex - iCount + 1 ) * ItemHeight;
            end;

          Scroll.Max      := ( List.Count - DropDownCount ) * ItemHeight;
          Scroll.PageSize := iCount * ItemHeight;
          Scroll.Step     := ItemHeight;
          if Scroll.Position > Scroll.Max Then
            Scroll.Position := Scroll.Max;
        end else
          Widget.part[ 0 ].visible := FALSE;
    end;
  gui_ProcEvents( Event );
end;

procedure gui_ProcGroupBox;
begin
  gui_ProcEvents( Event );
end;

procedure gui_ProcScrollBox;
begin
  gui_ProcEvents( Event );
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
          if ( mouseTimeDown > 15 ) and ( mouse_button = M_BLEFT ) Then
            begin
              DEC( mouseTimeDown, 5 );
              gui_AddEvent( EVENT_MOUSE_CLICK, Widget, nil, @Event.mouse_button );
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
                    begin
                      INC( Value );
                      if Assigned( Widget.Events.OnChange ) Then
                        Widget.Events.OnChange( Widget, Value, 1 );
                    end;
                end;
              if mouse_Y > Y + H / 2 Then
                begin
                  DPressed := TRUE;
                  if Value > Min Then
                    begin
                      DEC( Value );
                      if Assigned( Widget.Events.OnChange ) Then
                        Widget.Events.OnChange( Widget, Value, -1 );
                    end;
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

procedure gui_ProcScrollBar;
  var
    r : zglTRect;
begin
  with Event^, Widget.rect, zglTScrollBarDesc( Widget.desc^ ) do
    case _type of
      EVENT_CREATE:
        begin
          case Kind of
            SCROLLBAR_VERTICAL:   W := SCROLL_SIZE;
            SCROLLBAR_HORIZONTAL: H := SCROLL_SIZE;
          end;
        end;
      EVENT_DRAG_START:
        begin
          r := gui_GetScrollRect( Widget );
          if col2d_PointInRect( mouse_X, mouse_Y, r ) Then
            begin
              SDraged := TRUE;
              if Kind = SCROLLBAR_VERTICAL Then
                mouseShiftY := mouse_Y - Round( Widget.rect.Y + ( r.Y - Widget.rect.Y ) )
              else
                mouseShiftX := mouse_X - Round( Widget.rect.X + ( r.X - Widget.rect.X ) );
            end;
        end;
      EVENT_DRAG_MOVE:
        begin
          if SDraged Then
            begin
              r := gui_GetScrollRect( Widget );
              if Kind = SCROLLBAR_VERTICAL Then
                gui_ScrollXY2Pos( Widget, 0, Round( drag_pos.Y ) )
              else
                gui_ScrollXY2Pos( Widget, Round( drag_pos.X ), 0 );
            end;
        end;
      EVENT_DRAG_END:
        begin
          SDraged := FALSE;
        end;
      EVENT_MOUSE_MOVE:
        begin
          if Kind = SCROLLBAR_VERTICAL Then
            begin
              if mouse_Y > Y + SCROLL_SIZE     Then UPressed := FALSE;
              if mouse_Y < Y + H - SCROLL_SIZE Then DPressed := FALSE;
            end else
              begin
                if mouse_X > X + SCROLL_SIZE     Then UPressed := FALSE;
                if mouse_X < X + W - SCROLL_SIZE Then DPressed := FALSE;
              end;
        end;
      EVENT_MOUSE_LEAVE:
        begin
          UPressed := FALSE;
          DPressed := FALSE;
        end;
      EVENT_MOUSE_DOWN:
        begin
          if ( mouseTimeDown > 15 ) and ( mouse_button = M_BLEFT ) Then
            begin
              DEC( mouseTimeDown, 5 );
              gui_AddEvent( EVENT_MOUSE_CLICK, Widget, nil, @Event.mouse_button );
            end;
        end;
      EVENT_MOUSE_UP:
        begin
          if mouse_button = M_BLEFT Then
            begin
              UPressed := FALSE;
              DPressed := FALSE;
            end;
        end;
      EVENT_MOUSE_CLICK:
        begin
          r := gui_GetScrollRect( Widget );
          if ( mouse_button = M_BLEFT ) and ( not SDraged ) Then
            begin
              if Kind = SCROLLBAR_VERTICAL Then
                begin
                  if mouse_Y < Y + SCROLL_SIZE     Then UPressed := TRUE;
                  if mouse_Y > Y + H - SCROLL_SIZE Then DPressed := TRUE;
                end else
                  begin
                    if mouse_X < X + SCROLL_SIZE     Then UPressed := TRUE;
                    if mouse_X > X + W - SCROLL_SIZE Then DPressed := TRUE;
                  end;
              if UPressed Then
                gui_ScrollChange( Widget, -Step );

              if DPressed Then
                gui_ScrollChange( Widget, Step );

              if not col2d_PointInRect( mouse_X, mouse_Y, r ) Then
                if Kind = SCROLLBAR_VERTICAL Then
                  begin
                    if ( mouse_Y < r.Y ) and ( mouse_Y > Y + SCROLL_SIZE ) Then
                      gui_ScrollChange( Widget, -PageSize );
                    if ( mouse_Y > r.Y + r.H ) and ( mouse_Y < Y + H - SCROLL_SIZE ) Then
                      gui_ScrollChange( Widget, PageSize );
                  end else
                    begin
                      if ( mouse_X < r.X ) and ( mouse_X > X + SCROLL_SIZE ) Then
                        gui_ScrollChange( Widget, -PageSize );
                      if ( mouse_X > r.X + r.W ) and ( mouse_X < X + W - SCROLL_SIZE ) Then
                        gui_ScrollChange( Widget, PageSize );
                    end;
            end;
          if mouse_button = M_BMIDLE Then
            if Kind = SCROLLBAR_VERTICAL Then
              gui_ScrollXY2Pos( Widget, 0, mouse_Y - Round( r.H / 2 ) )
            else
              gui_ScrollXY2Pos( Widget, mouse_X - Round( r.W / 2 ), 0 );
        end;
      EVENT_MOUSE_WHEEL:
        begin
          if mouse_wheel = M_WUP Then
            gui_ScrollChange( Widget, -Step );
          if mouse_wheel = M_WDOWN Then
            gui_ScrollChange( Widget, Step );
        end;
    end;
  gui_ProcEvents( Event );
end;

end.
