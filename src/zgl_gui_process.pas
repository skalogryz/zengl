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
unit zgl_gui_process;

{$I zgl_config.cfg}

interface
uses
  zgl_gui_types;

function gui_ProcWidget( const Widget : zglPWidget ) : Boolean;

procedure gui_ProcEvents( const Event : zglPEvent );
procedure gui_ProcButton( const Event : zglPEvent );
procedure gui_ProcCheckBox( const Event : zglPEvent );
procedure gui_ProcRadioButton( const Event : zglPEvent );
procedure gui_ProcLabel( const Event : zglPEvent );
procedure gui_ProcEditBox( const Event : zglPEvent );
procedure gui_ProcListBox( const Event : zglPEvent );
procedure gui_ProcComboBox( const Event : zglPEvent );
procedure gui_ProcGroupBox( const Event : zglPEvent );
procedure gui_ProcScrollBox( const Event : zglPEvent );
procedure gui_ProcSpin( const Event : zglPEvent );
procedure gui_ProcScrollBar( const Event : zglPEvent );

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
    event : zglTEvent;
begin
  Result := FALSE;
  if ( not Assigned( Widget ) ) or ( not Widget.Visible ) Then exit;

  gui_AlignWidget( Widget );
  gui_UpdateClient( Widget );

  if mouse_Up( M_BLEFT ) Then
    begin
      dragBegin     := FALSE;
      dragMove      := FALSE;
      dragWidget    := nil;
      Widget.Draged := FALSE;
      event.drag_pos.X := mouse_X - mouseShiftX;
      event.drag_pos.Y := mouse_Y - mouseShiftY;
      gui_AddEvent( EVENT_DRAG_END, Widget, nil, @event.drag_pos );
    end;
  if Widget.Draged Then
    begin
      Result           := TRUE;
      event.drag_pos.X := mouse_X - mouseShiftX;
      event.drag_pos.Y := mouse_Y - mouseShiftY;
      gui_AddEvent( EVENT_DRAG_MOVE, Widget, nil, @event.drag_pos );
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

  if ( col2d_PointInRect( mouse_X, mouse_Y, Widget.Rect ) or col2d_PointInRect( mouse_X, mouse_Y, Widget.RectEx ) ) and
     ( col2d_PointInRect( mouse_X, mouse_Y, Widget.parent.Rect ) or col2d_PointInRect( mouse_X, mouse_Y, Widget.parent.RectEx ) ) Then
    begin
      Result := TRUE;

      event.mouse_pos.X := Widget.Rect.X - mouse_X;
      event.mouse_pos.Y := Widget.Rect.Y - mouse_Y;
      gui_AddEvent( EVENT_MOUSE_MOVE, Widget, nil, @event.mouse_pos );

      if not Widget.MouseIn Then
        begin
          Widget.MouseIn := TRUE;
          gui_AddEvent( EVENT_MOUSE_ENTER, Widget, nil, nil );
        end;

      if dragBegin Then
        if ( ( mouseShiftX <> mouse_X - Round( Widget.Rect.X ) ) or
             ( mouseShiftY <> mouse_Y - Round( Widget.Rect.Y ) ) ) and ( not Widget.Draged ) Then
          begin
            Widget.Draged := TRUE;
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
              mouseShiftX := mouse_X - Round( Widget.Rect.X );
              mouseShiftY := mouse_Y - Round( Widget.Rect.Y );
              dragBegin   := TRUE;
              dragWidget  := Widget;
            end;

          Event.mouse_button := M_BLEFT;
          gui_AddEvent( EVENT_MOUSE_CLICK, Widget, nil, @event.mouse_button );
        end;
      if mouse_Click( M_BRIGHT ) Then
        begin
          event.mouse_button := M_BRIGHT;
          gui_AddEvent( EVENT_MOUSE_CLICK, Widget, nil, @event.mouse_button );
        end;
      if mouse_Click( M_BMIDLE ) Then
        begin
          event.mouse_button := M_BMIDLE;
          gui_AddEvent( EVENT_MOUSE_CLICK, Widget, nil, @event.mouse_button );
        end;

      if mouse_Down( M_BLEFT ) Then
        begin
          INC( mouseTimeDown );
          event.mouse_button := M_BLEFT;
          gui_AddEvent( EVENT_MOUSE_DOWN, Widget, nil, @event.mouse_button );
        end;
      if mouse_Down( M_BRIGHT ) Then
        begin
          INC( mouseTimeDown );
          event.mouse_button := M_BRIGHT;
          gui_AddEvent( EVENT_MOUSE_DOWN, Widget, nil, @event.mouse_button );
        end;
      if mouse_Down( M_BMIDLE ) Then
        begin
          INC( mouseTimeDown );
          event.mouse_button := M_BMIDLE;
          gui_AddEvent( EVENT_MOUSE_DOWN, Widget, nil, @event.mouse_button );
        end;

      if mouse_Up( M_BLEFT ) Then
        begin
          mouseTimeDown := 0;
          event.mouse_button := M_BLEFT;
          gui_AddEvent( EVENT_MOUSE_UP, Widget, nil, @event.mouse_button );
        end;
      if mouse_Up( M_BRIGHT ) Then
        begin
          mouseTimeDown := 0;
          event.mouse_button := M_BRIGHT;
          gui_AddEvent( EVENT_MOUSE_UP, Widget, nil, @event.mouse_button );
        end;
      if mouse_Up( M_BMIDLE ) Then
        begin
          mouseTimeDown := 0;
          event.mouse_button := M_BMIDLE;
          gui_AddEvent( EVENT_MOUSE_UP, Widget, nil, @event.mouse_button );
        end;

      if mouse_Wheel( M_WUP ) Then
        begin
          event.mouse_wheel := M_WUP;
          gui_AddEvent( EVENT_MOUSE_WHEEL, Widget, nil, @event.mouse_wheel );
        end;
      if mouse_Wheel( M_WDOWN ) Then
        begin
          event.mouse_wheel := M_WDOWN;
          gui_AddEvent( EVENT_MOUSE_WHEEL, Widget, nil, @event.mouse_wheel );
        end;
    end else
      begin
        if Widget.MouseIn Then
          gui_AddEvent( EVENT_MOUSE_LEAVE, Widget, nil, nil );
        Widget.MouseIn := FALSE;
      end;

  if Widget.Focus Then
    begin
      if key_Last( KA_DOWN ) <> 0 Then
        begin
          Result := TRUE;
          event.key_code := key_Last( KA_DOWN );
          gui_AddEvent( EVENT_KEY_DOWN, Widget, nil, @event.key_code );
        end;
      if key_Last( KA_UP ) <> 0 Then
        begin
          Result := TRUE;
          event.key_code := key_Last( KA_UP );
          gui_AddEvent( EVENT_KEY_UP, Widget, nil, @event.key_code );
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
              Widget.Focus := TRUE;
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
              if Widget._id + 1 < Widget.parent.children Then
                begin
                  gui_AddEvent( EVENT_FOCUS_IN, Widget.parent.child[ Widget._id + 1 ], nil, nil );
                  Widget.parent.child[ Widget._id + 1 ].Focus := TRUE
                end;
            end;
          if Assigned( Widget.Events.OnKeyUp ) Then
            Widget.Events.OnKeyUp( Widget, key_code );
        end;
    end;
end;

procedure gui_ProcButton;
begin
  with Event^, zglTButtonDesc( Widget.Desc^ ) do
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
  with Event^, zglTCheckBoxDesc( Widget.Desc^ ) do
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
  with Event^, zglTRadioButtonDesc( Widget.Desc^ ) do
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
  with Event^, zglTEditBoxDesc( Widget.Desc^ ) do
    case _type of
      EVENT_FOCUS_IN:
        begin
          if not ReadOnly Then
            key_BeginReadText( Text, Max );
        end;
      EVENT_FOCUS_OUT:
        begin
          if not ReadOnly Then
            key_EndReadText();
        end;
      EVENT_KEY_DOWN:
        begin
          if not ReadOnly Then
            key_GetText( Text );
        end;
    end;
  gui_ProcEvents( Event );
end;

procedure gui_ProcListBox;
  var
    li     : Integer;
    iShift : Integer;
    iCount : Integer;
    scroll : zglPScrollBarDesc;
begin
  with Event^, zglTListBoxDesc( Widget.Desc^ ) do
    begin
      li := -1;
      case _type of
        EVENT_CREATE:
          begin
            gui_AddWidget( WIDGET_SCROLLBAR, Widget.Rect.W - SCROLL_SIZE, 0, SCROLL_SIZE, Widget.Rect.H, FALSE, TRUE, nil, nil, Widget, TRUE );
          end;
        EVENT_MOUSE_CLICK:
          begin
            if mouse_button = M_BLEFT Then
              begin
                scroll := Widget.part[ 0 ].Desc;
                if not Widget.part[ 0 ].Visible Then scroll.Position := 0;
                li := Round( mouse_Y - Widget.Rect.Y - 4 + scroll.Position ) div ItemHeight;
              end;
          end;
        EVENT_MOUSE_DOWN:
          begin
            if SelectMode = SELECT_BY_DOWN Then
              gui_AddEvent( EVENT_MOUSE_CLICK, Widget, nil, @Event.mouse_button );
          end;
        EVENT_MOUSE_WHEEL:
          begin
            if mouse_X < Widget.Rect.X + Widget.Rect.W - SCROLL_SIZE - 2 Then
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
          Widget.part[ 0 ].Visible := TRUE;
          scroll := Widget.part[ 0 ].Desc;
          iShift := scroll.Position div ItemHeight;

          if ( li < List.Count ) and ( li <> -1 ) and ( Event._type <> EVENT_MOUSE_DOWN ) and ( Event._type <> EVENT_MOUSE_CLICK ) Then
            begin
              if ItemIndex < iShift Then
                scroll.Position := ItemIndex * ItemHeight;
              if ItemIndex > iShift + iCount - 2 Then
                scroll.Position := ( ItemIndex - iCount + 1 ) * ItemHeight;
            end;

          scroll.Max      := List.Count * ItemHeight - Round( Widget.Client.H );
          scroll.PageSize := iCount * ItemHeight;
          scroll.Step     := ItemHeight;
          if scroll.Position > scroll.Max Then
            scroll.Position := scroll.Max;
        end else
          Widget.part[ 0 ].Visible := FALSE;
    end;
  gui_ProcEvents( Event );
end;

procedure gui_ProcComboBox;
  var
    li     : Integer;
    iShift : Integer;
    iCount : Integer;
    scroll : zglPScrollBarDesc;
begin
  with Event^, zglTComboBoxDesc( Widget.Desc^ ), Widget.Rect do
    begin
      li := -1;
      case _type of
        EVENT_CREATE:
          begin
            gui_AddWidget( WIDGET_SCROLLBAR, W - SCROLL_SIZE - 2, H, SCROLL_SIZE, DropDownCount * ItemHeight + 3, FALSE, TRUE, nil, nil, Widget, TRUE );
          end;
        EVENT_FOCUS_OUT:
          begin
            if Sender <> Widget.part[ 0 ] Then
              begin
                Widget.Modal := FALSE;
                DropedDown   := FALSE;
              end else
                Widget.Focus := TRUE;
          end;
        EVENT_MOUSE_CLICK:
          begin
            if DropedDown and col2d_PointInRect( mouse_X, mouse_Y, Widget.RectEx ) Then
              begin
                scroll := Widget.part[ 0 ].Desc;
                if not Widget.part[ 0 ].Visible Then scroll.Position := 0;
                li := Round( mouse_Y - Widget.Rect.Y - H - 4 + scroll.Position ) div ItemHeight;
                if mouse_X < Widget.Rect.X + Widget.Rect.W - SCROLL_SIZE - 2 Then
                  begin
                    Widget.Modal    := FALSE;
                    DropedDown      := FALSE;
                    scroll.Position := li * ItemHeight;
                  end;
              end;

            if col2d_PointInRect( mouse_X, mouse_Y, Widget.Rect ) Then
              begin
                Widget.Modal := not Widget.Modal;
                DropedDown   := not DropedDown;
              end;
          end;
        EVENT_MOUSE_WHEEL:
          begin
            if Widget.Focus and ( not DropedDown ) and col2d_PointInRect( mouse_X, mouse_Y, Widget.Rect ) Then
              begin
                li := ItemIndex;
                case Event.mouse_wheel of
                  M_WUP: if li > 0 Then DEC( li );
                  M_WDOWN: if li < List.Count Then INC( li );
                end;
              end;

            if mouse_X < Widget.Rect.X + Widget.Rect.W - SCROLL_SIZE - 2 Then
              gui_AddEvent( EVENT_MOUSE_WHEEL, Widget.part[ 0 ], Widget, @Event.mouse_wheel );
          end;
        EVENT_KEY_UP:
          begin
            if Widget.Focus Then
              begin
                li := ItemIndex;
                if key_code = K_UP   Then DEC( li );
                if key_code = K_DOWN Then INC( li );
              end;
          end;
      end;

      if DropedDown Then
        begin
          Widget.RectEx.X := X;
          Widget.RectEx.Y := Y + H;
          Widget.RectEx.W := W;
          Widget.RectEx.H := DropDownCount * ItemHeight + 4;
        end else
          Widget.RectEx := Widget.Rect;

      if ( li < List.Count ) and ( li <> -1 ) Then
        begin
          if ( ItemIndex <> li ) and Assigned( Widget.Events.OnChange ) Then
            Widget.Events.OnChange( Widget, li, li - ItemIndex );
          ItemIndex := li;
        end;

      iCount := gui_GetListItemsPerPage( Widget );
      if ( DropedDown ) and ( List.Count > iCount ) Then
        begin
          Widget.part[ 0 ].Visible := TRUE;
          scroll := Widget.part[ 0 ].Desc;
          iShift := scroll.Position div ItemHeight;

          if ( li < List.Count ) and ( li <> -1 ) and ( Event._type <> EVENT_MOUSE_DOWN ) and ( Event._type <> EVENT_MOUSE_CLICK ) Then
            begin
              if ItemIndex < iShift Then
                scroll.Position := ItemIndex * ItemHeight;
              if ItemIndex > iShift + iCount - 2 Then
                scroll.Position := ( ItemIndex - iCount + 1 ) * ItemHeight;
            end;

          scroll.Max      := ( List.Count - DropDownCount ) * ItemHeight;
          scroll.PageSize := iCount * ItemHeight;
          scroll.Step     := ItemHeight;
          if scroll.Position > scroll.Max Then
            scroll.Position := scroll.Max;
        end else
          Widget.part[ 0 ].Visible := FALSE;
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
  with Event^, Widget.Rect, zglTSpinDesc( Widget.Desc^ ) do
    case _type of
      EVENT_MOUSE_MOVE:
        begin
          if mouse_Y < Y + H / 2 Then dPressed := FALSE;
          if mouse_Y > Y + H / 2 Then uPressed := FALSE;
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
                  uPressed := TRUE;
                  if Value < Max Then
                    begin
                      INC( Value );
                      if Assigned( Widget.Events.OnChange ) Then
                        Widget.Events.OnChange( Widget, Value, 1 );
                    end;
                end;
              if mouse_Y > Y + H / 2 Then
                begin
                  dPressed := TRUE;
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
                uPressed := FALSE;
              if mouse_Y > Y + H / 2 Then
                dPressed := FALSE;
            end;
        end;
      EVENT_MOUSE_LEAVE:
        begin
          uPressed := FALSE;
          dPressed := FALSE;
        end;
    end;
  gui_ProcEvents( Event );
end;

procedure gui_ProcScrollBar;
  var
    r : zglTRect;
begin
  with Event^, Widget.Rect, zglTScrollBarDesc( Widget.Desc^ ) do
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
              sDraged := TRUE;
              if Kind = SCROLLBAR_VERTICAL Then
                mouseShiftY := mouse_Y - Round( Y + ( r.Y - Y ) )
              else
                mouseShiftX := mouse_X - Round( X + ( r.X - X ) );
            end;
        end;
      EVENT_DRAG_MOVE:
        begin
          if sDraged Then
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
          sDraged := FALSE;
        end;
      EVENT_MOUSE_MOVE:
        begin
          if Kind = SCROLLBAR_VERTICAL Then
            begin
              if mouse_Y > Y + SCROLL_SIZE     Then uPressed := FALSE;
              if mouse_Y < Y + H - SCROLL_SIZE Then dPressed := FALSE;
            end else
              begin
                if mouse_X > X + SCROLL_SIZE     Then uPressed := FALSE;
                if mouse_X < X + W - SCROLL_SIZE Then dPressed := FALSE;
              end;
        end;
      EVENT_MOUSE_LEAVE:
        begin
          uPressed := FALSE;
          dPressed := FALSE;
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
              uPressed := FALSE;
              dPressed := FALSE;
            end;
        end;
      EVENT_MOUSE_CLICK:
        begin
          r := gui_GetScrollRect( Widget );
          if ( mouse_button = M_BLEFT ) and ( not sDraged ) Then
            begin
              if Kind = SCROLLBAR_VERTICAL Then
                begin
                  if mouse_Y < Y + SCROLL_SIZE     Then uPressed := TRUE;
                  if mouse_Y > Y + H - SCROLL_SIZE Then dPressed := TRUE;
                end else
                  begin
                    if mouse_X < X + SCROLL_SIZE     Then uPressed := TRUE;
                    if mouse_X > X + W - SCROLL_SIZE Then dPressed := TRUE;
                  end;
              if uPressed Then
                gui_ScrollChange( Widget, -Step );

              if dPressed Then
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
