unit uUI;

interface
uses
  zgl_main,
  zgl_gui_main,
  zgl_gui_types,
  zgl_font,
  zgl_font_gen,
  zgl_text,
  zgl_utils;

procedure ui_Init;

procedure regen;

procedure cmb_fonts_change( Widget : zglPWidget; const Value, Change : Integer );
procedure cmb_tsize_change( Widget : zglPWidget; const Value, Change : Integer );
procedure sn_fsize_change( Widget : zglPWidget; const Value, Change : Integer );
procedure sn_cpage_change( Widget : zglPWidget; const Value, Change : Integer );
procedure cb_aa_click( Widget : zglPWidget );
procedure cb_bold_click( Widget : zglPWidget );
procedure cb_italic_click( Widget : zglPWidget );
procedure cb_pack_click( Widget : zglPWidget );
procedure bn_save_click( Widget : zglPWidget );
procedure bn_exit_click( Widget : zglPWidget );

var
  ui_font : zglPFont;

  gb_tools  : zglPWidget;
  cmb_fonts : zglPWidget;
  sn_fsize  : zglPWidget;
  eb_fsize  : zglPWidget;
  sn_tsize  : zglPWidget;
  eb_tsize  : zglPWidget;
  sn_cpage  : zglPWidget;
  eb_cpage  : zglPWidget;
  cb_aa     : zglPWidget;
  cb_italic : zglPWidget;
  cb_bold   : zglPWidget;
  cb_pack   : zglPWidget;
  bn_save   : zglPWidget;
  bn_exit   : zglPWidget;

  font_id : Integer;

implementation

procedure ui_Init;
  var
    i        : Integer;
    wlabel   : zglTLabelDesc;
    editbox  : zglTEditBoxDesc;
    groupbox : zglTGroupBoxDesc;
    spin     : zglTSpinDesc;
    checkbox : zglTCheckBoxDesc;
    button   : zglTButtonDesc;
    combobox : zglTComboBoxDesc;
begin
  ui_font := font_Add;
  // English
  for i := 32 to 126 do
    begin
      fg_CharsUse[ i ] := TRUE;
      INC( ui_font.Count.Chars );
    end;
  fg_FontSize   := 10;
  fg_FontBold   := FALSE;
  fg_FontItalic := FALSE;
  fg_FontAA     := TRUE;
  fg_PageSize   := 512;
  fontgen_BuildFont( ui_font, 'Monospace' );
  fg_FontSize   := 16;

  // Tools
  FillChar( groupbox, SizeOf( groupbox ), 0 );
  groupbox.Font    := ui_font;
  groupbox.Caption := '';
  gb_tools         := gui_AddWidget( WIDGET_GROUPBOX, 800 - 250, 0, 250, 600, FALSE, TRUE, @groupbox, nil, nil );

  // Font ComboBox
  FillChar( combobox, SizeOf( combobox ), 0 );
  combobox.Font := ui_font;
  combobox.List.Count := fg_FontList.Count;
  SetLength( combobox.List.Items, combobox.List.Count );
  for i := 0 to combobox.List.Count - 1 do
    combobox.List.Items[ i ] := fg_FontList.Items[ i ];
  combobox.DropDownCount := 8;
  combobox.ItemIndex     := 0;
  cmb_fonts := gui_AddWidget( WIDGET_COMBOBOX, 10, 10, 230, ui_font.MaxHeight + 6, FALSE, TRUE, @combobox, nil, gb_tools );
  cmb_fonts.Events.OnChange:= @cmb_fonts_change;

  // CheckBox Antialias
  FillChar( checkbox, SizeOf( checkbox ), 0 );
  checkbox.Font    := ui_font;
  checkbox.Caption := 'Antialiasing';
  checkbox.Checked := fg_FontAA;
  cb_aa            := gui_AddWidget( WIDGET_CHECKBOX, 10, 40, 15, 15, FALSE, TRUE, @checkbox, nil, gb_tools );
  cb_aa.Events.OnClick := @cb_aa_click;

  // CheckBox Bold
  FillChar( checkbox, SizeOf( checkbox ), 0 );
  checkbox.Font    := ui_font;
  checkbox.Caption := 'Bold';
  checkbox.Checked := fg_FontBold;
  cb_bold          := gui_AddWidget( WIDGET_CHECKBOX, 10, 65, 15, 15, FALSE, TRUE, @checkbox, nil, gb_tools );
  cb_bold.Events.OnClick := @cb_bold_click;

  // CheckBox Italic
  FillChar( checkbox, SizeOf( checkbox ), 0 );
  checkbox.Font    := ui_font;
  checkbox.Caption := 'Italic';
  checkbox.Checked := fg_FontItalic;
  cb_italic        := gui_AddWidget( WIDGET_CHECKBOX, 10, 90, 15, 15, FALSE, TRUE, @checkbox, nil, gb_tools );
  cb_italic.Events.OnClick := @cb_italic_click;

  // CheckBox Pack
  i := Round( text_GetWidth( ui_font, 'Pack' ) );
  FillChar( checkbox, SizeOf( checkbox ), 0 );
  checkbox.Font    := ui_font;
  checkbox.Caption := 'Pack';
  checkbox.Checked := fg_FontPack;
  cb_italic        := gui_AddWidget( WIDGET_CHECKBOX, 250 - i - 32, 90, 15, 15, FALSE, TRUE, @checkbox, nil, gb_tools );
  cb_italic.Events.OnClick := @cb_pack_click;

  i := Round( text_GetWidth( ui_font, 'Page Size: ' ) );
  // ComboBox TextureSize
  FillChar( combobox, SizeOf( combobox ), 0 );
  combobox.Font := ui_font;
  combobox.List.Count := 5;
  SetLength( combobox.List.Items, combobox.List.Count );
  combobox.List.Items[ 0 ] := '128';
  combobox.List.Items[ 1 ] := '256';
  combobox.List.Items[ 2 ] := '512';
  combobox.List.Items[ 3 ] := '1024';
  combobox.List.Items[ 4 ] := '2048';
  combobox.DropDownCount := 5;
  combobox.ItemIndex     := 2;
  cmb_fonts := gui_AddWidget( WIDGET_COMBOBOX, i + 10, 115, 250 - i - 20, ui_font.MaxHeight + 6, FALSE, TRUE, @combobox, nil, gb_tools );
  cmb_fonts.Events.OnChange:= @cmb_tsize_change;

  FillChar( wlabel, SizeOf( wlabel ), 0 );
  wlabel.Font    := ui_font;
  wlabel.Caption := 'Page Size: ';
  gui_AddWidget( WIDGET_LABEL, 10, 115 + ( 24 - ui_font.MaxHeight ) div 2, 110, 24, TRUE, TRUE, @wlabel, nil, gb_tools );


  i := Round( text_GetWidth( ui_font, 'Font Size: ' ) );
  // EditBox FontSize
  FillChar( editbox, SizeOf( editbox ), 0 );
  editbox.Font     := ui_font;
  editbox.Text     := u_IntToStr( fg_FontSize );
  editbox.Max      := 1;
  editbox.ReadOnly := TRUE;
  eb_fsize         := gui_AddWidget( WIDGET_EDITBOX, i + 10, 150, 250 - i - 20 - 12, 24, TRUE, TRUE, @editbox, nil, gb_tools );

  // Spin FintSize
  FillChar( spin, SizeOf( spin ), 0 );
  spin.Value    := fg_FontSize;
  spin.Max      := 76;
  spin.Min      := 8;
  spin.UPressed := FALSE;
  spin.DPressed := FALSE;
  sn_fsize      := gui_AddWidget( WIDGET_SPIN, i + ( 250 - i - 20 - 12 ) + 10, 150, 12, 24, FALSE, TRUE, @spin, nil, gb_tools );
  sn_fsize.Events.OnChange := @sn_fsize_change;

  FillChar( wlabel, SizeOf( wlabel ), 0 );
  wlabel.Font    := ui_font;
  wlabel.Caption := 'Font Size: ';
  gui_AddWidget( WIDGET_LABEL, 10, 150 + ( 24 - ui_font.MaxHeight ) div 2, 110, 24, TRUE, TRUE, @wlabel, nil, gb_tools );


  i := Round( text_GetWidth( ui_font, 'Current Page: ' ) );
  // EditBox CurrentPage
  FillChar( editbox, SizeOf( editbox ), 0 );
  editbox.Font     := ui_font;
  editbox.Text     := '1';
  editbox.Max      := 1;
  editbox.ReadOnly := TRUE;
  eb_cpage         := gui_AddWidget( WIDGET_EDITBOX, i + 10, 185, 250 - i - 20 - 12, 24, TRUE, TRUE, @editbox, nil, gb_tools );

  // Spin CurrentPage
  FillChar( spin, SizeOf( spin ), 0 );
  spin.Value    := 1;
  spin.Max      := 100;
  spin.Min      := 1;
  spin.UPressed := FALSE;
  spin.DPressed := FALSE;
  sn_cpage      := gui_AddWidget( WIDGET_SPIN, i + ( 250 - i - 20 - 12 ) + 10, 185, 12, 24, FALSE, TRUE, @spin, nil, gb_tools );
  sn_cpage.Events.OnChange := @sn_cpage_change;

  FillChar( wlabel, SizeOf( wlabel ), 0 );
  wlabel.Font    := ui_font;
  wlabel.Caption := 'Current Page: ';
  gui_AddWidget( WIDGET_LABEL, 10, 185 + ( 24 - ui_font.MaxHeight ) div 2, 110, 24, TRUE, TRUE, @wlabel, nil, gb_tools );


  // Button Save
  FillChar( button, SizeOf( button ), 0 );
  button.Font    := ui_font;
  button.Caption := 'Save font';
  button.Pressed := FALSE;
  bn_save        := gui_AddWidget( WIDGET_BUTTON, 10, 600 - 40 - 10, 110, 40, FALSE, TRUE, @button, nil, gb_tools );
  bn_save.Events.OnClick := @bn_save_click;

  // Button Exit
  FillChar( button, SizeOf( button ), 0 );
  button.Font    := ui_font;
  button.Caption := 'Exit';
  button.Pressed := FALSE;
  bn_exit        := gui_AddWidget( WIDGET_BUTTON, 250 - 110 - 10, 600 - 40 - 10, 110, 40, FALSE, TRUE, @button, nil, gb_tools );
  bn_exit.Events.OnClick := @bn_exit_click;
end;

procedure regen;
begin
  fontgen_BuildFont( fg_Font, fg_FontList.Items[ font_id ] );
end;

procedure cmb_fonts_change;
begin
  font_id := Value;
  regen;
end;

procedure cmb_tsize_change;
begin
  fg_PageSize := u_StrToInt( zglTComboBoxDesc( Widget.desc^ ).List.Items[ Value ] );
  regen;
end;

procedure sn_fsize_change;
begin
  zglTEditBoxDesc( eb_fsize.desc^ ).Text := u_IntToStr( Value );
  fg_FontSize := Value;
  regen;
end;

procedure sn_cpage_change;
begin
  if zglPSpinDesc( Widget.desc ).Value > fg_Font.Count.Pages Then
    zglPSpinDesc( Widget.desc ).Value := fg_Font.Count.Pages;
  zglTEditBoxDesc( eb_cpage.desc^ ).Text := u_IntToStr( zglPSpinDesc( Widget.desc ).Value );
end;

procedure cb_aa_click;
begin
  fg_FontAA := zglTCheckBoxDesc( Widget.desc^ ).Checked;
  regen;
end;

procedure cb_bold_click;
begin
  fg_FontBold := zglTCheckBoxDesc( Widget.desc^ ).Checked;
  regen;
end;

procedure cb_italic_click;
begin
  fg_FontItalic := zglTCheckBoxDesc( Widget.desc^ ).Checked;
  regen;
end;

procedure cb_pack_click;
begin
  fg_FontPack := zglTCheckBoxDesc( Widget.desc^ ).Checked;
  regen;
end;

procedure bn_save_click;
begin
  fontgen_SaveFont( fg_Font, fg_FontList.Items[ font_id ] + '-' + u_IntToStr( fg_FontSize ) + 'pt' );
end;

procedure bn_exit_click;
begin
  zgl_Exit;
end;

end.
