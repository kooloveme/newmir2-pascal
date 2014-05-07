program Demo02;

{$MODE Delphi}

uses
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_utils,
  zgl_font,
  zgl_textures_png,
  zglGui in '..\..\..\src\zglGui.pas',
  u_components in '..\units\u_components.pas';

var
  // Gui instance
  GUI: zglTGui;
  // Gui Skin
  gSkin: zglTGuiSkin;
  // Gui skin font
  fntMain: zglPFont;

procedure Init;
var i: integer;
begin
  // Load font
  fntMain := font_LoadFromFile('Data/Main.zfi');
  // Create skin (file is a usual zip archive)
  gSkin := zglTGuiSkin.Create('Data/main.skin');
  // Creating an gui object (set skin, screen rect and
  //   default font(normal, active, disabled))
  Gui := zglTGui.Create(gSkin, 0, 0, 800, 600,
    zglTFontContainer.Create(
      zglTFontObject.Create(fntMain, 1, $000000, 255),
      zglTFontObject.Create(fntMain, 1, $101010, 255),
      zglTFontObject.Create(fntMain, 1, $555555, 128)
    ));

  // Init Gui file, created by editor
  InitGui(GUI);

  // FILL SOME VALUES

  // Select first radiobutton
  SomeRadioBox.Selected := RadioButton1;

  // Add some values to list
  SomeList.AddItem(zglTListItem.Create(SomeList, 'First', 16));
  SomeList.AddItem(zglTListItem.Create(SomeList, 'Second', 16));
  SomeList.AddItem(zglTListItem.Create(SomeList, 'Third', 16));

  // Add some columns to table
  SomeTable.AddHeader(zglTTableHeader.Create(SomeTable, 'First', 80, TEXT_RIGHT));
  SomeTable.AddHeader(zglTTableHeader.Create(SomeTable, 'Check!', 80));

  // Add some values to table
  for i := 0 to 99 do begin
    SomeTable.AddItem(zglTTableItem.Create(SomeTable).
      AddColumn(u_IntToStr(i) + '=').
      AddComponent(zglTCheckBox.Create(Gui, 0, 0, 10, 10, '')));
  end;

  // Add other values to SomeMultiButton

  SomeMultiButton.DropMenu.AddItem(zglTMenuItem.Create('One', nil));
  SomeMultiButton.DropMenu.AddItem(zglTMenuItem.Create('Two', nil));
  SomeMultiButton.DropMenu.AddItem(zglTMenuItem.Create('Three', nil));
end;

procedure Draw;
begin

  // SOME CODE

  // Draw gui (last)
  Gui.Draw;
  // and gui's mouse
  Gui.DrawMouse;
end;

procedure Update(dt: Double);
begin
  // Update Gui (first)
  Gui.Update(dt);

  // SOME CODE

end;

procedure Quit;
begin
  // Release gui
  Gui.Free;
end;

begin

  zgl_Reg( SYS_LOAD,   @Init );
  zgl_Reg( SYS_DRAW,   @Draw );
  zgl_Reg( SYS_UPDATE, @Update );
  zgl_Reg( SYS_EXIT,   @Quit );

  wnd_SetCaption( 'Gui Demo 02' );
  wnd_ShowCursor( FALSE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, false, True );
  zgl_Init();
end.
