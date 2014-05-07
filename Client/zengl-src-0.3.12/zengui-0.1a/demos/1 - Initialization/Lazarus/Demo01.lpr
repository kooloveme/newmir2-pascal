program Demo01;

{$MODE Delphi}

uses
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_utils,
  zgl_font,
  zglGui in '..\..\..\src\zglGui.pas';

var
  // Gui instance
  GUI: zglTGui;
  // Gui Skin
  gSkin: zglTGuiSkin;
  // Gui skin font
  fntMain: zglPFont;

procedure Init;
var frmSome: zglTForm;
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

  // CREATE SOME FORM HERE
  frmSome := zglTForm.Create(Gui, 10, 10, 400, 200, 'I am form', true);
  // Move it to the screen center
  frmSome.MoveToCenter;
  // Make it resiziable
  frmSome.CanResize := true;
  // And moveable
  frmSome.CanMove := true;
  // Add it to the gui that draws last one
  Gui.Items.Add(frmSome);

  // ADD TEXT TO FORM
  frmSome.Items.Add(zglTLabel.Create(Gui, 10, 10, 200, 20, 'And contain nothing.'));

  // Show some message at begin
  Gui.ShowMessage('Hello!', 'Welcome to the GUI DEMO 01!');
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

  wnd_SetCaption( 'Gui Demo 01' );
  wnd_ShowCursor( FALSE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, false, True );
  zgl_Init();
end.

