program GuiEditor;

{$R *.dres}

uses
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_utils,
  RegExpr in '..\units\RegExpr.pas',
  u_components in '..\units\u_components.pas',
  u_editor in '..\units\u_editor.pas',
  u_editorgui in '..\units\u_editorgui.pas',
  zglGui in '..\..\..\src\zglGui.pas',
  zgl_ini;

begin

  zgl_Reg( SYS_LOAD,   @Init );
  zgl_Reg( SYS_DRAW,   @Draw );
  zgl_Reg( SYS_UPDATE, @Update );
  zgl_Reg( SYS_EXIT,   @Quit );

  //zgl_Disable( APP_USE_LOG );

  ini_LoadFromFile('Config.ini');
  if ini_IsKey('Main', 'sX') then
    sX := ini_ReadKeyInt('Main', 'sX')
  else
    sX := sX_;
  if ini_IsKey('Main', 'sY') then
    sY := ini_ReadKeyInt('Main', 'sY')
  else
    sY := sY_;
  if ini_IsKey('Main', 'Fullscreen') then
    FullScreen := ini_ReadKeyBool('Main', 'Fullscreen')
  else
    FullScreen := FullScreen_;

  wnd_SetCaption( 'Gui Editor' );
  wnd_ShowCursor( FALSE );

  scr_SetOptions( sX, sY, REFRESH_MAXIMUM, FullScreen, True );
  zgl_Init();
end.
