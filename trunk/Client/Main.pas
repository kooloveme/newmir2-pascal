unit Main;


interface

procedure Init;

implementation

uses
  SysUtils,ResManager,PngImage,
  zgl_main,
  MondoZenGL,
  LoadingScene,
  LoginScene,
  GuiDesign,
  Share,GfxFont;


procedure Init;
var
  Application: TMZApplication;
  GuiForm:TfrmGuiDesgin;
begin
  ClientPath:=ExtractFilePath(ExtractFilePath(ParamStr(0)));
  ClientWidth:=800;
  ClientHeight:=600;
  GuiForm:=TfrmGuiDesgin.Create(nil);
  GuiForm.Show;
  Application := TMZApplication.Create;
  Application.Options := Application.Options + [aoShowCursor]+[aoUseSound,aoVSync,aoUseInputEvents];
  Application.Caption := 'ÈÈÑª´«Ææ';
  Application.ScreenWidth := ClientWidth;
  Application.ScreenHeight := ClientHeight;
  Application.ScreenRefreshRate:=60;
  MainFont:=TGfxFont.Create('ËÎÌå',12,false,false,False);
  Application.SetScene(TLoadingScene.create);
  GuiForm.Free;

end;



end.
