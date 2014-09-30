unit Main;


interface

procedure Init;

implementation

uses
  SysUtils,ResManager,
  MondoZenGL,
  LoadingScene,
  GuiDesign,
  Share,
  GfxFont,
  SoundEngine,
  MZGui;


procedure Init;
var
  Application  :TMZApplication;
  GuiForm      :TfrmGuiDesgin;
begin
  {有斜杠 \ }
  g_sClientPath                   :=ExtractFilePath(ExtractFilePath(ParamStr(0)));
  g_nClientWidth                  :=800;
  g_nClientHeight                 :=600;
  GuiForm                         :=TfrmGuiDesgin.Create(nil);
  GuiForm.Show;
  Application                     := TMZApplication.Create;
  Application.Options             := Application.Options + [aoShowCursor]+[aoUseSound,aoVSync,aoUseInputEvents];
  Application.Caption             := '热血传奇';
  Application.ScreenWidth         := g_nClientWidth;
  Application.ScreenHeight        :=g_nClientHeight;
  Application.ScreenRefreshRate   :=60;
  g_MainFont                      :=TGfxFont.Create('宋体',12,false,false,False);
  Application.SetScene(TLoadingScene.create);

    //
  TResManager.GetInstance.Free;
    //程序结束释放GUI管理器
  TGuiManager.GetInstance.Free;

  //因为场景内是一个循环 当循环结束了也就表示程序结束了。所以需要再这里进行释放资源
  GuiForm.Free;
  g_MainFont.Free;
end;

end.
