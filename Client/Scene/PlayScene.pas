unit PlayScene;

interface
uses
GameScene,MondoZenGL,Mir2Map;
type
  TPlayScene=Class(TGameScene)
    Map:TMir2Map;
  protected
    X,Y:Integer;
    TimeTick:Cardinal;
    FPS:integer;
    FPSTick:Cardinal;
    nFPS:Integer;
    FPSLimite:Integer;
    ShowFps:Integer;
    procedure RenderFrame; override;
    procedure Startup; override;
    procedure Shutdown; override;
    procedure Update(const DeltaTimeMs: Double); override;
    End;
implementation
uses
Share,Windows,System.SysUtils;
{ TPlayScene }

procedure TPlayScene.RenderFrame;
begin
  inherited;
  Map.DrawTile(x,y);
  map.DrawObject(x,y);
  g_MainFont.TextOut(0,0,'FPS:'+inttostr(FPS));
end;

procedure TPlayScene.Shutdown;
begin
  inherited;
  Map.Free;
end;

procedure TPlayScene.Startup;
begin
  inherited;
  x:=333*48;
  Y:=325*32;
  Map:=TMir2Map.Create;
  Map.Scene:=Self;
  Map.SetViewSize(800,600);
  Map.LoadMap(g_sClientPath+'Map\0.map');
  TimeTick:=GettickCount;
  FPSTick:=GetTickCount;
  FPSLimite:=GetTickCount;
  ShowFps:=GetTickCount;
end;

procedure TPlayScene.Update(const DeltaTimeMs: Double);
begin
  inherited;
  if GetTickCount-FPSTick <= 1000 then
  begin
    INC(nFPS);
  end else
  begin
    FPSTick := GetTickCount;
    FPS := nFPS;
    nFPS:=0;

  end;

  if GetTickCount-TimeTick > 10 then
  begin
  //inc(Y,8);
  TimeTick:=GetTickCount;
  end;

  inc(y,2);

end;

end.
