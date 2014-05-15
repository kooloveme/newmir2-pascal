unit PlayScene;

interface
uses
GameScene,MondoZenGL,Mir2Map;
type
  TPlayScene=Class(TGameScene)
    Map:TMir2Map;
  protected
    X,Y:Integer;
    procedure RenderFrame; override;
    procedure Startup; override;
    procedure Shutdown; override;
    procedure Update(const DeltaTimeMs: Double); override;
    End;
implementation
uses
Share;
{ TPlayScene }

procedure TPlayScene.RenderFrame;
begin
  inherited;
  Map.DrawTile(x*48,y*32);
  map.DrawObject(x*48,y*32);
end;

procedure TPlayScene.Shutdown;
begin
  inherited;
  Map.Free;
end;

procedure TPlayScene.Startup;
begin
  inherited;
  x:=330;
  Y:=320;
  Map:=TMir2Map.Create;
  Map.Scene:=Self;
  Map.SetViewSize(800,600);
  Map.LoadMap(g_sClientPath+'Map\3.map');
end;

procedure TPlayScene.Update(const DeltaTimeMs: Double);
begin
  inherited;
  //inc(x,2);
  //inc(y,2);

end;

end.
