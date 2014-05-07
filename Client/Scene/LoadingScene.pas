unit LoadingScene;

interface
uses MondoZenGL,MZGui;
type
  TLoadingScene = class(TMZScene)
  private
    Texture:TMZTexture;
    lastTick:Cardinal;
  protected
     //在场景执行渲染之前执行
    procedure Startup; override;
     //在场景执行渲染之后执行
    procedure Shutdown; override;
    procedure RenderFrame;override;
  end;

var
I:integer;
implementation
uses ResManager,kpp,DrawEx,sysutils,classes,Texture,LoginScene,Share;
{ TLoadingScene }

procedure TLoadingScene.RenderFrame;
var
Percent:integer;
ShowText:string;
TextWidth:integer;
begin
  inherited;
  Percent:=TResManager.GetInstance.LoadPercent;
  ShowText:='正在加载资源...'+IntToStr(Percent)+'%';
  TextWidth:=MainFont.GetTextSize(PWideChar(ShowText)).cx;
  MainFont.Print(Application.ScreenWidth / 2-TextWidth div 2,Application.ScreenHeight/2,PWideChar(ShowText));
  if Percent = 100 then
  begin
    Application.SetScene(TLoginScene.create);
  end;
end;

procedure TLoadingScene.Shutdown;
begin
  inherited;

end;

procedure TLoadingScene.Startup;
begin
  inherited;
end;
end.
