unit Demo;

{$INCLUDE '../../src/mz_config.cfg'}

interface

procedure RunDemo;

implementation

uses
  SysUtils,
  { You don't necessarily have to use these units. However, Delphi will not
    inline some methods if these units are not in the uses clause. }
  {$IFNDEF FPC}
  {$IFDEF USE_ZENGL_STATIC}
  zgl_main,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  {$ENDIF}
  MondoZenGL;

type
  TDemoScene = class(TMZScene)
  private
    FTimer: TMZTimer;
    procedure TimerExpired(Sender: TObject);
  protected
    { Summary:
        Is called before the scene is executed. You can override this method
        to initialize scene specific resources. }
    procedure Startup; override;

    { Summary:
        Is called just before the scene is terminated. You can override this
        method to cleanup scene specific resources. }
    procedure Shutdown; override;
  end;

procedure RunDemo;
var
  Application: TMZApplication;
begin
  Application := TMZApplication.Create;
  Application.Options := Application.Options + [aoShowCursor];
  Application.Caption := '01 - Initialization';
  Application.ScreenWidth := 1024;
  Application.ScreenHeight := 768;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TDemoScene }

procedure TDemoScene.TimerExpired(Sender: TObject);
begin
  Application.Caption := TMZUtils.Format('01 - Initialization [%d FPS]',
    [Application.CurrentRenderFrameRate]);
end;

procedure TDemoScene.Startup;
begin
  inherited Startup;
  FTimer := TMZTimer.Create(TimerExpired, 1000);
end;

procedure TDemoScene.Shutdown;
begin
  FTimer.Free;
  inherited Shutdown;
end;

end.
