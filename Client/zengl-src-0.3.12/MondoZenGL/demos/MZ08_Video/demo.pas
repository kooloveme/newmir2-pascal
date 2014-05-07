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
  zgl_sprite_2d,
  zgl_textures,
  zgl_video,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  {$ENDIF}
  MondoZenGL;

const
  {$IFDEF DARWIN}
  RESOURCE_DIRECTORY = '';
  {$ELSE}
  RESOURCE_DIRECTORY = '../data/';
  {$ENDIF}

type
  TDemoScene = class(TMZScene)
  private
    FVideo: TMZVideoStream;
  protected
    { Summary:
        Is called before the scene is executed. You can override this method
        to initialize scene specific resources. }
    procedure Startup; override;

    { Summary:
        Is called during each iteration of the main loop to update the game
        state. The DeltaTimeMs is the number of milliseconds (1/1000th of a
        second) that has passed since the last call to Update.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update. }
    procedure Update(const DeltaTimeMs: Double); override;

    { Summary:
        Is called during each iteration of the main loop to render the current
        frame. }
    procedure RenderFrame; override;

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
  Application.Caption := '08 - Video';
  Application.ScreenWidth := 1024;
  Application.ScreenHeight := 600;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TDemoScene }

procedure TDemoScene.Startup;
begin
  inherited Startup;
  FVideo := TMZVideoStream.Create(RESOURCE_DIRECTORY + 'MondoZenGL.ogv');
end;

procedure TDemoScene.Update(const DeltaTimeMs: Double);
begin
  inherited;
  FVideo.Update(DeltaTimeMs, false);
end;

procedure TDemoScene.RenderFrame;
begin
  inherited;
  Canvas.DrawSprite(FVideo.Texture, 0, 0, 1024, 768);
end;

procedure TDemoScene.Shutdown;
begin
  FVideo.Free;
  inherited Shutdown;
end;

end.
