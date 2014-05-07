unit Demo;

{$INCLUDE '../../src/mz_config.cfg'}

{ This demo shows a couple of new concepts:

  1. We derived our own TDemoApplication class from TMZApplication. We use
     this class to load resources (a font) that is shared by different scenes.
  2. We have 3 different scenes:
      -TStartupScene is the initial scene that shows a menu to start a differnt
       scene (TScene1 or TScene2). When you press Esc in this scene, you quit
       the application.
      -TScene1 and TScene2 are different scenes that are started from
       TStartupScene. When you press Esc, you go back to the TStartupScene.
  3. TStartupScene has the AutoFree property set to False. This means that this
     scene is available during the entire duration of the application. We need
     to free it manually when the application ends.
  4. TScene1 and TScene2 have AutoFree set to True. These scenes will
     automatically be freed when a new scene is started. }

interface

procedure RunDemo;

implementation

uses
  SysUtils,
  { You don't necessarily have to use these units. However, Delphi will not
    inline some methods if these units are not in the uses clause. }
  {$IFNDEF FPC}
  {$IFDEF USE_ZENGL_STATIC}
  zgl_text,
  zgl_font,
  zgl_keyboard,
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
  TDemoApplication = class(TMZApplication)
  private
    FFont: TMZFont;
    FStartupScene: TMZScene;
  protected
    { Summary:
        Performs application startup. This method is called <b>after</b> the
        window and OpenGL contexts have been created. You can override this
        method to initialize the application and load application resources. }
    procedure Startup; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;

    property Font: TMZFont read FFont;
    property StartupScene: TMZScene read FStartupScene;
  end;

type
  TStartupScene = class(TMZScene)
  protected
    { Summary:
        Is called during each iteration of the main loop to render the current
        frame. }
    procedure RenderFrame; override;

    { Summary:
        Is called during each iteration of the main loop to update the game
        state. The DeltaTimeMs is the number of milliseconds (1/1000th of a
        second) that has passed since the last call to Update.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update. }
    procedure Update(const DeltaTimeMs: Double); override;
  end;

type
  TScene1 = class(TMZScene)
  protected
    procedure RenderFrame; override;
    procedure Update(const DeltaTimeMs: Double); override;
  end;

type
  TScene2 = class(TMZScene)
  protected
    procedure RenderFrame; override;
    procedure Update(const DeltaTimeMs: Double); override;
  end;

procedure RunDemo;
var
  App: TDemoApplication;
begin
  App := TDemoApplication.Create;
  App.Run;
  { The application will automatically be freed on shutdown }
end;

{ TDemoApplication }

procedure TDemoApplication.Startup;
begin
  { You cannot load the font (or any other resources) in TDemoApplication.Create
    because the OpenGL system has not been initialized yet at that point.
    You should load any application-wide resources in the Startup method.
    Don't forget to call the inherited Startup! }
  inherited Startup;
  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'font.zfi' );
end;

constructor TDemoApplication.Create;
begin
  inherited Create;
  Options := Options + [aoShowCursor];
  Caption := 'MZ01 - Multiple Scenes';
  ScreenWidth := 1024;
  ScreenHeight := 768;

  { Create the startup scene. We set AutoFree to False to keep it around for
    the duration of the application. }
  FStartupScene := TStartupScene.Create;
  FStartupScene.AutoFree := False;
end;

destructor TDemoApplication.Destroy;
begin
  FStartupScene.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TDemoApplication.Run;
begin
  SetScene(FStartupScene);
end;

{ TStartupScene }

procedure TStartupScene.RenderFrame;
var
  Font: TMZFont;
begin
  inherited RenderFrame;
  Font := (Application as TDemoApplication).Font;
  Canvas.DrawText(Font, 10, 10, 'This is TStartupScene');
  {$IFDEF iOS}
  Canvas.DrawText(Font, 10, 40, 'Tap left side of screen to switch to TScene1');
  Canvas.DrawText(Font, 10, 70, 'Tap right side of screen to switch to TScene2');
  {$ELSE}
  Canvas.DrawText(Font, 10, 40, 'Press "1" to switch to TScene1');
  Canvas.DrawText(Font, 10, 70, 'Press "2" to switch to TScene2');
  Canvas.DrawText(Font, 10, 100, 'Press "Esc" to quit');
  {$ENDIF}
end;

procedure TStartupScene.Update(const DeltaTimeMs: Double);
begin
  inherited Update(DeltaTimeMs);

  {$IFDEF iOS}      
  if TMZTouch.IsFingerTapped(0) then
  begin
    if (TMZTouch.X[0] < 512) then
      { Switch to TScene1. TScene1 has AutoFree set to True by default, so we
        don't need to keep track of it. }
      Application.SetScene(TScene1.Create)
    else      
      { Switch to TScene2. TScene2 has AutoFree set to True by default, so we
        don't need to keep track of it. }
          Application.SetScene(TScene2.Create);
  end;

  TMZTouch.ClearState;
  {$ELSE}
  if TMZKeyboard.IsKeyPressed(kc1) or TMZKeyboard.IsKeyPressed(kcKP1) then
    { Switch to TScene1. TScene1 has AutoFree set to True by default, so we
      don't need to keep track of it. }
    Application.SetScene(TScene1.Create)
  else if TMZKeyboard.IsKeyPressed(kc2) or TMZKeyboard.IsKeyPressed(kcKP2) then
    { Switch to TScene2. TScene2 has AutoFree set to True by default, so we
      don't need to keep track of it. }
    Application.SetScene(TScene2.Create)
  else if TMZKeyboard.IsKeyPressed(kcEscape) then
    Application.Quit;

  TMZKeyboard.ClearState;
  {$ENDIF}
end;

{ TScene1 }

procedure TScene1.RenderFrame;
var
  Font: TMZFont;
begin
  inherited RenderFrame;
  Font := (Application as TDemoApplication).Font;
  Canvas.DrawText(Font, 10, 130, 'This is TScene1');
  {$IFDEF iOS}
  Canvas.DrawText(Font, 10, 160, 'Tap the screen to return to the startup scene');
  {$ELSE}
  Canvas.DrawText(Font, 10, 160, 'Press "Esc" to return to the startup scene');
  {$ENDIF}
end;

procedure TScene1.Update(const DeltaTimeMs: Double);
begin
  inherited Update(DeltaTimeMs);
  {$IFDEF iOS}
  if TMZTouch.IsFingerTapped(0) then
    { Go back to the startup scene }
    Application.SetScene((Application as TDemoApplication).StartupScene);
  TMZTouch.ClearState;
  {$ELSE}
  if TMZKeyboard.IsKeyPressed(kcEscape) then
    { Go back to the startup scene }
    Application.SetScene((Application as TDemoApplication).StartupScene);
  TMZKeyboard.ClearState;
  {$ENDIF}
end;

{ TScene2 }

procedure TScene2.RenderFrame;
var
  Font: TMZFont;
begin
  inherited RenderFrame;
  Font := (Application as TDemoApplication).Font;
  Canvas.DrawText(Font, 200, 130, 'This is TScene2');
  {$IFDEF iOS}
  Canvas.DrawText(Font, 200, 160, 'Tap the screen to return to the startup scene');
  {$ELSE}
  Canvas.DrawText(Font, 200, 160, 'Press "Esc" to return to the startup scene');
  {$ENDIF}
end;

procedure TScene2.Update(const DeltaTimeMs: Double);
begin
  inherited Update(DeltaTimeMs);
  {$IFDEF iOS}
  if TMZTouch.IsFingerTapped(0) then
    { Go back to the startup scene }
    Application.SetScene((Application as TDemoApplication).StartupScene);
  TMZTouch.ClearState;
  {$ELSE}
  if TMZKeyboard.IsKeyPressed(kcEscape) then
    { Go back to the startup scene }
    Application.SetScene((Application as TDemoApplication).StartupScene);
  TMZKeyboard.ClearState;
  {$ENDIF}
end;

end.
