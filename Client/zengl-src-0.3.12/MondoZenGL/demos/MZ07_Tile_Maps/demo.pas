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
  zgl_textures,
  zgl_file,
  zgl_keyboard,
  zgl_mouse,
  zgl_text,
  zgl_main,
  zgl_utils,
  zgl_font,
  zgl_touch,
  zgl_collision_2d,
  zgl_primitives_2d,
  zgl_math_2d,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  {$ENDIF}
  MondoZenGL,
  mzTileMaps;

const
  SCREEN_WIDTH  = 1024;
  SCREEN_HEIGHT = 768;
  {$IFDEF DARWIN}
  RESOURCE_DIRECTORY = '';
  {$ELSE}
  RESOURCE_DIRECTORY = '../data/';
  {$ENDIF}

type
  TSimpleButton = class
  private
    FCanvas: TMZCanvas;
    FFont: TMZFont;
    FCaption: UTF8String;
    FBounds: TMZRect;
    FChecked: Boolean;
  public
    constructor Create(const Canvas: TMZCanvas; const Font: TMZFont;
      const Caption: UTF8String; const X, Y: Single);
    procedure Render;
    function IsClicked: Boolean;

    property Bounds: TMZRect read FBounds;
    property Checked: Boolean read FChecked write FChecked;
  end;

type
  TDemoScene = class(TMZScene)
  private
    FFont: TMZFont;
    FMap: TMZTileMap;
    FScrolling: Boolean;
    FMouseX: Integer;
    FMouseY: Integer;
    FButtonZelda: TSimpleButton;
    FButtonDesert: TSimpleButton;
    FButtonSewers: TSimpleButton;
    FButtonMap01: TSimpleButton;
    FMapIndex: Integer;
  private
    procedure LoadMap(const Filename: UTF8String);
  protected
    { Summary:
        Is called before the scene is executed. You can override this method
        to initialize scene specific resources. }
    procedure Startup; override;

    { Summary:
        Is called just before the scene is terminated. You can override this
        method to cleanup scene specific resources. }
    procedure Shutdown; override;

    { Summary:
        Is called during each iteration of the main loop to render the current
        frame. }
    procedure RenderFrame; override;

    { Summary:
        Is called during each iteration of the main loop to update the game
        state. The DeltaTimeMs is the number of milliseconds (1/1000th of a
        second) that has passed since the last calld to Update.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update. }
    procedure Update(const DeltaTimeMs: Double); override;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time the mouse moves.
        Does nothing by default.
      Parameters:
        X: The mouse X-coordinate.
        Y: The mouse Y-coordinate. }
    procedure MouseMove(const X, Y: Integer); override;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a mouse button is pressed.
        Does nothing by default.
      Parameters:
        Button: The mouse button that is pressed. }
    procedure MouseDown(const Button: TMZMouseButton); override;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a mouse button is released.
        Does nothing by default.
      Parameters:
        Button: The mouse button that is release. }
    procedure MouseUp(const Button: TMZMouseButton); override;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a finger moves over the touch screen.
        Does nothing by default.
      Parameters:
        Finger: The finger ID.
        X: The touch X-coordinate.
        Y: The touch Y-coordinate.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure TouchMove(const Finger, X, Y: Integer); override;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a finger is pressed onto the touch screen.
        Does nothing by default.
      Parameters:
        Finger: The finger ID.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure TouchDown(const Finger: Integer); override;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a finger is released from the touch screen.
        Does nothing by default.
      Parameters:
        Finger: The finger ID.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure TouchUp(const Finger: Integer); override;
end;

const
  MAPS: array [0..3] of UTF8String = ('zelda/zelda.mztm', 'desert/desert.mztm',
    'sewers/sewers.mztm', 'map01/Map01.mztm');

procedure RunDemo;
var
  Application: TMZApplication;
begin
  Randomize;
  Application := TMZApplication.Create;
  Application.Options := Application.Options
    + [aoShowCursor, aoUseSound, aoUseInputEvents]
    - [aoAllowPortraitOrientation];
  Application.Caption := 'MZ07 - Tile Maps';
  Application.ScreenWidth := SCREEN_WIDTH;
  Application.ScreenHeight := SCREEN_HEIGHT;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TDemoScene }

procedure TDemoScene.Startup;
begin
  inherited Startup;
  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'font.zfi');
  TMZZipArchive.OpenArchive(RESOURCE_DIRECTORY + 'maps.zip');
  FButtonZelda := TSimpleButton.Create(Canvas, FFont, 'Zelda', 100, 4);
  FButtonZelda.Checked := True;
  FButtonDesert := TSimpleButton.Create(Canvas, FFont, 'Desert (Tiled)',
    FButtonZelda.Bounds.X + FButtonZelda.Bounds.W + 4, 4);
  FButtonSewers := TSimpleButton.Create(Canvas, FFont, 'Sewers (Tiled)',
    FButtonDesert.Bounds.X + FButtonDesert.Bounds.W + 4, 4);
  FButtonMap01 := TSimpleButton.Create(Canvas, FFont, 'Map01 (tIDE)',
    FButtonSewers.Bounds.X + FButtonSewers.Bounds.W + 4, 4);
  LoadMap(MAPS[0]);
end;

procedure TDemoScene.Update(const DeltaTimeMs: Double);
var
  Index: Integer;
begin
  inherited;
  Index := -1;
  if (FButtonZelda.IsClicked) then
    Index := 0
  else if (FButtonDesert.IsClicked) then
    Index := 1
  else if (FButtonSewers.IsClicked) then
    Index := 2
  else if (FButtonMap01.IsClicked) then
    Index := 3;
  if (Index >= 0) then
  begin
    FButtonZelda.Checked := (Index = 0);
    FButtonDesert.Checked := (Index = 1);
    FButtonSewers.Checked := (Index = 2);
    FButtonMap01.Checked := (Index = 3);
    if (Index <> FMapIndex) then
    begin
      FMapIndex := Index;
      LoadMap(MAPS[Index]);
    end;
  end;

  FMap.Update(DeltaTimeMs);
  if TMZKeyboard.IsKeyPressed(kcEscape) then
    Application.Quit;
  TMZKeyboard.ClearState;
  TMZMouse.ClearState;
end;

procedure TDemoScene.Shutdown;
begin
  TMZZipArchive.CloseArchive;
  FMap.Free;
  FFont.Free;
  FButtonZelda.Free;
  FButtonDesert.Free;
  FButtonSewers.Free;
  FButtonMap01.Free;
  inherited Shutdown;
end;

procedure TDemoScene.LoadMap(const Filename: UTF8String);
begin
  FreeAndNil(FMap);
  FMap := TMZTileMap.Create(Filename);

  { View the top-left corner of the map instead of the center }
  FMap.ViewCenterX := SCREEN_WIDTH div 2;
  FMap.ViewCenterY := SCREEN_HEIGHT div 2;
end;

{ The Mouse* events fire on Windows and Mac OS X }

procedure TDemoScene.MouseDown(const Button: TMZMouseButton);
begin
  inherited;
  FScrolling := True;
  FMouseX := TMZMouse.X;
  FMouseY := TMZMouse.Y;
end;

procedure TDemoScene.MouseMove(const X, Y: Integer);
begin
  inherited;
  if (FScrolling) then
  begin
    FMap.ViewCenterX := FMap.ViewCenterX - (TMZMouse.X - FMouseX);
    FMap.ViewCenterY := FMap.ViewCenterY - (TMZMouse.Y - FMouseY);
    FMouseX := TMZMouse.X;
    FMouseY := TMZMouse.Y;
  end;
end;

procedure TDemoScene.MouseUp(const Button: TMZMouseButton);
begin
  inherited;
  FScrolling := False;
end;

{ The Touch* events fire on iOS }

procedure TDemoScene.TouchDown(const Finger: Integer);
begin
  inherited;
  FScrolling := True;
  {$IF DEFINED(iOS) or DEFINED(ANDROID)}
  FMouseX := TMZTouch.X[0];
  FMouseY := TMZTouch.Y[0];
  {$IFEND}
end;

procedure TDemoScene.TouchMove(const Finger, X, Y: Integer);
begin
  inherited;
  {$IF DEFINED(iOS) or DEFINED(ANDROID)}
  if (FScrolling) then
  begin
    FMap.ViewCenterX := FMap.ViewCenterX - (TMZTouch.X[0] - FMouseX);
    FMap.ViewCenterY := FMap.ViewCenterY - (TMZTouch.Y[0] - FMouseY);
    FMouseX := TMZTouch.X[0];
    FMouseY := TMZTouch.Y[0];
  end;
  {$IFEND}
end;

procedure TDemoScene.TouchUp(const Finger: Integer);
begin
  inherited;
  FScrolling := False;
end;

procedure TDemoScene.RenderFrame;
begin
  inherited RenderFrame;
  FMap.Render;
  FButtonZelda.Render;
  FButtonDesert.Render;
  FButtonSewers.Render;
  FButtonMap01.Render;
  Canvas.DrawText(FFont, 2, 8, 'Choose map:');
  Canvas.DrawText(FFont, 2, 28, 'FPS: ' + TMZUtils.IntToStr(Application.CurrentRenderFrameRate));
  Canvas.DrawText(FFont, 2, 48, 'Drag the map to scroll');
end;

{ TSimpleButton }

constructor TSimpleButton.Create(const Canvas: TMZCanvas; const Font: TMZFont;
  const Caption: UTF8String; const X, Y: Single);
begin
  inherited Create;
  FCanvas := Canvas;
  FFont := Font;
  FCaption := Caption;
  FBounds.X := X;
  FBounds.Y := Y;
  FBounds.W := FCanvas.CalculateTextWidth(FFont, FCaption) + 8;
  FBounds.H := FCanvas.CalculateTextHeight(FFont, FBounds.W, FCaption) + 8;
end;

function TSimpleButton.IsClicked: Boolean;
var
  P: TMZPoint;
begin
  P := TMZMouse.Position;
  Result := TMZMouse.IsButtonClicked(mbLeft);

  if (Result) then
    Result := FBounds.ContainsPoint(P);
end;

procedure TSimpleButton.Render;
begin
  if (FChecked) then
  begin
    FCanvas.FillRect(FBounds, $15428B);
    FCanvas.DrawText(FFont, FBounds, FCaption, [tfHAlignCenter, tfVAlignCenter]);
  end
  else
  begin
    FCanvas.FillRect(FBounds, $CBDEF4);
    FCanvas.DrawRect(FBounds, $15428B);
    FCanvas.DrawText(FFont, FBounds, 1, 0, FCaption, $FF, $15428B, [tfHAlignCenter, tfVAlignCenter]);
  end;
end;

end.
