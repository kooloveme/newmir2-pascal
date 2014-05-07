unit Demo;

{$INCLUDE '../../src/mz_config.cfg'}

{ This demo shows event based input processing, instead of polling the mouse or
  keyboard directly. This works by overriding various event methods in TMZScene
  (like MouseDown, KeyUp etc). }

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
  zgl_render_2d,
  zgl_keyboard,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  {$ENDIF}
  MondoZenGL;

const
  MAX_EVENTS = 600 div 20;
  {$IFDEF DARWIN}
  RESOURCE_DIRECTORY = '';
  {$ELSE}
  RESOURCE_DIRECTORY = '../data/';
  {$ENDIF}

type
  TDemoScene = class(TMZScene)
  private
    FFont: TMZFont;
    FEvents: array [0..MAX_EVENTS - 1] of UTF8String;
    FEventIndex: Integer;
    FMouseX, FMouseY: Integer;
    FTouchFinger, FTouchX, FTouchY: Integer;
  private
    procedure AddEvent(const Desc: UTF8String);
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
        Is called when the application becomes active (gets focus). You can
        override this method if you need to take special action on activation. }
    procedure Activate; override;

    { Summary:
        Is called when the application deactivates (loses focus). You can
        override this method if you need to take special action on
        deactivation. }
    procedure Deactivate; override;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time the mouse moves.
        Does nothing by default.
      Parameters:
        X, Y: The mouse coordinates. }
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
        If the aoUseInputEvents application option is set, then this method is
        called each time the mouse wheel is rotated.
        Does nothing by default.
      Parameters:
        Up: True of wheel is rotated up, False if wheel is rotated down. }
    procedure MouseWheel(const Up: Boolean); override;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a keyboard key is pressed.
        Does nothing by default.
      Parameters:
        KeyCode: the key that is pressed. }
    procedure KeyDown(const KeyCode: TMZKeyCode); override;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a keyboard key is released.
        Does nothing by default.
      Parameters:
        KeyCode: the key that is released. }
    procedure KeyUp(const KeyCode: TMZKeyCode); override;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a keyboard key is entered.
        Does nothing by default.
      Parameters:
        Symbol: the key that is entered. }
    procedure KeyChar(const Symbol: String); override;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a finger moves over the touch screen.
        Does nothing by default.
      Parameters:
        Finger: The finger ID.
        X, Y: The touch coordinates.
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

    { Summary:
        Is fired when a "Low Memory Warning" is received from the device.
        Does nothing by default.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure SysLowMemoryWarning; override;

    { Summary:
        Is fired when a device changes orientation.
        Does nothing by default.
      Parameters:
        Orientation: The new orientation.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure SysChangeOrientation(const Orientation: TMZInterfaceOrientation); override;
  end;

procedure RunDemo;
var
  Application: TMZApplication;
begin
  Application := TMZApplication.Create;

  { Enable the aoUseInputEvents options to get input event notification. }
  Application.Options := Application.Options + [aoUseInputEvents, aoShowCursor] - [aoAllowPortraitOrientation];

  Application.Caption := 'MZ02 - Events';
  Application.ScreenWidth := 800;
  Application.ScreenHeight := 600;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TDemoScene }

const
  MOUSE_BUTTONS: array [TMZMouseButton] of String = (
    'mbLeft', 'mbMiddle', 'mbRight');

  ORIENTATIONS: array [TMZInterfaceOrientation] of String = (
    'ioUnknown', 'ioPortrait', 'ioPortraitUpsideDown', 'LandscapeRight', 'ioLandscapeLeft');

procedure TDemoScene.AddEvent(const Desc: UTF8String);
begin
  FEvents[FEventIndex] := Desc;
  Inc(FEventIndex);
  if (FEventIndex = MAX_EVENTS) then
    FEventIndex := 0;
end;

procedure TDemoScene.Startup;
begin
  inherited Startup;
  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'font.zfi');
  FMouseX := -1;
  FTouchX := -1;
  FEvents[0] := 'Use your mouse and keyboard!';
  FEventIndex := 1;

  { The KeyChar events are only fired if the keyboard is set to reading text. }
  TMZKeyboard.BeginReadText('');
end;

procedure TDemoScene.Shutdown;
begin
  FFont.Free;
  inherited Shutdown;
end;

procedure TDemoScene.RenderFrame;
var
  I, J, Y: Integer;
  S: UTF8String;
begin
  inherited RenderFrame;
  Canvas.BeginBatch;
  try
    if (FMouseX >= 0) then
    begin
      S := TMZUtils.Format('MouseMove(%d,%d)', [FMouseX, FMouseY]);
      Canvas.DrawText(FFont, 800, 0, S, [tfHAlignRight]);
    end;

    if (FTouchX >= 0) then
    begin
      S := TMZUtils.Format('TouchMove(%d,%d,%d)', [FTouchFinger, FTouchX, FTouchY]);
      Canvas.DrawText(FFont, 800, 20, S, [tfHAlignRight]);
    end;

    J := FEventIndex;
    Y := 0;
    for I := 0 to MAX_EVENTS - 1 do
    begin
      S := FEvents[J];
      Inc(J);
      if (J = MAX_EVENTS) THEN
        J := 0;

      if (S <> '') then
      begin
        Canvas.DrawText(FFont, 0, Y, S);
        Inc(Y, 20);
      end;
    end;
  finally
    Canvas.EndBatch;
  end;
end;

procedure TDemoScene.Activate;
begin
  inherited Activate;
  AddEvent('Activate');
end;

procedure TDemoScene.Deactivate;
begin
  inherited Deactivate;
  AddEvent('Deactivate');
end;

procedure TDemoScene.MouseMove(const X, Y: Integer);
begin
  inherited MouseMove(X, Y);
  FMouseX := X;
  FMouseY := Y;
end;

procedure TDemoScene.MouseDown(const Button: TMZMouseButton);
begin
  inherited MouseDown(Button);
  AddEvent(TMZUtils.Format('MouseDown(%s)', [MOUSE_BUTTONS[Button]]));
end;

procedure TDemoScene.MouseUp(const Button: TMZMouseButton);
begin
  inherited MouseUp(Button);
  AddEvent(TMZUtils.Format('MouseUp(%s)', [MOUSE_BUTTONS[Button]]));
end;

procedure TDemoScene.MouseWheel(const Up: Boolean);
begin
  inherited MouseWheel(Up);
  if Up then
    AddEvent('MouseWheel(Up)')
  else
    AddEvent('MouseWheel(Down)');
end;

procedure TDemoScene.KeyDown(const KeyCode: TMZKeyCode);
begin
  inherited KeyDown(KeyCode);
  AddEvent(TMZUtils.Format('KeyDown(%d)', [Ord(KeyCode)]));
end;

procedure TDemoScene.KeyUp(const KeyCode: TMZKeyCode);
begin
  inherited KeyUp(KeyCode);
  AddEvent(TMZUtils.Format('KeyUp(%d)', [Ord(KeyCode)]));
  if (KeyCode = kcEscape) then
    Application.Quit;
end;

procedure TDemoScene.KeyChar(const Symbol: String);
begin
  inherited KeyChar(Symbol);
  AddEvent(TMZUtils.Format('KeyChar(''%s'')', [Symbol]));
end;

procedure TDemoScene.TouchMove(const Finger, X, Y: Integer);
begin
  inherited TouchMove(Finger, X, Y);
  FTouchFinger := Finger;
  FTouchX := X;
  FTouchY := Y;
end;

procedure TDemoScene.TouchDown(const Finger: Integer);
begin
  inherited TouchDown(Finger);
  AddEvent(TMZUtils.Format('TouchDown(%d)', [Finger]));
end;

procedure TDemoScene.TouchUp(const Finger: Integer);
begin
  inherited TouchUp(Finger);
  AddEvent(TMZUtils.Format('TouchUp(%d)', [Finger]));
end;

procedure TDemoScene.SysLowMemoryWarning;
begin
  inherited SysLowMemoryWarning;
  AddEvent('SysLowMemoryWarning');
end;

procedure TDemoScene.SysChangeOrientation(
  const Orientation: TMZInterfaceOrientation);
begin
  inherited SysChangeOrientation(Orientation);
  AddEvent(TMZUtils.Format('SysChangeOrientation(%s)', [ORIENTATIONS[Orientation]]));
end;

end.
