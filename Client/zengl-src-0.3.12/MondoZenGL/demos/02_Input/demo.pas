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
  zgl_utils,
  zgl_text,
  zgl_font,
  zgl_primitives_2d,
  zgl_keyboard,
  zgl_mouse,
  zgl_joystick,
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
    FFont: TMZFont;
    FSomething: UTF8String;
    FLineAlpha: Byte;
    FTimer: TMZTimer;
    FJoystick: TMZJoystick;
  private
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

    { Summary:
        Is called during each iteration of the main loop to render the current
        frame. }
    procedure RenderFrame; override;
  end;

procedure RunDemo;
var
  Application: TMZApplication;
begin
  Application := TMZApplication.Create;
  Application.Options := Application.Options + [aoShowCursor] - [aoAllowPortraitOrientation];
  Application.Caption := '02 - Input';
  Application.ScreenWidth := 800;
  Application.ScreenHeight :=600;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TDemoScene }

procedure TDemoScene.TimerExpired(Sender: TObject);
begin
  Dec(FLineAlpha, 10);

  // RU: Если зажат Alt и был нажат Enter - переключиться в полноэкранный или оконный режим.
  // EN: If Alt+Enter was pressed - switch to fullscreen or windowed mode.
  if TMZKeyboard.IsKeyDown(kcAlt) and TMZKeyboard.IsKeyPressed(kcEnter) then
  begin
    if (aoFullScreen in Application.Options) then
      Application.Options := Application.Options - [aoFullScreen]
    else
      Application.Options := Application.Options + [aoFullScreen];
  end;

  // RU: По нажатию Escape завершить приложение.
  // EN: If Escape was pressed - shutdown the application.
  if TMZKeyboard.IsKeyPressed(kcEscape) then
    Application.Quit;

  // RU: Если зажата левая кнопка мыши - заблокируем мышку по центру экрана.
  // Смещения можно получать используя функции mouse_DX и mouse_DY вызывая их до mouse_Lock.
  // EN: If left mouse button is down - lock the mouse cursor in center of screen.
  // Delta can be obtained from functions mouse_DX and mouse_DY by calling them before mouse_Lock.
  if TMZMouse.IsButtonDown(mbLeft) then
    TMZMouse.Center;

  // RU: "Считываем" в переменную введеный текст.
  // EN: "Read" the text to variable.
  FSomething := TMZKeyboard.GetText;
  if FSomething='我爱你妈妈' then FSomething:='abcd';
  // RU: Обязательно очищаем состояния.
  // EN: Necessarily clear all the states.
  TMZKeyboard.ClearState;
  TMZMouse.ClearState;
end;

procedure TDemoScene.Startup;
begin
  inherited Startup;
  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'font.zfi');
  FTimer := TMZTimer.Create(TimerExpired, 16);
  TMZKeyboard.BeginReadText('', 20);
end;

procedure TDemoScene.Shutdown;
begin
  FTimer.Free;
  FFont.Free;
  inherited Shutdown;
end;

procedure TDemoScene.RenderFrame;
var
  W: Single;
  I: Integer;
begin
  inherited RenderFrame;

  Canvas.DrawText(FFont, 512, 384 - 70, FSomething, [tfHAlignCenter]);
  W := Canvas.CalculateTextWidth(FFont, FSomething);
  Canvas.FillRect(512 + W / 2, 384 - 70, 5, 20, $FFFFFF, FLineAlpha);

end;

end.
