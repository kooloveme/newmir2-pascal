unit demo_delphi;

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
  zgl_render_2d,
  zgl_text,
  zgl_font,
  zgl_utils,
  zgl_math_2d,
  zgl_keyboard,
  zgl_fx,
  zgl_primitives_2d,
  zgl_main,
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
        second) that has passed since the last call to Update.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update. }
    procedure Update(const DeltaTimeMs: Double); override;
  end;

procedure RunDemo;
var
  Application: TMZApplication;
begin
  Application := TMZApplication.Create;
  Application.Options := Application.Options + [aoShowCursor];
  Application.Caption := '04 - Text';
  Application.ScreenWidth := 1024;
  Application.ScreenHeight := 600;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TDemoScene }

procedure TDemoScene.Startup;
begin
  inherited Startup;
  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'font.zfi');
end;

procedure TDemoScene.Shutdown;
begin
  FFont.Free;
  inherited Shutdown;
end;

procedure TDemoScene.RenderFrame;
var
  R: TMZRect;
  S: UTF8String;
begin
  inherited RenderFrame;
  Canvas.BeginBatch;
  Canvas.DrawText(FFont, 512, 25, 'Строка с выравниванием по центру', [tfHAlignCenter]);
  Canvas.DrawText(FFont, 512, 65, 2, 0, 'Масштабирование', 255, $FFFFFF, [tfHAlignCenter]);
  Canvas.SetPerVertexColors($FF0000, $00FF00, $0000FF, $FFFFFF, 255, 255, 255, 255);
  Canvas.DrawText(FFont, 512, 125, 'Градация цвета для каждого символа', [tfHAlignCenter, tfPerVertexColors]);

  R := TMZRect.Create(0, 384 - 128, 192, 256);
  Canvas.DrawText(FFont, R, 'Обычный вывод текста в прямоугольнике');
  Canvas.DrawRect(R, $FF0000);

  R := TMZRect.Create(1024 - 192, 384 - 128, 192, 256);
  Canvas.DrawText(FFont, R, 'Вывод текста используя выравнивание по правому краю и размещение снизу',
    [tfHAlignRight, tfVAlignBottom]);
  Canvas.DrawRect(R, $FF0000);

  R := TMZRect.Create(512 - 192, 384 - 128, 384, 256);
  // RU: Если возникает вопрос почему я разделил текст на две части, то отвечу - FreePascal капризничает, и не хочет
  // обрабатывать константные строки длиннее 255 символов :)
  // EN: If you want to know why I use two parts of text, I can answer - because FreePascal doesn't like constant
  // string with more than 255 symbols :)
  Canvas.DrawText(FFont, R, 'Этот текст использует выравнивание по ширине и центрируется по вертикали.' +
    ' Текст, который не помещается в пределах прямоугольника будет отсечен.',
    [tfHAlignJustify, tfVAlignCenter]);
  Canvas.DrawRect(R, $FF0000);

  R := TMZRect.Create(512 - 320, 384 + 160, 640, 128);
  Canvas.DrawText(FFont, R, 'Для переноса строк можно использовать LF-символ' +
    #10 + 'код которого равен 10 и обозначен в таблице Unicode как "Line Feed"',
    [tfHAlignCenter, tfVAlignCenter]);
  Canvas.DrawRect(R, $FF0000);

  // RU: Выводим количество FPS в правом углу, используя text_GetWidth.
  // EN: Render frames per second in the top right corner using text_GetWidth.
  S := 'FPS: ' + TMZUtils.IntToStr(Application.CurrentRenderFrameRate);
  Canvas.DrawText(FFont, 1024 - Canvas.CalculateTextWidth(FFont, S), 0, S);

  Canvas.EndBatch;
end;

procedure TDemoScene.Update(const DeltaTimeMs: Double);
begin
  inherited Update(DeltaTimeMs);
  if TMZKeyboard.IsKeyPressed(kcEscape) then
    Application.Quit;
  TMZKeyboard.ClearState;
end;

end.
