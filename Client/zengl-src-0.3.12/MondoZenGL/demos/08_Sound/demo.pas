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
  zgl_sound,
  zgl_mouse,
  zgl_sprite_2d,
  zgl_text,
  zgl_collision_2d,
  zgl_fx,
  zgl_math_2d,
  zgl_memory,
  zgl_textures,
  zgl_font,
  zgl_keyboard,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  {$ENDIF}
  MondoZenGL;

const
  SCREEN_WIDTH = 800;
  SCREEN_HEIGHT = 600;
  {$IFDEF DARWIN}
  RESOURCE_DIRECTORY = '';
  {$ELSE}
  RESOURCE_DIRECTORY = '../data/';
  {$ENDIF}

type
  TDemoScene = class(TMZScene)
  private
    FFont: TMZFont;
    FTimer: TMZTimer;
    FStaticSound: TMZStaticSound;
    FStreamingSound: TMZStreamingSound;
    FIcon: array [Boolean] of TMZTexture;
    FPlaying: Boolean;
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
  Randomize;
  Application := TMZApplication.Create;
  Application.Options := Application.Options + [aoShowCursor, aoUseSound] - [aoAllowPortraitOrientation];
  Application.Caption := '08 - Sound';
  Application.ScreenWidth := SCREEN_WIDTH;
  Application.ScreenHeight := SCREEN_HEIGHT;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TDemoScene }

// RU: Т.к. звуковая подсистема нацелена на 3D, для позиционирования звуков в 2D нужны некоторые ухищрения
// EN: Because sound subsystem using 3D, there is some tricky way to calculate sound position in 2D
function CalcX2D(const X: Single): Single;
begin
  Result := (X - SCREEN_WIDTH / 2)  * (1 / SCREEN_WIDTH / 2);
end;

function CalcY2D(const Y:Single): Single;
begin
  Result := (Y - SCREEN_HEIGHT / 2) * (1 / SCREEN_HEIGHT / 2);
end;

procedure TDemoScene.TimerExpired(Sender: TObject);
var
  R: TMZRect;
  P: Integer;
begin
  // RU: Проверяем играет ли музыка(1 - играет, 0 - не играет). Так же можно проверить и звуки - подставив zglPSound и ID вот так:
  // snd_Get( Sound, ID...
  // ID возвращается функцией snd_Play
  //
  // EN: Check if music playing(1 - playing, 0 - not playing). Sounds also can be checked this way - just use zglPSound and ID:
  // snd_Get( Sound, ID...
  // ID returns by function snd_Play.
  FPlaying := FStreamingSound.IsPlaying;

  if TMZMouse.IsButtonClicked(mbLeft) then
  begin
    // RU: В данном случаи мы начинаем воспроизводить звук сразу в указанных координатах, но их можно менять и в процессе используя процедуру snd_SetPos.
    // Важно: Для OpenAL можно позиционировать только mono-звуки
    //
    // EN: In this case, we begin to play the sound directly in these coordinates, but they can be changed later using procedure snd_SetPos.
    // Important: OpenAL can position only mono-sounds.
    FStaticSound.Play(False, CalcX2D(TMZMouse.X), CalcY2D(TMZMouse.Y));

    if (not FPlaying) then
    begin
      R := TMZRect.Create((SCREEN_WIDTH - 128) / 2, (SCREEN_HEIGHT - 128) / 2, 128, 128);
      if R.ContainsPoint(TMZMouse.Position) then
        FStreamingSound.Play;
    end;
  end;

  // RU: Получаем в процентах позицию проигрывания аудиопотока и ставим громкость для плавных переходов.
  // EN: Get position in percent's for audio stream and set volume for smooth playing.
  P := FStreamingSound.PercentComplete;
  if (P >= 0) and (P < 25) then
    FStreamingSound.SetVolume(P / 24)
  else if (P >= 75) and (P < 100) then
    FStreamingSound.SetVolume(1 - ((P - 75) / 24));

  if TMZKeyboard.IsKeyPressed(kcEscape) then
    Application.Quit;

  TMZKeyboard.ClearState;
  TMZMouse.ClearState;
end;

procedure TDemoScene.Startup;
begin
  inherited Startup;
  // RU: Загружаем звуковой файл и устанавливаем для него максимальноe количество проигрываемых семплов в 2.
  // EN: Load the sound file and set maximum count of samples that can be played to the 2.
  FStaticSound := TMZStaticSound.Create(RESOURCE_DIRECTORY + 'click.wav', 2);
  FStreamingSound := TMZStreamingSound.Create(RESOURCE_DIRECTORY + 'music.ogg');

  FIcon[False] := TMZTexture.Create(RESOURCE_DIRECTORY + 'audio-stop.png');
  FIcon[True] := TMZTexture.Create(RESOURCE_DIRECTORY + 'audio-play.png');

  FTimer := TMZTimer.Create(TimerExpired, 16);
  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'font.zfi' );
end;

procedure TDemoScene.Shutdown;
begin
  FFont.Free;
  FTimer.Free;
  FIcon[True].Free;
  FIcon[False].Free;
  FStreamingSound.Free;
  FStaticSound.Free;
  inherited Shutdown;
end;

procedure TDemoScene.RenderFrame;
var
  R: TMZRect;
begin
  inherited RenderFrame;
  R := TMZRect.Create((SCREEN_WIDTH - 128) / 2, (SCREEN_HEIGHT - 128) / 2, 128, 128);
  Canvas.DrawSprite(FIcon[FPlaying], R);
  Canvas.DrawText(FFont, SCREEN_WIDTH / 2, (SCREEN_HEIGHT / 2) + 64,
    'Skillet - Comatose - Whispers In The Dark', [tfHAlignCenter]);

  if R.ContainsPoint(TMZMouse.Position) then
  begin
    Canvas.BlendMode := bmAdd;
    Canvas.DrawSprite(FIcon[FPlaying], (SCREEN_WIDTH - 132) / 2,
      (SCREEN_HEIGHT - 132) / 2, 132, 132, 0, 155);
    Canvas.BlendMode := bmNormal;
  end;
end;

end.
