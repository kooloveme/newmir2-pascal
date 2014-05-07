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
  zgl_primitives_2d,
  zgl_fx,
  zgl_utils,
  zgl_text,
  zgl_font,
  zgl_render_2d,
  zgl_textures,
  zgl_sprite_2d,
  zgl_keyboard,
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
    FTexBackground: TMZTexture;
    FTexParticle: TMZTexture;
    FFire: array [0..6] of TMZParticleEmitter;
    FSparkle: TMZParticleEmitter;
    FRain: TMZParticleEmitter;
    FTotalParticleCount: Integer;
    FDebug: Boolean;
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
  Randomize;
  Application := TMZApplication.Create;
  Application.Options := Application.Options + [aoShowCursor] - [aoAllowPortraitOrientation];
  Application.Caption := '10 - Particles 2D';
  Application.ScreenWidth := 800;
  Application.ScreenHeight := 600;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TDemoScene }

procedure TDemoScene.Startup;
const
  POSITIONS: array [0..6,0..1] of Single = ((822, 243), (51, 471), (315, 471),
    (681, 312), (407, 540), (746, 538), (947, 672));
var
  I: Integer;
  E: TMZParticleEmitter;
begin
  inherited Startup;
  FTexBackground := TMZTexture.Create(RESOURCE_DIRECTORY + 'back02.png');
  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'font.zfi' );
  FTexParticle := TMZTexture.Create(RESOURCE_DIRECTORY + 'particle.png',
    $FF000000, DEFAULT_TEXTURE_FLAGS + [tfGenerateMipMaps]);
  FTexParticle.SetFrameSize(32, 32);

  for I := 0 to 6 do
  begin
    E := TMZParticleEmitter.Create(FTexParticle);
    FFire[I] := E;

    if (I < 3) then
    begin
      E.SetPointEmitter(POSITIONS[I,0], POSITIONS[I,1], 270, 15);
      E.Loop := True;
      E.EmitterLifetime := 1000;
      E.EmissionRate := 10;
      E.ParticleLifetime := 1200;
    end
    else if (I < 6) then
    begin
      E.SetPointEmitter(POSITIONS[I,0], POSITIONS[I,1], 270, 15);
      E.Loop := True;
      E.EmitterLifetime := 1000;
      E.EmissionRate := 10;
      E.ParticleLifetime := 1500;
    end
    else
    begin
      { Horizontal line emitter }
      E.SetLineEmitter(POSITIONS[I,0], POSITIONS[I,1], 115, 270, 15);
      E.Loop := True;
      E.EmitterLifetime := 1000;
      E.EmissionRate := 100;
      E.ParticleLifetime := 1200;
    end;

    E.BlendMode := bmAdd;
    E.StartFrameNumber := 4;
    E.EndFrameNumber := 4;
    E.SetColors([$FF2222, $FF2222, $FFFF00, $FF2200, $FF0000],
                [0,       0.1,     0.2,     0.8,     1      ]);
    E.SetAlphas([0, 55,  255, 55,  0],
                [0, 0.1, 0.3, 0.8, 1]);
    E.ParticleWidth  := 8 + Ord((I > 2) and (I < 6)) * 2;
    E.ParticleHeight := 8 + Ord((I > 2) and (I < 6)) * 2;
    E.SetSizes([1, 1, 4, 4, 2, 2], [0, 0.6, 1]);
    E.RotationAngleVariation := 360;
    E.Velocity := 64;
    E.SetVelocities([0, 1], [0, 1]);
    E.SetAngularVelocities([0, 0], [0, 1]);
    E.SpinAngleVariation := 90;
    E.SetSpinAngles([1, 2], [0, 1]);
  end;

  FSparkle := TMZParticleEmitter.Create(FTexParticle);
  FSparkle.SetRectangleEmitter(5, 538, 30, 35, 0, 0);
  FSparkle.Loop := True;
  FSparkle.EmitterLifetime := 1000;
  FSparkle.EmissionRate := 2;
  FSparkle.ParticleLifetime := 1000;
  FSparkle.ParticleLifetimeVariation := 1000;
  FSparkle.BlendMode := bmAdd;
  FSparkle.StartFrameNumber := 2;
  FSparkle.EndFrameNumber := 2;
  FSparkle.SetColors([$FFFF00, $FFAAAA], [0, 1]);
  FSparkle.SetAlphas([0, 255, 0], [0, 0.4, 1]);
  FSparkle.ParticleWidth := 16;
  FSparkle.ParticleHeight := 16;
  FSparkle.SetSizes([1, 1, 2, 2], [0, 1]);
  FSparkle.SetVelocities([0, 0], [0, 1]);
  FSparkle.SetAngularVelocities([0, 0], [0, 1]);
  FSparkle.SpinAngle := 45;
  FSparkle.SetSpinAngles([1, 1], [0, 1]);

  FRain := TMZParticleEmitter.Create(FTexParticle);
  FRain.SetLineEmitter(512, 0, 1024, 80, 5);
  FRain.Loop := True;
  FRain.EmitterLifetime := 1000;
  FRain.EmissionRate := 250;
  FRain.ParticleLifetime := 1000;
  FRain.ParticleLifetimeVariation := 100;
  FRain.StartFrameNumber := 3;
  FRain.EndFrameNumber := 3;
  FRain.SetAlphas([0, 255, 200, 0], [0, 0.1, 0.9, 1]);
  FRain.ParticleWidth := 16;
  FRain.ParticleHeight := 16;
  FRain.SetSizes([1, 1, 1, 1], [0, 1]);
  FRain.Velocity := 575;
  FRain.SetVelocities([1, 1], [0, 1]);
  FRain.SetAngularVelocities([0, 0], [0, 1]);
  FRain.SetSpinAngles([0, 0], [0, 1]);
end;

procedure TDemoScene.Shutdown;
var
  I: Integer;
begin
  FRain.Free;
  FSparkle.Free;
  for I := 0 to 6 do
    FFire[I].Free;
  FTexParticle.Free;
  FFont.Free;
  FTexBackground.Free;
  inherited Shutdown;
end;

procedure TDemoScene.RenderFrame;
var
  I: Integer;
begin
  inherited RenderFrame;
  Canvas.BeginBatch;
  try
    Canvas.DrawSprite(FTexBackground, 0, 0, 1024, 768);

    for I := 0 to 6 do
      FFire[I].Render;
    FSparkle.Render;
    FRain.Render;

    if (FDebug) then
    begin
      for I := 0 to 6 do
        Canvas.DrawRect(FFire[I].BoundingBox, $FF0000);
      Canvas.DrawRect(FSparkle.BoundingBox, $FF0000);
      Canvas.DrawRect(FRain.BoundingBox, $FF0000);
    end;

    Canvas.BlendMode := bmNormal;
    Canvas.ColorMode := cmMix;

    Canvas.DrawText(FFont, 0, 0, 'FPS: ' + TMZUtils.IntToStr(Application.CurrentRenderFrameRate));
    Canvas.DrawText(FFont, 0, 20, 'Particles: ' + TMZUtils.IntToStr(FTotalParticleCount));
    {$IFDEF iOS}
    Canvas.DrawText(FFont, 0, 40, 'Debug(Tap): ' + TMZUtils.BoolToStr(FDebug));
    {$ELSE}
    Canvas.DrawText(FFont, 0, 40, 'Debug(F1): ' + TMZUtils.BoolToStr(FDebug));
    {$ENDIF}
  finally
    Canvas.EndBatch;
  end;
end;

procedure TDemoScene.Update(const DeltaTimeMs: Double);
var
  I: Integer;
begin
  inherited Update(DeltaTimeMs);

  FTotalParticleCount := 0;
  for I := 0 to 6 do
  begin
    FFire[I].Update(DeltaTimeMs);
    Inc(FTotalParticleCount, FFire[I].ParticleCount);
  end;
  FSparkle.Update(DeltaTimeMs);
  Inc(FTotalParticleCount, FSparkle.ParticleCount);
  FRain.Update(DeltaTimeMs);
  Inc(FTotalParticleCount, FRain.ParticleCount);

  if TMZKeyboard.IsKeyPressed(kcEscape) then
    Application.Quit;

  if TMZKeyboard.IsKeyPressed(kcF1) then
    FDebug := not FDebug;

  TMZKeyboard.ClearState;
  
  {$IFDEF iOS}
  if TMZTouch.IsFingerTapped(0) then
    FDebug := not FDebug;
    
  TMZTouch.ClearState;
  {$ENDIF}
end;

end.
