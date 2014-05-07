unit Demo;

{$INCLUDE '../../src/mz_config.cfg'}

{ This is a MondoZenGL version of the standard Chipmunk demos, with the addition
  that some shapes can be rendered with textures.

  Note that the original demos use a coordinate system with the origin at the
  center of the window, and the Y-axis pointing upwards.

  However, the (Mondo)ZenGL coordinate space has the origin at the top-left
  corner of the window with the Y-axis pointing downwards.

  We correct for this difference in the drawing functions my offsetting all
  objects towards the center of the screen and inverting the Y-axis. }

interface

procedure RunDemo;

implementation

uses
  MondoZenGL,
  DemoApplication,
  DemoScene;

procedure RunDemo;
var
  Application: TDemoApplication;
begin
  Randomize;
  Application := TDemoApplication.Create;
  Application.Options := Application.Options + [aoShowCursor, aoUseInputEvents] - [aoAllowPortraitOrientation];
  Application.ScreenWidth := SCREEN_WIDTH;
  Application.ScreenHeight := SCREEN_HEIGHT;
  Application.SetScene(DEMO_CLASSES[0].Create);
  { The application and scene will automatically be freed on shutdown }
end;

end.
