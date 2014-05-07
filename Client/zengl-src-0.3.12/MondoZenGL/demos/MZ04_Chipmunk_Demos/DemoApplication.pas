unit DemoApplication;

interface

{$INCLUDE '..\..\src\mz_config.cfg'}

uses
  MondoZenGL,
  mzChipmunk,
  {$IFNDEF FPC}
  {$IFDEF USE_ZENGL_STATIC}
  zgl_textures,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  {$ENDIF}
  DemoLogoSmash,
  DemoSimple,
  DemoPyramidStack,
  DemoPlink,
  DemoTumble,
  DemoPyramidTopple,
  DemoBounce,
  DemoPlanet,
  DemoSpringies,
  DemoPump,
  DemoTheoJansen,
  DemoMagnetsElectric,
  DemoUnsafeOps,
  DemoQuery,
  DemoOneWay,
  {$IFNDEF iOS}
  DemoPlayer,
  {$ENDIF}
  DemoSensors,
  DemoJoints,
  DemoTank;

const
  {$IFDEF DARWIN}
  RESOURCE_DIRECTORY = '';
  {$ELSE}
  RESOURCE_DIRECTORY = '../data/';
  {$ENDIF}

type
  { Custom application class }
  TDemoApplication = class(TMZApplication)
  private
    FFont: TMZFont;
    FTextures: TMZTexture;
    FMouseBody: TCPBody;
  protected
    { Summary:
        Performs application startup. This method is called <b>after</b> the
        window and OpenGL contexts have been created. You can override this
        method to initialize the application and load application resources. }
    procedure Startup; override;

    { Summary:
        Performs application cleanup. This method is called <b>before</b> the
        window and OpenGL contexts will be destroyed. You can override this
        method to cleanup your application resources. }
    procedure Shutdown; override;
  public
    property Font: TMZFont read FFont;
    property Textures: TMZTexture read FTextures;
    property MouseBody: TCPBody read FMouseBody;
  end;

const
  DEMO_CLASSES: array [0..{$IFDEF iOS}17{$ELSE}18{$ENDIF}] of TMZSceneClass = (
    TDemoLogoSmash,
    TDemoSimple,
    TDemoPyramidStack,
    TDemoPlink,
    TDemoTumble,
    TDemoPyramidTopple,
    TDemoBounce,
    TDemoPlanet,
    TDemoSpringies,
    TDemoPump,
    TDemoTheoJansen,
    TDemoMagnetsElectric,
    TDemoUnsafeOps,
    TDemoQuery,
    TDemoOneWay,
    {$IFNDEF iOS}
    TDemoPlayer,
    {$ENDIF}
    TDemoSensors,
    TDemoJoints,
    TDemoTank);

implementation

{ TDemoApplication }

procedure TDemoApplication.Shutdown;
begin
  inherited;
  FMouseBody.Free;
  FTextures.Free;
  FFont.Free;
end;

procedure TDemoApplication.Startup;
begin
  TCPChipmunk.Initialize;
  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'Tahoma-Regular-8pt.zfi');
  FTextures := TMZTexture.Create(RESOURCE_DIRECTORY + 'physics.png');
  FTextures.SetFrameSize(64, 64);
  FMouseBody := TCPBody.Create(INFINITY, INFINITY);
  inherited;
end;

end.
