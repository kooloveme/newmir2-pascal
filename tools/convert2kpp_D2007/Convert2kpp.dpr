program Convert2kpp;

{%TogetherDiagram 'ModelSupport_Convert2kpp\default.txaPackage'}

uses
  Forms,
  main in 'main.pas' {MainForm},
  CompressUnit in 'mir2resources\CompressUnit.pas',
  CompressUnit1 in 'mir2resources\CompressUnit1.pas',
  GameImages in 'mir2resources\GameImages.pas',
  HUtil32 in 'mir2resources\HUtil32.pas',
  MapFiles in 'mir2resources\MapFiles.pas',
  DIB in 'DirectX\DIB.pas',
  Mir2ImageFactory in 'Mir2ImageFactory.pas',
  DXConsts in 'DirectX\DXConsts.pas',
  DXClass in 'DirectX\DXClass.pas',
  DirectX in 'DirectX\DirectX.pas',
  Textures in 'mir2resources\Textures.pas',
  Wil in 'mir2resources\Wil.pas',
  Wis in 'mir2resources\Wis.pas',
  wmUtil in 'mir2resources\wmUtil.pas',
  Wzl in 'mir2resources\Wzl.pas',
  ZLibEx in 'ZLib\ZLibEx.pas',
  zLibx in 'ZLib\zLibx.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
