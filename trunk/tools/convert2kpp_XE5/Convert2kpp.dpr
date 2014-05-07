program Convert2kpp;



uses
  Forms,
  main in 'main.pas' {MainForm},
  CompressUnit in 'mir2resources\CompressUnit.pas',
  CompressUnit1 in 'mir2resources\CompressUnit1.pas',
  GameImages in 'mir2resources\GameImages.pas',
  HUtil32 in 'mir2resources\HUtil32.pas',
  MapFiles in 'mir2resources\MapFiles.pas',
  Mir2ImageFactory in 'Mir2ImageFactory.pas',
  Wil in 'mir2resources\Wil.pas',
  Wis in 'mir2resources\Wis.pas',
  wmUtil in 'mir2resources\wmUtil.pas',
  Wzl in 'mir2resources\Wzl.pas',
  ZLibEx in 'ZLib\ZLibEx.pas',
  zLibx in 'ZLib\zLibx.pas',
  DIB in 'delphix_all_in_one_12\Source\DIB.pas',
  DirectX in 'delphix_all_in_one_12\Source\DirectX.pas',
  DXClass in 'delphix_all_in_one_12\Source\DXClass.pas',
  DXConsts in 'delphix_all_in_one_12\Source\DXConsts.pas',
  Texture in '..\..\Client\Texture.pas',
  Convert in 'Convert.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
