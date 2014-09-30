program Client;

{$R *.res}
{$I zglCustomConfig.cfg}
uses
 // FastMM4,
  Main in 'Main.pas',
  KPP in 'Resource\KPP.pas',
  LoadingScene in 'Scene\LoadingScene.pas',
  ResManager in 'Resource\ResManager.pas',
  DrawEx in 'DrawEx.pas',
  Texture in 'Texture.pas',
  LoginScene in 'Scene\LoginScene.pas',
  GameImage in 'Resource\GameImage.pas',
  Wil in 'Resource\Wil.pas',
  Util32Ex in 'Util32Ex.pas',
  GfxFont in 'GfxFont.pas',
  Guidesign in 'Guidesign.pas' {frmGuiDesgin},
  SoundEngine in 'Sound\SoundEngine.pas',
  GameScene in 'Scene\GameScene.pas',
  Share in 'Share.pas',
  Wzl in 'Resource\Wzl.pas',
  MZGui in 'MZGui.pas',
  PlayScene in 'Scene\PlayScene.pas',
  Map in 'Map\Map.pas',
  Mir2Map in 'Map\Mir2Map.pas',
  ActionType in 'Actor\ActionType.pas',
  HumAction in 'Actor\HumAction.pas',
  MonsterAction in 'Actor\MonsterAction.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  Main.Init;
end.
