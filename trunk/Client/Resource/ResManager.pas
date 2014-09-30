unit ResManager;

interface
uses  Classes, sysutils, Texture,Gameimage,Kpp,vcl.forms,Wil,Wzl,System.Generics.Collections;
const
  PRGUSECOUNT = 3; //界面文件数量
  TILESCOUNT = 10;  //地砖数量
  SMTILESCOUNT = 10;//小地砖数量
  MAPOBJCOUNT = 40; //Objects.wil文件数量
  OTHERCOUNT = 1; //无规则的单项文件数量
  FILECOUNT = PRGUSECOUNT + TILESCOUNT + SMTILESCOUNT + MAPOBJCOUNT + OTHERCOUNT; //资源管理器维护文件的数量
type
  TResManager = class(TThread)
  private
    class var FInstance : TResManager;
    m_bInited           : Boolean;
    m_sRoot             : string;
    m_nLoadedFileCount  : integer;
    m_FileList          :TList<TGameImage>;
    function FileNameFormat(sfile: string; index: integer): string;
    procedure LoadFile;
    Function CalcPercent():integer;
    constructor Create(sRoot: string); //传递进来的根目录要确保最后是斜杠/
    Function LoadResFile(sFile:String):TGameImage;//传递进来的文件名是不包括后缀名的。
  protected
    procedure Execute; override;
  public
    class function GetInstance: TResManager;
    destructor Destroy; override;
    Function GetTexture(FileType:Integer;FileIndex:integer;ImageIndex:Integer):TTexture;
    property LoadPercent: integer read CalcPercent;
  end;
var
{数组下标}
  CHRSEL   :Integer=1;
  PRGUSE   :Integer;
  MAPOBJ   :Integer;
  MONSTER  :Integer;
  TILES    :Integer;
  SMTILES  :Integer;
  HUMAN    :Integer;
  WEAPON   :Integer;
  HUMEFF   :Integer;
  WEAPONEFF:Integer;
  OTHER    :Integer;
implementation
uses
  MondoZenGL,Share;
{ TKppManager }

destructor TResManager.Destroy;
var
  i: integer;
  t: TGameImage;
begin
  for I := 0 to m_FileList.Count - 1 do
  begin
    t := m_FileList[i];
    FreeAndNil(T);

  end;
  m_FileList.Free;
  inherited;
end;

procedure TResManager.Execute;
begin
  inherited;
  if not m_bInited then
    LoadFile;
  while not Terminated do
  begin
    Sleep(100);
  end;
end;

function TResManager.FileNameFormat(sfile: string; index: integer): string;
begin
  result := m_sRoot + sfile;
  if index <= 1 then
    exit;
  if index >= 256 then
    exit;
  result := format(sfile + '%d', [index]);
  result := m_sRoot + result;
end;

class function TResManager.GetInstance: TResManager;
begin
  if not assigned(FInstance) then
    FInstance := TResManager.Create(g_sClientPath+'Data\');
  result := FInstance;
end;


function TResManager.GetTexture(FileType, FileIndex, ImageIndex: Integer): TTexture;
begin
  Result:=m_FileList[FileType+FileIndex-1].GetTexture(ImageIndex);
end;

procedure TResManager.LoadFile;
var
  I: integer;
  GameImage:TGameImage;
begin
  m_nLoadedFileCount := 0;
  //载入界面文件
  PRGUSE:=0;//在FileList的下标内=0；
  for I := 1 to PRGUSECOUNT do
  begin
    GameImage:=LoadResFile(FileNameformat('Prguse', i));
    m_FileList.Add(GameImage);
    inc(m_nLoadedFileCount);
  end;
  OTHER:=m_FileList.Count;
  //载入杂项文件
  for I := 1 to OTHERCOUNT do
  begin
    GameImage:= LoadResFile(FileNameformat('ChrSel', i));
    m_FileList.Add(GameImage);
    inc(m_nLoadedFileCount);
  end;
  //载入地图Object文件
  MAPOBJ:=m_FileList.Count;
  for I := 1 to MAPOBJCOUNT do
  begin
    GameImage:=LoadResFile(FileNameFormat('Objects',i));
    m_FileList.Add(GameImage);
    inc(m_nLoadedFileCount);
  end;
  TILES:=m_FileList.Count;
  for I := 1 to TILESCOUNT do
  begin
    GameImage:=LoadResFile(FileNameFormat('Tiles',i));
    m_FileList.Add(GameImage);
    inc(m_nLoadedFileCount);
  end;
  SMTILES := m_FileList.Count;
  for I := 1 to SMTILESCOUNT do
  begin
    GameImage:=LoadResFile(FileNameFormat('SmTiles',i));
    m_FileList.Add(GameImage);
    inc(m_nLoadedFileCount);
  end;

  m_bInited := True;
end;

function TResManager.LoadResFile(sFile: String): TGameImage;
begin
  //首先检查kpp 文件是否存在。
  //再检查Wzl文件是否存在。
  //再检查wil文件是否存在.
  Result:=nil;
  if FileExists(sFile+'.KPP') then
  begin
    Result:=TKPPFile.Create(sFile+'.KPP');
    Result.Init;
    Exit;
  end else
  if FileExists(sFile+'.Wzl') Then
  begin
    Result:=TWzlImage.Create(sFile+'.Wzl');
    Result.Init;
    Exit;
  end else
  if FileExists(sFile+'.Wil') then
  begin
    Result:=TWilImage.Create(sFile+'.Wil');
    Result.Init;
    Exit
  end;
end;


Function TResManager.CalcPercent:Integer;
begin
  Result := trunc((m_nLoadedFileCount div FILECOUNT) * 100);
end;

constructor TResManager.Create(sRoot: string);

begin
  inherited Create(False);
  FreeOnTerminate := False;
  m_bInited := False;
  m_sRoot := sRoot;
  m_FileList:= TList<TGameImage>.Create;
end;

initialization
finalization
begin

end;
end.

