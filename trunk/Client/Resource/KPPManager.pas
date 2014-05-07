unit ResManager;

interface
uses KPP, Classes, sysutils, Texture,Gameimage;
const
  PrguseCount = 3; //界面文件数量

  KppFileCount = 4; //资源管理器维护文件的数量
  OtherFileCount = 1; //无规则的单项文件数量

  {数组下标}
  CHRSEL = 1;
type
  TResManager = class(TThread)
  private
    Finited: Boolean;
    FRoot: string;
    FLoadPercent: integer;
    class var FInstance: TResManager;
      PrguseFile: array[1..PrguseCount] of TGameImage;
      OtherFile: array[1..OtherFileCount] of TGameImage; //单项文件数组
      AllFileList: TList;
    function FileNameFormat(sfile: string; index: integer): string;
    procedure LoadKppFile;
    procedure CalcPercent(loaded: integer);
    constructor Create(sRoot: string); //传递进来的根目录要确保最后是斜杠/
  protected
    procedure Execute; override;
  public
    class function GetInstance: TResManager;
    destructor Destroy; override;
    procedure WriteLog(s: string);
    function GetPrguseTexture(PrguseIndex: integer; Index: integer; AutoFree:
      Boolean = True): TTexture;
    function GetOtherTexture(OtherFileIndex: integer; index: integer; AutoFree:
      Boolean = True): TTexture;
    Function GetTexture(ImageFile:TGameImage;index:Integer;AutoFree:Boolean=True):TTexture;
    property LoadPercent: integer read FLoadPercent;
  end;
implementation
uses
  MondoZenGL;
{ TKppManager }

destructor TResManager.Destroy;
var
  i: integer;
  t: TkppFile;
begin
  for I := 0 to AllFileList.Count - 1 do
  begin
    t := AllFileList[i];
    FreeAndNil(T);

  end;
  AllFileList.Free;
  inherited;
end;

procedure TResManager.Execute;
var
  i: integer;
begin
  inherited;
  if not Finited then
    LoadKppFile;
  while not Terminated do
  begin
    //for i := 1 to PrguseCount do
      //PrguseFile[i].CheckFreeTexture;
    Sleep(100);
  end;
end;

function TResManager.FileNameFormat(sfile: string; index: integer): string;
begin
  result := Froot + sfile + '.KPP';
  if index <= 1 then
    exit;
  if index >= 256 then
    exit;
  result := format(sfile + '%d', [index]);
  result := Froot + result + '.KPP';
end;

class function TResManager.GetInstance: TResManager;
begin
  if not assigned(FInstance) then
    FInstance := TResManager.Create('D:/KppData/');
  result := FInstance;
end;

function TResManager.GetOtherTexture(OtherFileIndex, index: integer; AutoFree:
  Boolean): TTexture;
begin
  Result := nil;
  if not OtherFileIndex in [1..OtherFileCount] then
    Exit;
    Result:=OtherFile[OtherFileIndex].GetTexture(index,AutoFree);
end;

function TResManager.GetPrguseTexture(PrguseIndex, Index: integer; AutoFree:
  Boolean): TTexture;
begin
  Result := nil;
  if not PrguseIndex in [1..PrguseCount] then
    exit;
  Result:=PrguseFile[PrguseIndex].GetTexture(index,AutoFree);
end;

function TResManager.GetTexture(ImageFile: TGameImage; index: Integer;
  AutoFree: Boolean): TTexture;
begin
Result:=nil;
if Assigned(ImageFile) then
begin
 Result:=ImageFile.GetTexture(index,AutoFree);
end;

end;

procedure TResManager.LoadKppFile;
var
  I: integer;
  LoadedFileCount: integer;
begin
  FLoadPercent := 0;
  LoadedFileCount := 0;
  for I := 1 to PrguseCount do
  begin
    try
      PrguseFile[i] := TKPPFile.Create(FileNameformat('Prguse', i));
      PrguseFile[i].Init;
    except
      WriteLog('Load Error' + FileNameformat('Prguse', i));
      PrguseFile[i].Free;
      PrguseFile[i] := nil;
    end;
    AllFileList.Add(PrguseFile[I]);
    inc(LoadedFileCount);
    CalcPercent(LoadedFileCount);
  end;

  for I := 1 to OtherFileCount do
  begin
    try
      OtherFile[i] := TKPPFile.Create(FileNameformat('ChrSel', i));
      OtherFile[i].init;
    except
      WriteLog('Load Error' + FileNameformat('Prguse', i));
      OtherFile[i].Free;
      OtherFile[i] := nil;
    end;
    inc(LoadedFileCount);
    AllFileList.Add(OtherFile[I]);
    CalcPercent(LoadedFileCount);
    Sleep(1000);
  end;

  Finited := True;
end;

procedure TResManager.WriteLog(s: string);
begin
  TMZLog.Log(S, True);
end;

procedure TResManager.CalcPercent(loaded: integer);
begin
  FLoadPercent := trunc((Loaded / KppFileCount) * 100);
end;

constructor TResManager.Create(sRoot: string);

begin
  inherited Create(False);
  FreeOnTerminate := False;
  Finited := False;
  FRoot := sRoot;
  AllFileList := TList.Create;
end;

end.

