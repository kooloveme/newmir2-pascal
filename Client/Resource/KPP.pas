unit KPP;

interface
uses
  Classes, SysUtils, pngimage, MondoZenGl,Texture,GameImage;
const
  FileDesc = 'Kadin Png Package File';

type
  TCoordinate = record
    X: SmallInt;
    Y: Smallint;
  end;
  TKPPFileHeader = record
    Desc: string[30];
    Version: integer;
    PictureCount: Integer;
    IdxDescOffset: Integer;
    IdxDescLength: Integer;
  end;
  TKPPIdxDesc = record
    Coordinate: TCoordinate;
    Offset: Integer;
    Len: Integer;
  end;
  TKPPFile = class(TGameImage)
  private
    FHeader: TKPPFileHeader;
    FVerifyIdxDesc: TKPPIdxDesc;
    FLastdataPostion: Integer; //记录最后一张图片的索引位置
    FileStream: TFileStream;
    FTextureInfoList: TThreadList;
    FIdxDescList:array of TKPPIdxDesc;
    FCount: Integer;
    FInited: Boolean;
    FIsEdit: Boolean; //标示文件是被读取还是被编辑
    FCheckFreeTick:Cardinal; //检查文件释放间隔
    FLastCheckTick:Cardinal;//上次释放时间。
    FFreeTextureTick:Cardinal;//多久未使用的纹理就会被释放
    function DecryptIndex: Boolean; //解密资源 未实现
    function EncryptIndex: Boolean;//加密资源 未实现
    function GetPng(Index: integer): TPNGImage;
    function GetCoordinate(idx: integer): TCoordinate;
    procedure SetCoordinate(idx: integer; Coor: TCoordinate);
    function GetTexture(index: integer): TTexture;
  public
    Key: string;
    FileName: string;
    constructor Create(sFile: string;CheckFreeTick:integer = 5000;FreeTextureTick:integer=10000; sKey: string = '123456');
    {文件名;多久执行检查释放纹理，多久没使用的纹理就会被释放，加密密码。
        第二和第三个参数 由资源管理器进行。自身并不会检测}
    destructor Destroy; override;
    function CreateFile: Boolean;
    Procedure Init;override;
    procedure Append(PNG: TPNGImage); overload;
    procedure Append(sFile: string); overload;
    procedure Append(Stream: TStream); overload;
    procedure Insert(idx: integer; Png: TPNGImage; Coor: TCoordinate); overload;
    procedure Insert(idx: Integer; Png: Pointer; Size: Integer; Coor:
      TCoordinate); overload;
    procedure Delete(idx: integer);
    procedure save;
    procedure CheckFreeTexture;
    Function GetCacheTexture(index:Integer):TTexture;//获取缓存纹理，此纹理不会加入自动释放列表
    property Png[idx: integer]: TPNGImage read GetPng;
    property Count: integer read FCount;
    property Inited: Boolean read FInited;
    property Coordinate[idx: integer]: TCoordinate read GetCoordinate write
      SetCoordinate;

  end;

implementation

{ TKPPFile }

procedure TKPPFile.Append(sFile: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(sFile, fmOpenRead);
  Append(s);
  s.Free;
end;

procedure TKPPFile.Append(PNG: TPNGImage);
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  Png.SaveToStream(M);
  Append(M);
  M.Free;
end;

procedure TKPPFile.Append(Stream: TStream);
begin
  FIsEdit := True;
  FCount := FCount + 1; //扩展数组长度.
  SetLength(FIdxDescList, FCount);
  FileStream.Seek(FLastdataPostion, soBeginning);
  FIdxDescList[FCount - 1].Offset := FileStream.Position;
  FIdxDescList[FCount - 1].Len := Stream.Size;
  Stream.Seek(0, soBeginning);
  FileStream.CopyFrom(Stream, Stream.Size);
  FLastdataPostion := FileStream.Position;
end;

procedure TKPPFile.CheckFreeTexture;
var
List:TList;
I:integer;
Tex:TTexture;
begin
if not FInited then Exit;//如果文件没有初始化。则不干
if TThread.GetTickCount-FLastCheckTick < FCheckFreeTick  then Exit; //上次时间和本次执行 间隔太小。则不干
List:=FTextureInfoList.LockList;
for I := 0 to List.Count - 1 do
  begin
  Tex:=List[i];
  if not Assigned(Tex) then Continue;
  if TThread.GetTickCount-Tex.m_nLastCheckTime > FFreeTextureTick then
  begin
    Tex.Free;
    List[i]:=nil;
  end;
  end;
  FLastCheckTick:=TThread.GetTickCount;
  FTextureInfoList.UnlockList;

end;

constructor TKPPFile.Create(sFile: string;CheckFreeTick:integer = 5000;FreeTextureTick:integer=10000; sKey: string = '123456');
begin
  inherited Create;
  FileName := sFile;
  FCheckFreeTick:=CheckFreeTick;
  FFreeTextureTick:=FreeTextureTick;
  FLastCheckTick:=TThread.GetTickCount;
  Key := sKey;
  FileStream := nil;
  FInited := False;
  FIsEdit := False;
end;

function TKPPFile.CreateFile: Boolean;

begin
  Result := False;
  if Inited and (FileStream <> nil) then
    Exit;
  FCount := 0;
  FileStream := TFileStream.Create(FileName, fmCreate); //创建文件。
  with FHeader do
  begin
    Desc := FileDesc;
    Version := 1;
    PictureCount := 0;
    IdxDescOffset := 0;
    IdxDescLength := 0;
  end;
  FileStream.Write(FHeader, SizeOf(TKPPFileHeader)); //文件头
  FileStream.Write(FVerifyIdxDesc, SizeOf(TKPPIdxDesc)); //验证索引
  FLastdataPostion := FileStream.Position;
end;

function TKPPFile.DecryptIndex: Boolean;
begin
  //初始化解密类
  //开始解密
  //验证解密是否OK
  Result := True;
end;

procedure TKPPFile.Delete(idx: integer);
begin

end;

destructor TKPPFile.Destroy;
begin
  inherited;
  if FIsEdit then
    save;
  FileStream.Free;
  FTextureInfoList.Free;
end;

function TKPPFile.EncryptIndex: Boolean;
begin
  //将最后一个数组下标赋值 作为验证。
  //加密所有索引序列。
  Result := True;
end;

function TKPPFile.GetCacheTexture(index: Integer): TTexture;
var
Size,Offset:Integer;
P:Pointer;
tex:TMZTexture;
Coor:TCoordinate;
begin
Result:=nil;
if not ((index >= 0) and (index < FCount)) then exit;
Size:=FIdxDescList[index].Len;
Offset:=FIdxDescList[index].Offset;
FileStream.Seek(Offset,soBeginning);
GetMem(P,Size);
FileStream.Read(P^,Size);
Tex:= TMzTexture.Create(P, size, 'png', 0, [], tflinear, twclamp, nil);
FreeMem(P,Size);
Coor:=Coordinate[index];
Result:=TTexture.Create(Tex,Coor.X,Coor.y);
end;

function TKPPFile.GetCoordinate(idx: integer): TCoordinate;
begin
  if (idx > -1) and (idx < FCount) then
  begin
    Result := FIdxDescList[idx].Coordinate;
  end;
end;

function TKPPFile.GetPng(Index: integer): TPNGImage;
begin
  FileStream.Seek(FIdxDescList[Index].Offset, soBeginning);
  Result := TPNGImage.Create;
  Result.LoadFromStream(FileStream);
end;

function TKPPFile.GetTexture(index: integer): TTexture;
var
  List:TList;
begin
  Result:=nil;
  if not ((index >= 0) and (index < FCount)) then exit;
  List:=FTextureInfoList.LockList;//锁定列表
  Result:=List[index]; //读取纹理。
  if not Assigned(Result) then
  begin
    Result:=GetCacheTexture(index);
    List[index]:=Result; //保存到列表
  end;
  Result.m_nLastCheckTime:=TThread.GetTickCount;
  FTextureInfoList.UnlockList;//解锁列表
end;

procedure TKPPFile.Insert(idx: integer; Png: TPNGImage; Coor: TCoordinate);
begin

end;

procedure TKPPFile.Insert(idx: Integer; Png: Pointer; Size: Integer;
  Coor: TCoordinate);
begin

end;

procedure TKPPFile.Init;
begin
  if FInited then
    Exit;
  try
    FileStream := TFileStream.Create(FileName, fmOpenReadWrite or
      fmShareExclusive);
  except
    FileStream.Free;
    exit;
  end;
  FileStream.Read(FHeader, SizeOf(TKPPFileHeader));
  FCount := FHeader.PictureCount;
  FTextureInfoList:=TThreadList.Create;
  FTextureInfoList.LockList.Count:=FCount;
  FTextureInfoList.UnlockList;
  FileStream.Seek(FHeader.IdxDescOffset, soBeginning);
  if FCount>0  then SetLength(FIdxDescList,Fcount);
  if FHeader.IdxDescLength <> FileStream.Read(FIdxDescList[0], FCount *Sizeof(TKPPIdxDesc)) then Exit;
  FLastdataPostion := FHeader.IdxDescOffset;
  //如果读入的长度和文件设置的长度不一致 则不行。
  //读入完毕开始解密。 如果解密错误则退出。
  if not DecryptIndex then
    Exit;
  FInited := True;
end;

procedure TKPPFile.save;
begin
  with FHeader do
  begin
    PictureCount := FCount;
    IdxDescOffset := FLastdataPostion;
    IdxDescLength := SizeOf(TKPPIdxDesc) * FCount;
  end;
  with FVerifyIdxDesc do
  begin
    Coordinate := FIdxDescList[FCount - 1].Coordinate;
    Offset := FIdxDescList[FCount - 1].Offset;
    len := FIdxDescList[FCount - 1].Len;
  end;
  FileStream.Seek(0, soBeginning);
  FileStream.Write(FHeader, SizeOf(TKPPFileHeader));
  FileStream.Write(FVerifyIdxDesc, SizeOf(TKPPIdxDesc));
  FileStream.Seek(FLastdataPostion, soBeginning);
  if EncryptIndex then
    FileStream.Write(FIdxDescList[0], SizeOf(TKPPIdxDesc) * FCount);
  //读会包含当前位置 wirte不会
end;

procedure TKPPFile.SetCoordinate(idx: integer; Coor: TCoordinate);
begin
  if (idx > -1) and (idx < FCount) then
  begin
    FIdxDescList[idx].Coordinate := Coor;
  end;
end;

end.

