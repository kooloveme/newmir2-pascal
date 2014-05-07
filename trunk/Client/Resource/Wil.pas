unit Wil;

interface
uses
GameImage,Texture,MondoZenGL,zgl_textures;
type
  TWMImageHeader = record
    Title: string[40];
    ImageCount: Integer;
    ColorCount: Integer;
    PaletteSize: Integer;
    VerFlag: Integer;
  end;
  PTWMImageHeader = ^TWMImageHeader;

  TWMImageInfo = record
    nWidth: SmallInt;
    nHeight: SmallInt;
    px: SmallInt;
    py: SmallInt;
    bits: PByte;
  end;
  PTWMImageInfo = ^TWMImageInfo;

  TWMIndexHeader = record
    Title: string[40];
    IndexCount: integer;
    VerFlag: integer;
  end;
  PTWMIndexHeader = ^TWMIndexHeader;

  TWilImage=class(TGameImage)
  private
    FPalette:array[0..255] of Cardinal;
    FVersion:Integer;
    FColorBit:Integer;
    procedure Loadindex;
    procedure LoadPalette;
  protected
    procedure Init;override;
    function GetCacheTexture(idx:Integer):TTexture;override;
  public
    constructor Create(FileName:string);
    destructor Destroy;override;
    Function GetTexture(idx:integer;AutoFree:Boolean=True):TTexture;override;

    end;

implementation
uses SysUtils,Classes,Util32Ex;
{ TWilImage }

constructor TWilImage.Create(FileName: string);
begin
inherited Create;
FFileName:=FileName;
FVersion:=0;
end;

destructor TWilImage.Destroy;
begin

  inherited;
end;

function TWilImage.GetCacheTexture(idx: Integer): TTexture;
var
MZTexture:TMZTexture;
ImageInfo:TWMImageInfo;
i,OffSet,bitSize,Pixel:Integer;
ColorIndex:Byte;
Pbits:PByte;

PBits16:PWord;
Pixel16:Word;
PTexData:PInteger;
PTexture:zglPTexture;
begin
Result:=nil;
if  not ((idx >= 0) and (idx < ImageCount) and (FFileStream <> Nil))  then Exit;

    OffSet:=Findex[idx];
    FFileStream.seek(OffSet,soBeginning);
    if FVersion <> 0 then
        FFileStream.Read(ImageInfo,SizeOf(TWMImageInfo) - 4)
      else
        FFileStream.Read(ImageInfo,SizeOf(TWMImageInfo));
    BitSize:=Imageinfo.nWidth*ImageInfo.nHeight*(FColorBit div 8);
    GetMem(Pbits,BitSize);
    FFileStream.Read(Pbits^,BitSize);
    {$POINTERMATH ON}
    case FColorBit of
      8:begin
        //32位目标纹理
        GetMem(PTexData,BitSize*4);
        //获取调色板像素 组合成纹理像素
        for I := 0 to BitSize-1 do
        begin
          ColorIndex:=PBits[i];
          Pixel:=FPalette[ColorIndex];
          PTexData[i]:=Pixel;
        end;
      end;
      16:begin
        GetMem(PTexData,BitSize*2);
        for i := 0 to ImageInfo.nWidth*ImageInfo.nHeight-1 do
        begin
          Pbits16:=PWord(Pbits);
          Pixel16:=Pbits16[i];
          Pixel:=RGB565ToABGR(Pixel16);
          PTexData[i]:=Pixel;
        end;
      end;
      24:begin

      end;
      32:begin


      end;
    end;
    {$POINTERMATH OFF}
      //创建纹理像素
      //PTexture:=tex_CreateZero(ImageInfo.nWidth,ImageInfo.nHeight);
      //MZTexture:=TMZTexture.Create(ImageInfo.nWidth,ImageInfo.nHeight,$FFFFFFFF,TEX_DEFAULT_2D);

      //tex_SetData(PTexture,Pointer(PTexData),0,0,ImageInfo.nWidth,ImageInfo.nHeight);
      MZTexture:=TMZTexture.create(ImageInfo.nWidth,ImageInfo.nHeight,$FFFFFFFF,[]);
      MZTexture.SetData(PTexData,0,0,ImageInfo.nWidth,ImageInfo.nHeight);
      FreeMem(Pbits,BitSize);
      FreeMem(PTexData);
      Result:=TTexture.Create(MZTexture,Imageinfo.Px,Imageinfo.Py);
end;

function TWilImage.GetTexture(idx: integer; AutoFree: Boolean): TTexture;
var
List:Tlist;
begin
Result:=nil;
if  not ((idx >= 0) and (idx < ImageCount) and (FFileStream <> Nil))  then Exit;
  if AutoFree then
  begin
    Result:=ReadyLoadTexture(idx);
    if Result=nil then
    begin
      Result:=GetCacheTexture(idx);
      List:=FTextureList.lockList;
      List[idx]:=Result;
      FTextureList.UnLockList;
    end;
  end else
  begin
    Result:=GetCacheTexture(idx);
  end;
end;

procedure TWilImage.Init;
var
  Header: TWMImageHeader;
  List:TList;
  Count:Integer;
begin
  inherited;
  try
  FFileStream:=TFileStream.Create(FFileName,fmOpenRead);
  except
  FFileStream.free;
  FFileStream:=nil;
  //输出Log 无法打开文件
  end;
  if FFileStream <> nil then
  begin
  //首先是读头,
    FFileStream.Read(Header,SizeOf(TWMImageHeader));

    if (Header.VerFlag = 0) or (Header.ColorCount = 65536) then
    begin
      FVersion := 1;
      FFileStream.seek(-4,soCurrent);
    end;

    case Header.ColorCount of
        256: FColorBit := 8;
        65536: FColorBit := 16;
        16777216: FColorBit := 24;
        else FColorBit := 32;
        end;

    FImageCount:=Header.ImageCount;
    //给纹理列表设置大小
    List:=FTextureList.lockList;
    List.Count:=FImageCount;
    FTextureList.UnLockList;
    //读色板
    LoadPalette;
    //读索引
    LoadIndex;
  end;
end;

procedure TWilImage.Loadindex;
var
  I: integer;
  Header: TWMIndexHeader;
  PValue: PInteger;
  idxFile:string;
  S:TFileStream;
  Value:Integer;
begin
S:=nil;
idxfile := ExtractFilePath(FFileName) + ExtractFileNameOnly(FFileName) + '.WIX';
  if FileExists(IdxFile) then begin
    try
      begin
        S:=TFileStream.Create(idxFile,fmOpenRead);
        if FVersion <> 0 then
        S.Read(Header, SizeOf(TWMIndexHeader) - 4)
        else
        S.Read(Header, SizeOf(TWMIndexHeader));
        SetLength(FIndex,Header.IndexCount);
        S.Read(FIndex[0], 4 * Header.IndexCount);
      end;
    finally
    s.Free;
    end;

  end;
end;

procedure TWilImage.LoadPalette;
var
i:Integer;
//F:TFileStream;
begin
  if FVersion <> 0 then
    FFileStream.Seek(SizeOf(TWMImageHeader) - 4, soBeginning)
  else
  FFileStream.Seek(SizeOf(TWMImageHeader), soBeginning);
  FFileStream.Read(FPalette[0],1024);
  //读入调色板格式为ARGB 但是OPENGL使用的是RGBA.
  for I := 0 to 255 do
  begin
    FPalette[i]:=ARGBToABGR(FPalette[i]);
  end;
  //F:=TFileStream.Create('D:\wil.pal',fmCreate);
  //F.Write(FPalette[0],1024);
  //F.Free;

end;
end.
