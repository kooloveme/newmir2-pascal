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
    m_nArr_Palette:array[0..255] of Cardinal;
    m_nVersion:Integer;
    m_nColorBit:Integer;
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
  m_sFileName:=FileName;
  m_nVersion:=0;
end;

destructor TWilImage.Destroy;
begin

  inherited;
end;

function TWilImage.GetCacheTexture(idx: Integer): TTexture;
var
  MZTexture : TMZTexture;
  ImageInfo : TWMImageInfo;
  i,OffSet,bitSize,Pixel : Integer;
  ColorIndex : Byte;
  Pbits : PByte;

  PBits16  : PWord;
  Pixel16  : Word;
  PTexData : PInteger;
  PTexture : zglPTexture;
begin
  Result:=nil;
  if  not ((idx >= 0) and (idx < ImageCount) and (m_FileStream <> Nil))  then Exit;
  OffSet:=m_nArr_Index[idx];
  m_FileStream.seek(OffSet,soBeginning);
  if m_nVersion <> 0 then
    m_FileStream.Read(ImageInfo,SizeOf(TWMImageInfo) - 4)
  else
   m_FileStream.Read(ImageInfo,SizeOf(TWMImageInfo));

  BitSize:=Imageinfo.nWidth*ImageInfo.nHeight*(m_nColorBit div 8);
  GetMem(Pbits,BitSize);
  m_FileStream.Read(Pbits^,BitSize);
    {$POINTERMATH ON}
  case m_nColorBit of
    8:begin
        //32位目标纹理
        GetMem(PTexData,BitSize*4);
        //获取调色板像素 组合成纹理像素
        for I := 0 to BitSize-1 do
        begin
          ColorIndex:=PBits[i];
          Pixel:=m_nArr_Palette[ColorIndex];
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
  if  not ((idx >= 0) and (idx < ImageCount) and (m_FileStream <> Nil))  then Exit;
  if AutoFree then
  begin
    Result:=ReadyLoadTexture(idx);
    if Result=nil then
    begin
      Result:=GetCacheTexture(idx);
      List:=m_TextureList.lockList;
      List[idx]:=Result;
      m_TextureList.UnLockList;
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
    m_FileStream:=TFileStream.Create(m_sFileName,fmOpenRead);
  except
    m_FileStream.free;
    m_FileStream:=nil;
    TMZLog.Log('Can not Read:'+m_sFileName);
  end;
  if m_FileStream <> nil then
  begin
  //首先是读头,
    m_FileStream.Read(Header,SizeOf(TWMImageHeader));

    if (Header.VerFlag = 0) or (Header.ColorCount = 65536) then
    begin
      m_nVersion := 1;
      m_FileStream.seek(-4,soCurrent);
    end;

    case Header.ColorCount of
      256: m_nColorBit := 8;
      65536: m_nColorBit := 16;
      16777216: m_nColorBit := 24;
    else m_nColorBit := 32;
    end;

    m_nFImageCount:=Header.ImageCount;
    //给纹理列表设置大小
    List:=m_TextureList.lockList;
    List.Count:=m_nFImageCount;
    m_TextureList.UnLockList;
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
  idxfile := ExtractFilePath(m_sFileName) + ExtractFileNameOnly(m_sFileName) + '.WIX';
  if FileExists(IdxFile) then begin
    try
      begin
        S:=TFileStream.Create(idxFile,fmOpenRead);
        if m_nVersion <> 0 then
        S.Read(Header, SizeOf(TWMIndexHeader) - 4)
        else
        S.Read(Header, SizeOf(TWMIndexHeader));
        SetLength(m_nArr_Index,Header.IndexCount);
        S.Read(m_nArr_Index[0], 4 * Header.IndexCount);
      end;
    finally
    s.Free;
    end;

  end;
end;

procedure TWilImage.LoadPalette;
var
i:Integer;
begin
  if m_nVersion <> 0 then
    m_FileStream.Seek(SizeOf(TWMImageHeader) - 4, soBeginning)
  else
  m_FileStream.Seek(SizeOf(TWMImageHeader), soBeginning);
  m_FileStream.Read(m_nArr_Palette[0],1024);
  //读入调色板格式为ARGB 但是OPENGL使用的是RGBA.
  for I := 0 to 255 do
  begin
    m_nArr_Palette[i]:=ARGBToABGR(m_nArr_Palette[i]);
  end;
end;
end.
