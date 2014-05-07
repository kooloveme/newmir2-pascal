unit wmUtil;

interface
uses
 Windows, SysUtils, Graphics, DIB;
 
type
  TWZXIndexHeader = record//48
	  Title: string[43];//44
	  IndexCount: Integer;//4
  end;
  pTWZXIndexHeader = ^TWZXIndexHeader;

  {TWZXIndexInfo = record //实际用不到了 循环就行了
	  Position: Integer;
  end;
  pTWZXIndexInfo = ^TWZXIndexInfo;

  TWZXIndexInfoArray = array of TWZXIndexInfo; }

  TWZLImageHeader = record//48
	  Title: string[43];//44
	  ImageCount: Integer;//4
  end;
  pTWZLImageHeader = ^TWZLImageHeader;

  TWZLImageInfo = record//12
    m_Enc1: Byte; //1 3:8位  5:16位
    m_Enc2: Byte; //1 不清楚1
    m_type1: Byte; //1 A3可能是表示还需要载 只是猜测
    m_type2: Byte; //1 不清楚0
    m_nWidth: smallint; //2 宽度
    m_nHeight: smallint; //2 高度
    m_wPx: smallint; //2 x
    m_wPy: smallint; //2 y
    m_Len: Integer; // 压缩数据长度
  end;
  pTWZLImageInfo = ^TWZLImageInfo;

  TWMImageHeader = record  //wil图头
    Title: string[40]; //'WEMADE Entertainment inc.'
    ImageCount: Integer;
    ColorCount: Integer;
    PaletteSize: Integer;
    VerFlag: Integer;
  end;
  PTWMImageHeader = ^TWMImageHeader;

  TWMImageInfo = record //will图信息
    nWidth: SmallInt;
    nHeight: SmallInt;
    px: SmallInt;
    py: SmallInt;
    bits: PByte;
  end;
  PTWMImageInfo = ^TWMImageInfo;

  TWMIndexHeader = record //wil索引头
    Title: string[40]; //'WEMADE Entertainment inc.'
    IndexCount: integer;
    VerFlag: integer;
  end;

  PTWMIndexHeader = ^TWMIndexHeader;

  TWMIndexInfo = record  //will索引信息
    Position: Integer;
    Size: Integer;
  end;
  PTWMIndexInfo = ^TWMIndexInfo;










  //=======================wis=========================
  TWisFileHeaderInfo = packed record
    nTitle: Integer; //04 $41534957 = WISA
    VerFlag: Integer; //01
    Reserve1: Integer; //00
    DateTime: TDateTime;
    Reserve2: Integer;
    Reserve3: Integer;
    CopyRight: string[20];
    aTemp1: array[1..107] of Char;
    nHeaderEncrypt: Integer; //0XA0
    nHeaderLen: Integer; //0XA4
    nImageCount: Integer; //0XA8
    nHeaderData: Integer; //0XAC
    aTemp2: array[1..$200 - $AC] of Char;
  end;
  PWisFileHeaderInfo = ^TWisFileHeaderInfo;

  TWisHeader = packed record
    OffSet: Integer;
    Length: Integer;
    temp3: Integer;
  end;
  PTWisHeader = ^TWisHeader;

  TWisFileHeaderArray = array of TWisHeader;

  TImgInfo = packed record
    btEncr0: Byte; //0X00
    btEncr1: Byte; //0X01
    bt2: Byte; //0X02
    bt3: Byte; //0X03
    wW: Smallint; //0X04
    wH: Smallint; //0X06
    wPx: Smallint; //0X08
    wPy: Smallint; //0X0A
  end;
  PTImgInfo = ^TImgInfo;
  //============================Data=====================================

 { TDataHeader = record //Data文件头
    Title: string[40];
    Size: Integer;
    ImageCount: Integer;
    IndexOffSet: Integer;
    BitCount: Word;
    Compression: Word;
  end;
  pTDataHeader = ^TDataHeader;

  TDataImageInfo = record //Data图片信息
    nWidth: Smallint;
    nHeight: Smallint;
    Px: Smallint;
    Py: SmallInt;
    nSize: Integer; //数据大小
  end;
  pTDataImageInfo = ^TDataImageInfo; }
  TDataHeader = record //iamwgh新定义的ids文件头
    Title: string[40];
    Size: DWORD;
    ImageCount: DWORD;
    Planes: LongWord;
    BitCount: word;
    Compression: word;
  end;
  PTDataHeader = ^TDataHeader;

  TDataImageInfo = record //iamwgh新定义ids图片信息
    nWidth: SmallInt;
    nHeight: SmallInt;
    px: SmallInt;
    py: SmallInt;
    nSize: uint; //数据大小
  end;
  PTDataImageInfo = ^TDataImageInfo;


function WidthBytes(w: integer): integer;
//function PaletteFromBmpInfo(BmpInfo: PBitmapInfo): HPalette;
//function MakeBmp(w, h: integer; bits: pointer; pal: TRGBQuads): TBitmap;
//procedure DrawBits(Canvas: TCanvas; XDest, YDest: integer; PSource: PByte; Width, Height: integer);
implementation

function WidthBytes(w: integer): integer;
begin
  Result := (((w * 8) + 31) div 32) * 4;
end;

function PaletteFromBmpInfo(BmpInfo: PBitmapInfo): HPalette;
var
  PalSize, n: integer;
  Palette: PLogPalette;
begin
     //Allocate Memory for Palette
  PalSize := SizeOf(TLogPalette) + (256 * SizeOf(TPaletteEntry));
  Palette := AllocMem(PalSize);

     //Fill in structure
  with Palette^ do
  begin
    palVersion := $300;
    palNumEntries := 256;
    for n := 0 to 255 do
    begin
      palPalEntry[n].peRed := BmpInfo^.bmiColors[n].rgbRed;
      palPalEntry[n].peGreen := BmpInfo^.bmiColors[n].rgbGreen;
      palPalEntry[n].peBlue := BmpInfo^.bmiColors[n].rgbBlue;
      palPalEntry[n].peFlags := 0;
    end;
  end;
  Result := CreatePalette(Palette^);
  FreeMem(Palette, PalSize);
end;

procedure CreateDIB256(var bmp: TBitmap; BmpInfo: PBitmapInfo; bits: PByte);
var
  DC, MemDc: hdc;
  OldPal: HPalette;
begin
  DC := 0;
  MemDc := 0; //jacky
   //First Release Handle and Palette from BMP
  DeleteObject(bmp.ReleaseHandle);
  DeleteObject(bmp.ReleasePalette);

  try
    DC := GetDC(0);
    try
      MemDc := CreateCompatibleDC(DC);
      DeleteObject(SelectObject(MemDc, CreateCompatibleBitmap(DC, 1, 1)));

      OldPal := 0;
      bmp.Palette := PaletteFromBmpInfo(BmpInfo);
      OldPal := SelectPalette(MemDc, bmp.Palette, False);
      RealizePalette(MemDc);
      try
        bmp.handle := CreateDIBitmap(MemDc, BmpInfo^.bmiHeader, CBM_INIT,
          pointer(bits), BmpInfo^, DIB_RGB_COLORS);
      finally
        if OldPal <> 0 then
          SelectPalette(MemDc, OldPal, True);
      end;
    finally
      if MemDc <> 0 then
        DeleteDC(MemDc);
    end;
  finally
    if DC <> 0 then
      ReleaseDC(0, DC);
  end;
  if bmp.handle = 0 then
    Exception.Create('CreateDIBitmap failed');
end;

function MakeBmp(w, h: integer; bits: pointer; pal: TRGBQuads): TBitmap;
var
  I: integer;
  BmpInfo: PBitmapInfo;
  HeaderSize: integer;
  bmp: TBitmap;
begin
  HeaderSize := SizeOf(TBitmapInfo) + (256 * SizeOf(TRGBQuad));
  GetMem(BmpInfo, HeaderSize);
  for I := 0 to 255 do begin
    BmpInfo.bmiColors[I] := pal[I];
  end;
  with BmpInfo^.bmiHeader do begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := w;
    biHeight := h;
    biPlanes := 1;
    biBitCount := 8; //8bit
    biCompression := BI_RGB;
    biClrUsed := 0;
    biClrImportant := 0;
  end;
  bmp := TBitmap.Create;
  CreateDIB256(bmp, BmpInfo, bits);
  FreeMem(BmpInfo);
  Result := bmp;
end;

procedure DrawBits(Canvas: TCanvas; XDest, YDest: integer; PSource: PByte; Width, Height: integer);
var
  HeaderSize: integer;
  BmpInfo: PBitmapInfo;
begin
  if PSource = nil then Exit;

  HeaderSize := SizeOf(TBitmapInfo) + (256 * SizeOf(TRGBQuad));
  BmpInfo := AllocMem(HeaderSize);
  if BmpInfo = nil then raise Exception.Create('TNoryImg: Failed to allocate a DIB');
  with BmpInfo^.bmiHeader do begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := Width;
    biHeight := -Height;
    biPlanes := 1;
    biBitCount := 8;
    biCompression := BI_RGB;
    biClrUsed := 0;
    biClrImportant := 0;
  end;
  SetDIBitsToDevice(Canvas.handle, XDest, YDest, Width, Height, 0, 0, 0, Height,
    PSource, BmpInfo^, DIB_RGB_COLORS);
  FreeMem(BmpInfo, HeaderSize);
end;

end.
