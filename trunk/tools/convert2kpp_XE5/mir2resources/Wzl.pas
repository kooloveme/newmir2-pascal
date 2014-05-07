unit Wzl;

interface

uses
  Windows, SysUtils, Classes, Graphics, wmUtil, GameImages, Texture, DIB, HUtil32, ZlibEx, CompressUnit, CompressUnit1;

type
  pTWzlImages = ^TWzlImages;
  TWzlImages = class(TGameImages)

  private
    FCompress: Integer; //压缩算法类型
    FHeader: TWZLImageHeader;
    function UnComp(const InData: Pointer; InSize: LongInt; out OutData: Pointer): Integer;
    procedure LoadIndex(sIdxFile: string); //加载索引
    procedure LoadDxImage(Position: Integer; DXImage: pTDXImage);
  protected
    function GetCachedSurface(Index: Integer): TTexture; override;
  public
    m_IndexList: TList;
    m_FileStream: TFileStream; //TMapStream; //TFileStream;

    constructor Create(); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure ReverseDIB(DIB: TDIB);
    Function GetBitmap(Index: Integer; var PX, PY: Integer): TBitmap;override;
    function GetCachedImage(Index: Integer; var PX, PY: Integer): TTexture; override;
  end;

implementation
uses Math;
var
  MainPalette: TRGBQuads;
{ TWzlImages }

procedure UnCompressRle(const InData: Pointer; InSize: LongInt; out OutData: Pointer; out OutSize: LongInt);
var
  I, J, K: integer;
  wsbuf, wdbuf: PWordArray;
begin
  wsbuf := PWordArray(InData);
  wdbuf := PWordArray(OutData);
  J := 0;
  I := 0;
  while I < InSize div 2 do begin
    if (wsbuf[I] = $AAAA) then begin
      for K := 0 to wsbuf[I + 2] - 1 do begin
        wdbuf[J] := wsbuf[I + 1];
        Inc(J);
      end;
      Inc(I, 2);
    end else begin
      wdbuf[J] := wsbuf[I];
      Inc(J);
    end;
    Inc(I);
  end;
  OutSize := J * 2;
end;

constructor TWzlImages.Create();
begin
  inherited;
  m_FileStream := nil;
  m_IndexList := TList.Create;
end;

destructor TWzlImages.Destroy;
begin
  m_IndexList.Free;
  inherited;
end;

procedure TextOutStr(Msg: string); //未用
var
  flname: string;
  fhandle: TextFile;
begin
  flname := '.\Text.txt';
  if FileExists(flname) then begin
    AssignFile(fhandle, flname);
    Append(fhandle);
  end else begin
    AssignFile(fhandle, flname);
    Rewrite(fhandle);
  end;
  Writeln(fhandle, TimeToStr(Time) + ' ' + Msg);
  CloseFile(fhandle);
end;

procedure TWzlImages.Initialize;
var
  idxfile: string;
begin
  if not Initialized then begin
    if FileExists(FileName) then begin //加栽wzl  文件名FileName由TGameImages得到
      m_FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
      m_FileStream.Read(FHeader, SizeOf(TWZLImageHeader));
      idxfile := ExtractFilePath(FileName) + ExtractFileNameOnly(FileName) + '.Wzx';
      LoadIndex(idxfile); //加载索引
      ImageCount := FHeader.ImageCount; //全局的
      m_ImgArr := AllocMem(SizeOf(TDXImage) * ImageCount);
      Initialized := True;
    end;
  end;
end;

procedure TWzlImages.Finalize;
var
  I: Integer;
begin
  if Initialized then begin
    Initialized := False;
    IndexList.Clear;
    if m_ImgArr <> nil then begin
      for I := 0 to ImageCount - 1 do begin
        if m_ImgArr[I].Texture <> nil then begin
          m_ImgArr[I].Texture.Free;
          m_ImgArr[I].Texture := nil;
        end;
        if m_ImgArr[I].Bitmap <> nil then begin
          m_ImgArr[I].Bitmap.Free;
          m_ImgArr[I].Bitmap := nil;
        end;
      end;
      FreeMem(m_ImgArr);
    end;

    m_ImgArr := nil;
    ImageCount := 0;
    if m_FileStream <> nil then
      FreeAndNil(m_FileStream);
  end;
end;

procedure TWzlImages.LoadIndex(sIdxFile: string);
var
  FHandle, I, Value: integer;
  Header: TWZXIndexHeader;
  PValue: PInteger;
begin
  m_IndexList.Clear;
  if FileExists(sIdxFile) then begin
    FHandle := FileOpen(sIdxFile, fmOpenRead or fmShareDenyNone);
    if FHandle > 0 then begin
      FileRead(FHandle, Header, SizeOf(TWZXIndexHeader)); //取wzx头部

      GetMem(PValue, 4 * Header.IndexCount); //索引分配内存
      FileRead(FHandle, PValue^, 4 * Header.IndexCount); //读取
      for I := 0 to Header.IndexCount - 1 do begin //取integer数组
        Value := PInteger(Integer(PValue) + 4 * I)^;
        m_IndexList.Add(Pointer(Value)); //加到Tlist
      end;
      FreeMem(PValue);
      FileClose(FHandle);
    end;
  end;
end;

{----------------- Private Variables ---------------------}

procedure TWzlImages.LoadDxImage(Position: Integer; DXImage: pTDXImage);
var
  I, J, nError: Integer;
  ImageInfo: TWZLImageInfo;
  S: Pointer;
  SrcP: PByte;
  DesP: PByte;
  nSize: Integer;
  RGB: TRGBQuad;
  Source: TDIB;
begin
  try
    m_FileStream.Position := Position;
    m_FileStream.Read(ImageInfo, SizeOf(TWZLImageInfo)); //头
    if (ImageInfo.m_nWidth * ImageInfo.m_nHeight <= 0) then Exit;
    case ImageInfo.m_Enc1 of
      3: begin
          Source := TDIB.Create;
          Source.SetSize(WidthBytes(ImageInfo.m_nWidth), ImageInfo.m_nHeight, 8);
          Source.ColorTable := MainPalette;
          Source.UpdatePalette;
          Source.Canvas.Brush.Color := clblack;
          Source.Canvas.FillRect(Source.Canvas.ClipRect);

          GetMem(S, ImageInfo.m_nWidth * ImageInfo.m_nHeight); //实际大小
          GetMem(SrcP, ImageInfo.m_Len); //压缩源大小
          try
            DesP := S; //实际大小
            m_FileStream.Read(SrcP^, ImageInfo.m_Len); //读压缩数据
            FCompress := 2;
            if ImageInfo.m_Len > 0 then begin
              nSize := UnComp(Pointer(SrcP), ImageInfo.m_Len, Pointer(DesP)); //解压缩位图数据
            end else
            begin
              nSize := ImageInfo.m_nWidth * ImageInfo.m_nHeight;
              m_FileStream.Read(DesP^, nSize);
            end;
            FreeMem(SrcP);
            SrcP := S;
            ReverseDIB(Source); //反转显示
 //(DXImage.Texture.Height - 1 - I)
              for J := 0 to ImageInfo.m_nWidth - 1 do begin
                RGB := MainPalette[SrcP^];
                if Integer(RGB) = 0 then begin
                  PWord(DesP)^ := 0;
                end else begin
              //PWord(DesP)^ := RGBColors[RGB.rgbRed, RGB.rgbGreen, RGB.rgbBlue];
                  PWord(DesP)^ := Word((Max(RGB.rgbRed and $F8, 8) shl 8) or (Max(RGB.rgbGreen and $FC, 8) shl 3) or (Max(RGB.rgbBlue and $F8, 8) shr 3)); //565格式
                end;
                Inc(SrcP);
                Inc(DesP, 2);
              end;
        //FreeMem(S);

          finally
            Source.Free;
            FreeMem(S);
          end;

        end;

      5: begin
          Source := TDIB.Create;
          Source.PixelFormat := MakeDIBPixelFormat(5, 6, 5);
          Source.SetSize(ImageInfo.m_nWidth, ImageInfo.m_nHeight, 16);

          try
            DesP := S; //实际大小
            m_FileStream.Read(SrcP^, ImageInfo.m_Len); //读压缩数据
            FCompress := 2;
            if ImageInfo.m_Len > 0 then begin
              nSize := UnComp(Pointer(SrcP), ImageInfo.m_Len, Pointer(DesP)); //解压缩位图数据
            end else begin
              nSize := ImageInfo.m_nWidth * ImageInfo.m_nHeight * 2;
              m_FileStream.Read(DesP^, nSize);
            end;

      //FreeMem(S);
          finally
            Source.Free;
            FreeMem(S);
          end;

        end;

    end;

  except
    //DebugOutStr('TWzlImages.LoadDxImage:' + IntToStr(nError));
  end;
end;

function TWzlImages.GetCachedSurface(Index: Integer): TTexture;
var
  nPosition: Integer;
  nErrCode: Integer;
begin
  Result := nil;
  try
    nErrCode := 0;
    if (Index >= 0) and (Index < ImageCount) and (m_FileStream <> nil) and (Initialized) then begin

      if GetTickCount - m_dwMemChecktTick > 1000 * 5 then begin
        m_dwMemChecktTick := GetTickCount;
        FreeOldMemorys(Index);
      end;

      nErrCode := 4;
      if m_ImgArr[Index].Texture = nil then begin
        nErrCode := 5;
        if Index < m_IndexList.Count then begin
          IndexList.Add(Pointer(Index)); //IndexList优化释放内存
          nErrCode := 6;
          nPosition := Integer(m_IndexList[Index]); //做个转换没用结构体数组方式
          nErrCode := 7;
          LoadDxImage(nPosition, @m_ImgArr[Index]); //加载图
          nErrCode := 8;
          m_ImgArr[Index].dwLatestTime := GetTickCount;
          Result := m_ImgArr[Index].Texture;
        end;
      end else begin
        m_ImgArr[Index].dwLatestTime := GetTickCount;
        Result := m_ImgArr[Index].Texture;
      end;
    end;
  except
    Result := nil;
      //DebugOutStr('TWzlImages.GetCachedSurface Index: ' + IntToStr(Index) + ' Error Code: ' + IntToStr(nErrCode));
  end;
end;

function TWzlImages.GetBitmap(Index: Integer; var PX, PY: Integer): TBitmap;
var
  I, J, nError,Position: Integer;
  ImageInfo: TWZLImageInfo;
  S: Pointer;
  SrcP: PByte;
  DesP: PByte;
  nSize: Integer;
  RGB: TRGBQuad;
  Source: TDIB;
begin
Result:=nil;
if not (Index in[0..ImageCount]) then exit;

Position:=Integer(m_IndexList[Index]);
  try
    m_FileStream.Position := Position;
    m_FileStream.Read(ImageInfo, SizeOf(TWZLImageInfo)); //头
    PX:=ImageInfo.m_wPx;
    PY:=ImageInfo.m_wPy;
    if (ImageInfo.m_nWidth * ImageInfo.m_nHeight <= 0) then Exit;
    case ImageInfo.m_Enc1 of
      3: begin
          Source := TDIB.Create;
          Source.SetSize(WidthBytes(ImageInfo.m_nWidth), ImageInfo.m_nHeight, 8);
          Source.ColorTable := MainPalette;
          Source.UpdatePalette;
          Source.Canvas.Brush.Color := clblack;
          Source.Canvas.FillRect(Source.Canvas.ClipRect);

          GetMem(S, ImageInfo.m_nWidth * ImageInfo.m_nHeight); //实际大小
          GetMem(SrcP, ImageInfo.m_Len); //压缩源大小
          try
            DesP := S; //实际大小
            m_FileStream.Read(SrcP^, ImageInfo.m_Len); //读压缩数据
            FCompress := 2;
            if ImageInfo.m_Len > 0 then begin
              nSize := UnComp(Pointer(SrcP), ImageInfo.m_Len, Pointer(DesP)); //解压缩位图数据
            end else
            begin
              nSize := ImageInfo.m_nWidth * ImageInfo.m_nHeight;
              m_FileStream.Read(DesP^, nSize);
            end;
            FreeMem(SrcP);
            SrcP := S;
            ReverseDIB(Source); //反转显示
            Result:=TBitmap.Create;
            Result.SetSize(ImageInfo.m_nWidth,ImageInfo.m_nHeight);
            Result.PixelFormat:=pf8bit;
            Result.Canvas.Draw(0,0,source);

          finally
            Source.Free;
            FreeMem(S);
          end;

        end;

      5: begin
          Source := TDIB.Create;
          Source.PixelFormat := MakeDIBPixelFormat(5, 6, 5);
          Source.SetSize(ImageInfo.m_nWidth, ImageInfo.m_nHeight, 16);

          try
            DesP := S; //实际大小
            m_FileStream.Read(SrcP^, ImageInfo.m_Len); //读压缩数据
            FCompress := 2;
            if ImageInfo.m_Len > 0 then begin
              nSize := UnComp(Pointer(SrcP), ImageInfo.m_Len, Pointer(DesP)); //解压缩位图数据
            end else begin
              nSize := ImageInfo.m_nWidth * ImageInfo.m_nHeight * 2;
              m_FileStream.Read(DesP^, nSize);
            end;

            Result:=TBitmap.Create;

            Result.SetSize(ImageInfo.m_nWidth,ImageInfo.m_nHeight);
            Result.PixelFormat:=pf16bit;
            Result.Canvas.Draw(0,0,source);
          finally
            Source.Free;
            FreeMem(S);
          end;

        end;

    end;

  except
    //DebugOutStr('TWzlImages.LoadDxImage:' + IntToStr(nError));
  end;
end;

function TWzlImages.GetCachedImage(Index: Integer; var PX, PY: Integer): TTexture;
var
  nPosition: integer;
  nErrCode: integer;
  Name: string;
begin
  Result := nil;
  try
    nErrCode := 0;
    if (Index >= 0) and (Index < ImageCount) and (m_FileStream <> nil) and (Initialized) then begin
      nErrCode := 1;
      if GetTickCount - m_dwMemChecktTick > 1000 * 5 then begin
        m_dwMemChecktTick := GetTickCount;
        nErrCode := 2;
        FreeOldMemorys(Index);
        nErrCode := 3;
      end;
      nErrCode := 4;
      if m_ImgArr[Index].Texture = nil then begin
        nErrCode := 5;
        if Index < m_IndexList.Count then begin
          IndexList.Add(Pointer(Index));
          nErrCode := 6;
          nPosition := Integer(m_IndexList[Index]);
          nErrCode := 7;
          if LibType = ltUseCache then begin
            LoadDxImage(nPosition, @m_ImgArr[Index]);
          end else begin
            //LoadDxBitmap(nPosition, @m_ImgArr[Index]);
          end;
          nErrCode := 8;
          m_ImgArr[Index].dwLatestTime := GetTickCount;
          PX := m_ImgArr[Index].nPx;
          PY := m_ImgArr[Index].nPy;
          Result := m_ImgArr[Index].Texture;
        end;
      end else begin
        m_ImgArr[Index].dwLatestTime := GetTickCount;
        PX := m_ImgArr[Index].nPx;
        PY := m_ImgArr[Index].nPy;
        Result := m_ImgArr[Index].Texture;
      end;
    end;
  except
    Result := nil;
  end;
end;

function TWzlImages.UnComp(const InData: Pointer; InSize: Integer;
  out OutData: Pointer): Integer;
begin
  case FCompress of
    1: UnCompressRle(InData, InSize, OutData, Result);
    2: DecompressBufZ(InData, InSize, 0, OutData, Result);
    3: UnCompressBufferL(InData, InSize, OutData, Result);
    4: UnCompBufferL(InData, InSize, OutData, Result);
  end;
end;

var
  ColorArray: array[0..1023] of Byte = (
    $00, $00, $00, $00, $00, $00, $80, $00, $00, $80, $00, $00, $00, $80, $80, $00,
    $80, $00, $00, $00, $80, $00, $80, $00, $80, $80, $00, $00, $C0, $C0, $C0, $00,
    $97, $80, $55, $00, $C8, $B9, $9D, $00, $73, $73, $7B, $00, $29, $29, $2D, $00,
    $52, $52, $5A, $00, $5A, $5A, $63, $00, $39, $39, $42, $00, $18, $18, $1D, $00,
    $10, $10, $18, $00, $18, $18, $29, $00, $08, $08, $10, $00, $71, $79, $F2, $00,
    $5F, $67, $E1, $00, $5A, $5A, $FF, $00, $31, $31, $FF, $00, $52, $5A, $D6, $00,
    $00, $10, $94, $00, $18, $29, $94, $00, $00, $08, $39, $00, $00, $10, $73, $00,
    $00, $18, $B5, $00, $52, $63, $BD, $00, $10, $18, $42, $00, $99, $AA, $FF, $00,
    $00, $10, $5A, $00, $29, $39, $73, $00, $31, $4A, $A5, $00, $73, $7B, $94, $00,
    $31, $52, $BD, $00, $10, $21, $52, $00, $18, $31, $7B, $00, $10, $18, $2D, $00,
    $31, $4A, $8C, $00, $00, $29, $94, $00, $00, $31, $BD, $00, $52, $73, $C6, $00,
    $18, $31, $6B, $00, $42, $6B, $C6, $00, $00, $4A, $CE, $00, $39, $63, $A5, $00,
    $18, $31, $5A, $00, $00, $10, $2A, $00, $00, $08, $15, $00, $00, $18, $3A, $00,
    $00, $00, $08, $00, $00, $00, $29, $00, $00, $00, $4A, $00, $00, $00, $9D, $00,
    $00, $00, $DC, $00, $00, $00, $DE, $00, $00, $00, $FB, $00, $52, $73, $9C, $00,
    $4A, $6B, $94, $00, $29, $4A, $73, $00, $18, $31, $52, $00, $18, $4A, $8C, $00,
    $11, $44, $88, $00, $00, $21, $4A, $00, $10, $18, $21, $00, $5A, $94, $D6, $00,
    $21, $6B, $C6, $00, $00, $6B, $EF, $00, $00, $77, $FF, $00, $84, $94, $A5, $00,
    $21, $31, $42, $00, $08, $10, $18, $00, $08, $18, $29, $00, $00, $10, $21, $00,
    $18, $29, $39, $00, $39, $63, $8C, $00, $10, $29, $42, $00, $18, $42, $6B, $00,
    $18, $4A, $7B, $00, $00, $4A, $94, $00, $7B, $84, $8C, $00, $5A, $63, $6B, $00,
    $39, $42, $4A, $00, $18, $21, $29, $00, $29, $39, $46, $00, $94, $A5, $B5, $00,
    $5A, $6B, $7B, $00, $94, $B1, $CE, $00, $73, $8C, $A5, $00, $5A, $73, $8C, $00,
    $73, $94, $B5, $00, $73, $A5, $D6, $00, $4A, $A5, $EF, $00, $8C, $C6, $EF, $00,
    $42, $63, $7B, $00, $39, $56, $6B, $00, $5A, $94, $BD, $00, $00, $39, $63, $00,
    $AD, $C6, $D6, $00, $29, $42, $52, $00, $18, $63, $94, $00, $AD, $D6, $EF, $00,
    $63, $8C, $A5, $00, $4A, $5A, $63, $00, $7B, $A5, $BD, $00, $18, $42, $5A, $00,
    $31, $8C, $BD, $00, $29, $31, $35, $00, $63, $84, $94, $00, $4A, $6B, $7B, $00,
    $5A, $8C, $A5, $00, $29, $4A, $5A, $00, $39, $7B, $9C, $00, $10, $31, $42, $00,
    $21, $AD, $EF, $00, $00, $10, $18, $00, $00, $21, $29, $00, $00, $6B, $9C, $00,
    $5A, $84, $94, $00, $18, $42, $52, $00, $29, $5A, $6B, $00, $21, $63, $7B, $00,
    $21, $7B, $9C, $00, $00, $A5, $DE, $00, $39, $52, $5A, $00, $10, $29, $31, $00,
    $7B, $BD, $CE, $00, $39, $5A, $63, $00, $4A, $84, $94, $00, $29, $A5, $C6, $00,
    $18, $9C, $10, $00, $4A, $8C, $42, $00, $42, $8C, $31, $00, $29, $94, $10, $00,
    $10, $18, $08, $00, $18, $18, $08, $00, $10, $29, $08, $00, $29, $42, $18, $00,
    $AD, $B5, $A5, $00, $73, $73, $6B, $00, $29, $29, $18, $00, $4A, $42, $18, $00,
    $4A, $42, $31, $00, $DE, $C6, $63, $00, $FF, $DD, $44, $00, $EF, $D6, $8C, $00,
    $39, $6B, $73, $00, $39, $DE, $F7, $00, $8C, $EF, $F7, $00, $00, $E7, $F7, $00,
    $5A, $6B, $6B, $00, $A5, $8C, $5A, $00, $EF, $B5, $39, $00, $CE, $9C, $4A, $00,
    $B5, $84, $31, $00, $6B, $52, $31, $00, $D6, $DE, $DE, $00, $B5, $BD, $BD, $00,
    $84, $8C, $8C, $00, $DE, $F7, $F7, $00, $18, $08, $00, $00, $39, $18, $08, $00,
    $29, $10, $08, $00, $00, $18, $08, $00, $00, $29, $08, $00, $A5, $52, $00, $00,
    $DE, $7B, $00, $00, $4A, $29, $10, $00, $6B, $39, $10, $00, $8C, $52, $10, $00,
    $A5, $5A, $21, $00, $5A, $31, $10, $00, $84, $42, $10, $00, $84, $52, $31, $00,
    $31, $21, $18, $00, $7B, $5A, $4A, $00, $A5, $6B, $52, $00, $63, $39, $29, $00,
    $DE, $4A, $10, $00, $21, $29, $29, $00, $39, $4A, $4A, $00, $18, $29, $29, $00,
    $29, $4A, $4A, $00, $42, $7B, $7B, $00, $4A, $9C, $9C, $00, $29, $5A, $5A, $00,
    $14, $42, $42, $00, $00, $39, $39, $00, $00, $59, $59, $00, $2C, $35, $CA, $00,
    $21, $73, $6B, $00, $00, $31, $29, $00, $10, $39, $31, $00, $18, $39, $31, $00,
    $00, $4A, $42, $00, $18, $63, $52, $00, $29, $73, $5A, $00, $18, $4A, $31, $00,
    $00, $21, $18, $00, $00, $31, $18, $00, $10, $39, $18, $00, $4A, $84, $63, $00,
    $4A, $BD, $6B, $00, $4A, $B5, $63, $00, $4A, $BD, $63, $00, $4A, $9C, $5A, $00,
    $39, $8C, $4A, $00, $4A, $C6, $63, $00, $4A, $D6, $63, $00, $4A, $84, $52, $00,
    $29, $73, $31, $00, $5A, $C6, $63, $00, $4A, $BD, $52, $00, $00, $FF, $10, $00,
    $18, $29, $18, $00, $4A, $88, $4A, $00, $4A, $E7, $4A, $00, $00, $5A, $00, $00,
    $00, $88, $00, $00, $00, $94, $00, $00, $00, $DE, $00, $00, $00, $EE, $00, $00,
    $00, $FB, $00, $00, $94, $5A, $4A, $00, $B5, $73, $63, $00, $D6, $8C, $7B, $00,
    $D6, $7B, $6B, $00, $FF, $88, $77, $00, $CE, $C6, $C6, $00, $9C, $94, $94, $00,
    $C6, $94, $9C, $00, $39, $31, $31, $00, $84, $18, $29, $00, $84, $00, $18, $00,
    $52, $42, $4A, $00, $7B, $42, $52, $00, $73, $5A, $63, $00, $F7, $B5, $CE, $00,
    $9C, $7B, $8C, $00, $CC, $22, $77, $00, $FF, $AA, $DD, $00, $2A, $B4, $F0, $00,
    $9F, $00, $DF, $00, $B3, $17, $E3, $00, $F0, $FB, $FF, $00, $A4, $A0, $A0, $00,
    $80, $80, $80, $00, $00, $00, $FF, $00, $00, $FF, $00, $00, $00, $FF, $FF, $00,
    $FF, $00, $00, $00, $FF, $00, $FF, $00, $FF, $FF, $00, $00, $FF, $FF, $FF, $00
    );

procedure TWzlImages.ReverseDIB(DIB: TDIB);
var
  I: Integer;
  lsDIB: TDIB;
  SrcP: PByte;
  DesP: PByte;
begin
  lsDIB := TDIB.Create;
  lsDIB.Assign(DIB);
  DesP := DIB.PBits;
  for I := lsDIB.Height - 1 downto 0 do begin
    SrcP := Pointer(Integer(lsDIB.PBits) + I * lsDIB.WidthBytes);
    Move(SrcP^, DesP^, DIB.WidthBytes);
    Inc(Integer(DesP), DIB.WidthBytes);
  end;
  lsDIB.Free;

end;

initialization
  begin
    Move(ColorArray, MainPalette, SizeOf(ColorArray));
  end;
end.

