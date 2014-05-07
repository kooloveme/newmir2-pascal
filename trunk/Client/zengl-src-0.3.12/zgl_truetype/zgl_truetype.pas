unit zgl_truetype;
{$mode objfpc}
interface

uses
  Classes, SysUtils, FreeType, TTTypes, TTRASTER, Types, zgl_math_2d;

type
  TGlyphRenderQuality = (grqMonochrome, grqLowQuality, grqHighQuality);
  ArrayOfSingle= array of single;
  
  TFreeTypeInformation = (ftiCopyrightNotice, ftiFamily, ftiStyle, ftiIdentifier, ftiFullName,
     ftiVersionString, ftiPostscriptName, ftiTrademark, ftiManufacturer, ftiDesigner,
     ftiVendorURL, ftiDesignerURL, ftiLicenseDescription, ftiLicenseInfoURL);

const
  FreeTypeInformationStr : array[TFreeTypeInformation] of string =
    ('Copyright notice', 'Family', 'Style', 'Identifier', 'Full name',
     'Version string', 'Postscript name', 'Trademark', 'Manufacturer', 'Designer',
     'Vendor URL', 'Designer URL', 'License description', 'License info URL');

type
  TFreeTypeGlyph = class;
  
  TGlyphInfo = record
   CharCode : integer;
   Glyph: TFreeTypeGlyph;
  end;
  
  TGlyphArray = array of TGlyphInfo;
  

{********************************* Font implementation **********************************}

  { TFreeTypeFont }
  TFreeTypeFont = class
  private
    FName: String;
    FPointSize: single;
    FBold, FItalic, FHint, fUnderLine, fStrikeOut: boolean;
    FWidthFactor: single;
    FClearType: boolean;
    fColor : LongWord;
    fAlpha : Byte;
    FNamesArray: array of string;
    function GetCharIndex(AChar: integer): integer;
    function GetDPI: integer;
    function GetFamily: string;
    function GetGlyph(Index: integer): TFreeTypeGlyph;
    function GetGlyphCount: integer;
    function GetInformation(AIndex: TFreeTypeInformation): string;
    function GetPixelSize: single;
    function GetVersionNumber: string;
    procedure SetDPI(const AValue: integer);
    procedure UpdateFace(const AName: String);
    procedure DiscardFace;
    procedure DiscardInstance;
    function LoadGlyphInto(_glyph      : TT_Glyph;
                            glyph_index : Word): boolean;
    procedure SetWidthFactor(const AValue: single);
    procedure UpdateInstance;
    procedure UpdateSizeInPoints;
    procedure UpdateMetrics;
    procedure UpdateCharmap;
  protected
    FFace: TT_Face;
    FFaceLoaded: boolean;
    FInstance: TT_Instance;
    FInstanceCreated : boolean;
    FGlyphTable: TGlyphArray;
    FCharMap: TT_CharMap;
    FCharmapOk: boolean;
    FAscentValue, FDescentValue, FLineGapValue, FLargeLineGapValue: single;
    function GetClearType: boolean;
    procedure SetClearType(const AValue: boolean);
    function GetLineFullHeight: single;
    function GetAscent: single;
    function GetDescent: single;
    function GetLineSpacing: single;
    procedure FetchNames;
    procedure DestroyGlyphs;
    procedure RenderTextDirectly(x, y, tx: integer; data: pointer);
    procedure RenderTextClearDirectly(x, y, tx: integer; data: pointer);
  public
    Quality : TGlyphRenderQuality;
    SmallLinePadding: boolean;
    constructor Create(const AName: String; ASize: integer; AHint: boolean = true);
    destructor Destroy; override;
    procedure RenderText(AText: string; ARect: zglTRect);
    procedure RenderText(AText: string; x,y: single; ARect: TRect; OnRender : TDirectRenderingFunction);
    function TextWidth(AText: string): single;
    function TextHeight(AText: string): single;
    function CharsWidth(AText: string): ArrayOfSingle;
    property Name: String read FName;
    property DPI: integer read GetDPI write SetDPI;
    property SizeInPoints: single read FPointSize;
    property SizeInPixels: single read GetPixelSize;
    property Glyph[Index: integer]: TFreeTypeGlyph read GetGlyph;
    property GlyphCount: integer read GetGlyphCount;
    property CharIndex[AChar: integer]: integer read GetCharIndex;
    property Hinted: boolean read FHint;
    property Bold: boolean read FBold write fBold;
    property Italic: boolean read FItalic write fItalic;
    property UnderLine: boolean read FUnderLine write fUnderLine;
    property StrikeOut: boolean read FStrikeOut write fStrikeOut;
    property WidthFactor: single read FWidthFactor write SetWidthFactor;
    property LineFullHeight: single read GetLineFullHeight;
    property Information[AIndex: TFreeTypeInformation]: string read GetInformation;
    property VersionNumber: string read GetVersionNumber;
    property Family: string read GetFamily;
    property Color: longword read fColor write fColor;
    property Alpha: byte read fAlpha write fAlpha;
    property ClearType: boolean read GetClearType write SetClearType;
    property Ascent: single read GetAscent;
    property Descent: single read GetDescent;
    property LineSpacing: single read GetLineSpacing;
  end;

  { TFreeTypeGlyph }

  TFreeTypeGlyph = class
  private
    FLoaded, FItalic: boolean;
    FGlyphData: TT_Glyph;
    FIndex: integer;
    function GetAdvance: single;
    function GetBounds: TRect;
    function GetBoundsWithOffset(x, y: single): TRect;
  public
    constructor Create(AFont: TFreeTypeFont; AIndex: integer);
    function RenderDirectly(x,y: single; Rect: TRect; OnRender : TDirectRenderingFunction; quality : TGlyphRenderQuality; ClearType: boolean = false): boolean;
    function RenderDirectly(ARasterizer: TFreeTypeRasterizer; x,y: single; Rect: TRect; OnRender : TDirectRenderingFunction; quality : TGlyphRenderQuality; ClearType: boolean = false): boolean;
    destructor Destroy; override;
    property Loaded: boolean read FLoaded;
    property Data: TT_Glyph read FGlyphData;
    property Index: integer read FIndex;
    property Bounds: TRect read GetBounds;
    property BoundsWithOffset[x,y: single]: TRect read GetBoundsWithOffset;
    property Advance: single read GetAdvance;
    property Italic: boolean read FItalic write FItalic;
  end;

  { TFreeTypeRasterMap }

  TFreeTypeRasterMap = class
  protected
    map: TT_Raster_Map;
    FRasterizer: TFreeTypeRasterizer;
    function GetHeight: integer; virtual;
    function GetWidth: integer; virtual;
    function GetScanLine(y: integer): pointer;
    procedure Init(AWidth,AHeight: integer); virtual; abstract;
  public
    constructor Create(AWidth,AHeight: integer); virtual;
    constructor Create(ARasterizer: TFreeTypeRasterizer; AWidth,AHeight: integer); virtual;
    procedure Clear;
    procedure Fill;
    function RenderGlyph(glyph : TFreeTypeGlyph; x,y: single) : boolean; virtual; abstract;
    procedure ScanMoveTo(x,y: integer); virtual; abstract;
    destructor Destroy; override;

    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property ScanLine[y: integer]: pointer read GetScanLine;
  end;

  { TFreeTypeMonochromeMap }

  TFreeTypeMonochromeMap = class(TFreeTypeRasterMap)
  private
    ScanPtrStart,ScanPtrCur: pbyte;
    ScanBit: byte;
    ScanX: integer;
    function GetPixelsInHorizlineNoBoundsChecking(x,y,x2: integer) : integer; inline;
  protected
    procedure Init(AWidth,AHeight: integer); override;
  public
    function RenderGlyph(glyph : TFreeTypeGlyph; x,y: single) : boolean; override;
    procedure ScanMoveTo(x,y: integer); override;
    function ScanNextPixel: boolean;
    function GetPixel(x,y: integer): boolean;
    procedure SetPixel(x,y: integer; value: boolean);
    function GetPixelsInRect(x,y,x2,y2: integer): integer;
    function GetPixelsInHorizline(x,y,x2: integer): integer;
    procedure TogglePixel(x,y: integer);
  end;

  { TFreeTypeGrayscaleMap }

  TFreeTypeGrayscaleMap = class(TFreeTypeRasterMap)
  private
    ScanPtrStart: pbyte;
    ScanX: integer;
  protected
    procedure Init(AWidth, AHeight: integer); override;
  public
    RenderQuality: TGlyphRenderQuality;
    function RenderGlyph(glyph : TFreeTypeGlyph; x,y: single) : boolean; override;
    procedure ScanMoveTo(x,y: integer); override;
    function ScanNextPixel: byte;
    function GetPixel(x,y: integer): byte;
    procedure SetPixel(x,y: integer; value: byte);
    procedure XorPixel(x,y: integer; value: byte);
  end;

type
  ArrayOfString = array of string;

function StylesToArray(AStyles: string): ArrayOfString;

implementation
uses
 zgl_utils, zgl_primitives_2d, zgl_render_2d;

function StylesToArray(AStyles: string): ArrayOfString;
var
  StartIndex, EndIndex: integer;
  Count: integer;

  procedure AddStyle(AName: string);
  begin
    if (AName = 'Normal') or (AName = 'Regular') or (AName = 'Roman') or (AName = 'Plain') or (AName = 'Book') then exit;
    if Count = length(result) then
      setlength(result, length(result)+4);
    result[Count] := AName;
    inc(Count);
  end;

begin
  Count := 0;
  result := nil;
  StartIndex := 1;
  while StartIndex <= length(AStyles) do
  begin
    while (StartIndex < length(AStyles)) and (AStyles[StartIndex] = ' ') do inc(StartIndex);
    if AStyles[StartIndex] <> ' ' then
    begin
      EndIndex := StartIndex;
      while (EndIndex < length(AStyles)) and (AStyles[EndIndex+1] <> ' ') do inc(EndIndex);
      AddStyle(copy(AStyles, StartIndex, EndIndex-StartIndex+1));
      StartIndex := EndIndex+1;
    end;
  end;
  setlength(result,Count);
end;

var
  BitCountTable: packed array[0..255] of byte;
  RegularGray5: TT_Gray_Palette;
  FreeTypeInitialized,FreeTypeCannotInitialize : boolean;

procedure EnsureFreeTypeInitialized;
begin
  if not FreeTypeInitialized and not FreeTypeCannotInitialize then
  begin
    FreeTypeInitialized := (TT_Init_FreeType = TT_Err_Ok);
    FreeTypeCannotInitialize := not FreeTypeInitialized;
  end;
  if FreeTypeCannotInitialize then
    raise Exception.Create('FreeType cannot be initialized');
end;

{ TFreeTypeGlyph }

{$hints off}
function TFreeTypeGlyph.GetBounds: TRect;
var metrics: TT_Glyph_Metrics;
begin
  TT_Get_Glyph_Metrics(FGlyphData, metrics);
  with metrics.bbox do
    result := rect(IncludeFullGrainMin(xMin,64) div 64,IncludeFullGrainMin(-yMax,64) div 64,
       (IncludeFullGrainMax(xMax,64)+1) div 64,(IncludeFullGrainMax(-yMin,64)+1) div 64);
end;
{$hints on}

{$hints off}
function TFreeTypeGlyph.GetAdvance: single;
var metrics: TT_Glyph_Metrics;
begin
  TT_Get_Glyph_Metrics(FGlyphData, metrics);
  result := metrics.advance/64;
end;
{$hints on}

{$hints off}
function TFreeTypeGlyph.GetBoundsWithOffset(x, y: single): TRect;
var metrics: TT_Glyph_Metrics;
begin
  TT_Get_Glyph_Metrics(FGlyphData, metrics);
  with metrics.bbox do
    result := rect(IncludeFullGrainMin(xMin+round(x*64),64) div 64,IncludeFullGrainMin(-yMax+round(y*64),64) div 64,
       (IncludeFullGrainMax(xMax+round(x*64),64)+1) div 64,(IncludeFullGrainMax(-yMin+round(y*64),64)+1) div 64);
end;
{$hints on}

constructor TFreeTypeGlyph.Create(AFont: TFreeTypeFont; AIndex: integer);
begin
  if TT_New_Glyph(AFont.FFace, FGlyphData) <> TT_Err_Ok then
    raise Exception.Create('Cannot create empty glyph');
  FLoaded := AFont.LoadGlyphInto(FGlyphData, AIndex);
  FIndex := AIndex;
end;

function TFreeTypeGlyph.RenderDirectly(x, y: single; Rect: TRect;
  OnRender: TDirectRenderingFunction; quality : TGlyphRenderQuality; ClearType: boolean): boolean;
begin
  result := RenderDirectly(TTGetDefaultRasterizer, x,y, Rect, OnRender, quality, ClearType);
end;

function TFreeTypeGlyph.RenderDirectly(ARasterizer: TFreeTypeRasterizer; x,
  y: single; Rect: TRect; OnRender: TDirectRenderingFunction;
  quality: TGlyphRenderQuality; ClearType: boolean): boolean;
var mono: TFreeTypeMonochromeMap;
    tx,xb,yb: integer;
    pdest: pbyte;
    buf: pointer;
    glyphBounds: TRect;
begin
  if ClearType then
  begin
    Rect.Left := Rect.Left * 3;
    Rect.Right := Rect.Right * 3;
    x := x * 3;
  end;

  glyphBounds := BoundsWithOffset[x,y];


  if ClearType then
  begin
    InflateRect(glyphBounds,1,0);
    glyphBounds.Left := IncludeFullGrainMin( glyphBounds.Left, 3);
    glyphBounds.Right := IncludeFullGrainMax( glyphBounds.Right-1, 3) + 1;
  end;
  if not IntersectRect(Rect,Rect,glyphBounds) then exit;

  case quality of
    grqMonochrome: begin
                      tx := rect.right-rect.left;
                      mono := TFreeTypeMonochromeMap.Create(ARasterizer,tx,rect.bottom-rect.top);
                      result := mono.RenderGlyph(self,x-rect.left,y-rect.top);
                      if result then
                      begin
                        getmem(buf, tx);
                        for yb := mono.Height-1 downto 0 do
                        begin
                          mono.ScanMoveTo(0,yb);
                          pdest := pbyte(buf);
                          for xb := tx-1 downto 0 do
                          begin
                            if mono.ScanNextPixel then
                              pdest^ := $ff
                            else
                              pdest^ := 0;
                            inc(pdest);
                          end;
                          OnRender(rect.Left,rect.top+yb,tx,buf);
                        end;
                        freemem(buf);
                      end;
                      mono.Free;
                   end;
    grqLowQuality: begin
                     ARasterizer.Set_Raster_Palette(RegularGray5);
                     result := TT_Render_Directly_Glyph_Gray(FGlyphData, round((x-rect.left)*64), round((rect.bottom-y)*64), rect.left,rect.top,rect.right-rect.left,rect.bottom-rect.top, FItalic, OnRender, ARasterizer) = TT_Err_Ok;
                   end;
    grqHighQuality: result := TT_Render_Directly_Glyph_HQ(FGlyphData, round((x-rect.left)*64), round((rect.bottom-y)*64), rect.left,rect.top,rect.right-rect.left,rect.bottom-rect.top, FItalic, OnRender, ARasterizer) = TT_Err_Ok;
  else
    result := false;
  end;
end;

destructor TFreeTypeGlyph.Destroy;
begin
  TT_Done_Glyph(FGlyphData);
  inherited Destroy;
end;

{ TFreeTypeFont }

procedure TFreeTypeFont.UpdateFace(const AName: String);
var errorNum: TT_Error;
    PrevDPI: integer;
begin
  PrevDPI := DPI;
  DiscardFace;

  if AName = '' then exit;

  errorNum := TT_Open_Face(AName,FFace);
  if errorNum <> TT_Err_Ok then
    raise exception.Create('Cannot open font (TT_Error ' + intToStr(errorNum)+') "'+AName+'"');
  FFaceLoaded:= true;
  FName:=AName;
  UpdateInstance;
  DPI := PrevDPI;
end;

{$hints off}
function TFreeTypeFont.GetDPI: integer;
var metrics: TT_Instance_Metrics;
begin
  if not FInstanceCreated then
    result := 96
  else
  begin
    if TT_Get_Instance_Metrics(FInstance,metrics) = TT_Err_Ok then
      result := metrics.y_resolution
    else
      result := 96;
  end;
end;
{$hints on}

function TFreeTypeFont.GetFamily: string;
begin
  result := Information[ftiFamily];
end;

function TFreeTypeFont.GetAscent: single;
begin
  result := FAscentValue*SizeInPixels;
end;

function TFreeTypeFont.GetClearType: boolean;
begin
  Result:= FClearType;
end;

function TFreeTypeFont.GetCharIndex(AChar: integer): integer;
begin
  if FCharmapOk then
    result := TT_Char_Index(FCharMap, AChar)
  else
    result := AChar;
end;

function TFreeTypeFont.GetDescent: single;
begin
  result := FDescentValue*SizeInPixels;
end;

function TFreeTypeFont.GetGlyph(Index: integer): TFreeTypeGlyph;
var 
  i: integer;
begin
  if not FInstanceCreated then
  begin
    result := nil;
    exit;
  end;
  for i:=0 to high(FGlyphTable) do begin
    if FGlyphTable[i].charcode = Index then begin
      result:= FGlyphTable[i].Glyph;
      exit;
    end;
  end;
  i:= length(FGlyphTable);
  SetLength(FGlyphTable, i+1);
  FGlyphTable[i].Glyph := TFreeTypeGlyph.Create(self, Index);
  FGlyphTable[i].charcode:= Index;
  result := FGlyphTable[i].Glyph;
end;

{$hints off}
function TFreeTypeFont.GetGlyphCount: integer;
var prop : TT_Face_Properties;
begin
  if not FFaceLoaded then
    result := 0
  else
  begin
    if TT_Get_Face_Properties(FFace, prop) <> TT_Err_Ok then
      result := 0
    else
      result := prop.num_glyphs;
  end;
end;

function TFreeTypeFont.GetInformation(AIndex: TFreeTypeInformation): string;
begin
  if FNamesArray = nil then FetchNames;
  if (ord(AIndex) < 0) or (ord(AIndex) > high(FNamesArray)) then
    result := ''
  else
    result := FNamesArray[ord(AIndex)];
end;

{$hints on}

function TFreeTypeFont.GetLineFullHeight: single;
begin
  result := (FAscentValue + FDescentValue)*SizeInPixels + GetLineSpacing;
end;

function TFreeTypeFont.GetLineSpacing: single;
begin
  if not SmallLinePadding then
    result := FLargeLineGapValue*SizeInPixels
  else
    result := FLineGapValue*SizeInPixels;
end;

function TFreeTypeFont.GetPixelSize: single;
begin
  result := SizeInPoints * DPI / 72;
end;

function TFreeTypeFont.GetVersionNumber: string;
var VersionStr: string;
    idxStart,idxEnd: integer;
begin
  VersionStr := Information[ftiVersionString];
  idxStart := 1;
  while (idxStart < length(VersionStr)) and not (VersionStr[idxStart] in['0'..'9']) do
    inc(idxStart);
  idxEnd := idxStart;
  while (idxEnd+1 <= length(VersionStr)) and (VersionStr[idxEnd+1] in['0'..'9']) do inc(idxEnd);
  if (idxEnd+1 <= length(VersionStr)) and (VersionStr[idxEnd+1] = '.') then inc(idxEnd);
  while (idxEnd+1 <= length(VersionStr)) and (VersionStr[idxEnd+1] in['0'..'9']) do inc(idxEnd);
  result := copy(VersionStr,idxStart,idxEnd-idxStart+1);
end;

procedure TFreeTypeFont.SetClearType(const AValue: boolean);
begin
  if FClearType=AValue then exit;
  FClearType:=AValue;
  UpdateSizeInPoints;
end;

procedure TFreeTypeFont.SetDPI(const AValue: integer);
begin
  if FInstanceCreated then
  begin
    TT_Set_Instance_Resolutions(FInstance, AValue,AValue);
    UpdateSizeInPoints;
  end;
end;

procedure TFreeTypeFont.DiscardFace;
begin
  if FFaceLoaded then
  begin
    DiscardInstance;
    TT_Close_Face(FFace);
    FFaceLoaded := false;
    FNamesArray := nil;
  end;
  FCharmapOk := false;
end;

procedure TFreeTypeFont.DiscardInstance;
begin
  if FInstanceCreated then
  begin
    TT_Done_Instance(FInstance);
    FInstanceCreated := false;
    DestroyGlyphs;
  end;
end;

function TFreeTypeFont.LoadGlyphInto(_glyph: TT_Glyph; glyph_index: Word): boolean;
var flags: integer;
begin
  if not FInstanceCreated then
    raise Exception.Create('No font instance');
  flags := TT_Load_Scale_Glyph;
  if not FBold then flags := flags or TT_Load_Hint_Glyph;
  result := (TT_Load_Glyph(FInstance, _glyph, glyph_index, flags) <> TT_Err_Ok);
end;

procedure TFreeTypeFont.SetWidthFactor(const AValue: single);
begin
  if FWidthFactor=AValue then exit;
  FWidthFactor:=AValue;
  DestroyGlyphs;
  if FInstanceCreated then
    UpdateSizeInPoints;
end;

procedure TFreeTypeFont.UpdateInstance;
var
    errorNum: TT_Error;
begin
  DiscardInstance;

  errorNum := TT_New_Instance(FFace, FInstance);
  if errorNum = TT_Err_Ok then
  begin
    FInstanceCreated := true;
    UpdateMetrics;
    UpdateCharmap;
  end else
    raise exception.Create('Cannot create font instance (TT_Error ' + intToStr(errorNum)+')');
end;

procedure TFreeTypeFont.UpdateSizeInPoints;
var charsizex: integer;
begin
  if FInstanceCreated then
  begin
    if not FClearType then
      charsizex := round(FPointSize*64*FWidthFactor)
    else
      charsizex := round(FPointSize*64*FWidthFactor*3);

    if TT_Set_Instance_CharSizes(FInstance,charsizex,round(FPointSize*64)) <> TT_Err_Ok then
      raise Exception.Create('Unable to set point size');
  end;
end;

procedure TFreeTypeFont.UpdateMetrics;
var prop: TT_Face_Properties;
begin
  if FFaceLoaded then
  begin
    TT_Get_Face_Properties(FFace,prop);
    FAscentValue := prop.horizontal^.ascender;
    FDescentValue := prop.horizontal^.descender;
    FLineGapValue:= prop.horizontal^.line_gap;
    FLargeLineGapValue:= FLineGapValue;

    if (FAscentValue = 0) and (FDescentValue = 0) then
    begin
      if prop.os2^.version <> $ffff then
      begin
        if (prop.os2^.usWinAscent <> 0) or (prop.os2^.usWinDescent <> 0) then
        begin
          FAscentValue := prop.os2^.usWinAscent;
          FDescentValue := -prop.os2^.usWinDescent;
        end else
        begin
          FAscentValue := prop.os2^.sTypoAscender;
          FDescentValue := prop.os2^.sTypoDescender;
        end;
      end;
    end;

    if prop.os2^.version <> $ffff then
    begin
      if prop.os2^.sTypoLineGap > FLargeLineGapValue then
        FLargeLineGapValue := prop.os2^.sTypoLineGap;
    end;

    FAscentValue := FAscentValue / prop.header^.units_per_EM;
    FDescentValue := FDescentValue / -prop.header^.units_per_EM;
    FLineGapValue := FLineGapValue / prop.header^.units_per_EM;
    FLargeLineGapValue := FLargeLineGapValue / prop.header^.units_per_EM;

    if FLargeLineGapValue = 0 then
      FLargeLineGapValue := (FAscentValue+FDescentValue)*0.1;

  end else
  begin
    FAscentValue := -0.5;
    FDescentValue := 0.5;
    FLineGapValue := 0;
  end;
end;

procedure TFreeTypeFont.UpdateCharmap;
var i,n: integer;
    platform,encoding: integer;
begin
  if FCharmapOk then exit;
  if not FFaceLoaded then
  begin
    FCharmapOk := false;
    exit;
  end;

  n := TT_Get_CharMap_Count(FFace);
  platform := 0;
  encoding := 0;

  //MS Unicode
  for i := 0 to n-1 do
  begin
    if TT_Get_CharMap_ID(FFace, i, platform, encoding) = TT_Err_Ok then
    begin
      if (platform = 3) and (encoding = 1) then
        if TT_Get_CharMap(FFace, i, FCharMap) = TT_Err_Ok then
        begin
          FCharmapOk := true;
          exit;
        end;
    end;
  end;

  //Apple Unicode
  for i := 0 to n-1 do
  begin
    if TT_Get_CharMap_ID(FFace, i, platform, encoding) = TT_Err_Ok then
    begin
      if (platform = 0) then
        if TT_Get_CharMap(FFace, i, FCharMap) = TT_Err_Ok then
        begin
          FCharmapOk := true;
          exit;
        end;
    end;
  end;

  //ISO Unicode
  for i := 0 to n-1 do
  begin
    if TT_Get_CharMap_ID(FFace, i, platform, encoding) = TT_Err_Ok then
    begin
      if (platform = 2) and (encoding = 1) then
        if TT_Get_CharMap(FFace, i, FCharMap) = TT_Err_Ok then
        begin
          FCharmapOk := true;
          exit;
        end;
    end;
  end;

  FCharmapOk := false;
end;

constructor TFreeTypeFont.Create(const AName: String; ASize: integer; AHint: boolean = true);
begin
  EnsureFreeTypeInitialized;
  FFaceLoaded := false;
  FInstanceCreated := false;
  FCharmapOk := false;
  FGlyphTable := nil;
  fColor:= $00000000;
  fAlpha:= 255;
  FHint := AHint;
  FBold := false;
  FItalic := false;
  FWidthFactor := 1;
  FClearType := false;
  SmallLinePadding:= true;
  Quality := grqHighQuality;
  FPointSize :=ASize;
  UpdateFace(AName);
end;

destructor TFreeTypeFont.Destroy;
begin
  DiscardInstance;
  DiscardFace;
  DestroyGlyphs;
  inherited Destroy;
end;

procedure TFreeTypeFont.RenderText(AText: string; ARect: zglTRect);
var
 desc : single;
 rect: TRect;
begin
 if AText='' then exit; 
 rect.Left:= round(ARect.x);
 rect.Top:= round(ARect.y);
 rect.Right:= round(ARect.x + ARect.w);
 rect.Bottom:= round(ARect.y + ARect.h); 
 desc:= GetDescent;
 if not b2dStarted then batch2d_Begin;
 if FClearType then
  RenderText(AText, rect.Left, rect.Bottom - desc, rect, @RenderTextClearDirectly)
 else
  RenderText(AText, rect.Left, rect.Bottom - desc, rect, @RenderTextDirectly);
 if not b2dStarted then batch2d_End;
end;

procedure TFreeTypeFont.RenderText(AText: string; x, y: single; ARect: TRect;
  OnRender: TDirectRenderingFunction);
var
  pstr: pchar;
  left,charcode,charlen: integer;
  idx,j: integer;
  g: TFreeTypeGlyph;
  t:string;
begin
  if not FInstanceCreated then exit;
  if AText = '' then exit;
  idx := pos(LineEnding,AText);
  while idx <> 0 do
  begin
    RenderText(copy(AText,1,idx-1),x,y,ARect,OnRender);
    delete(AText,1,idx+length(LineEnding)-1);
    y := y + LineFullHeight;
    idx := pos(LineEnding,AText);
  end;
  pstr := @AText[1];
  left := length(AText);
  j:= 1;
  while j<=left do
  begin
    charcode :=utf8_GetID(pstr, 1, @charlen);
    charlen:= charlen-1;
    inc(pstr,charlen);
    inc(j,charlen);
    g := Glyph[CharIndex[charcode]];
    if g <> nil then begin
     g.Italic:= FItalic;
     with g do
     begin
       if not fHint then
        RenderDirectly(x,round(y),ARect,OnRender,quality,FClearType)
       else
        RenderDirectly(x,y,ARect,OnRender,quality,FClearType);
       if FClearType then
         x := x + Advance/3
       else
         x := x + Advance;
     end;
    end;
  end;
end;

function TFreeTypeFont.TextWidth(AText: string): single;
var
  pstr: pchar;
  left,charcode,charlen: integer;
  maxWidth,w: single;
  idx,j: integer;
  g: TFreeTypeGlyph;
begin
  result := 0;
  if not FInstanceCreated then exit;
  if AText = '' then exit;

  maxWidth := 0;
  idx := pos(LineEnding,AText);
  while idx <> 0 do
  begin
    w := TextWidth(copy(AText,1,idx-1));
    if w > maxWidth then maxWidth:= w;
    delete(AText,1,idx+length(LineEnding)-1);
    idx := pos(LineEnding,AText);
  end;
  if AText = '' then
  begin
    result := maxWidth;
    exit;
  end;

  pstr := @AText[1];
  left := length(AText);
  j:= 1;
  while j<=left do
  begin
    charcode :=utf8_GetID(pstr, 1, @charlen);
    charlen:= charlen-1;
    inc(pstr,charlen);
    inc(j,charlen);
    g := Glyph[CharIndex[charcode]];
    if g <> nil then
    with g do
    begin
      if FClearType then
        result := result + Advance/3
      else
        result := result + Advance;
    end;
  end;
  if maxWidth > result then
    result := maxWidth;
end;

function TFreeTypeFont.TextHeight(AText: string): single;
var idx: integer;
    nb: integer;
begin
  if AText= '' then result := 0
   else
  begin
    result := LineFullHeight;
    nb := 1;
    idx := pos(LineEnding,AText);
    while idx <> 0 do
    begin
      nb := nb + 1;
      delete(AText,1,idx+length(LineEnding)-1);
      idx := pos(LineEnding,AText);
    end;
    result := result * nb;
  end;
end;

function TFreeTypeFont.CharsWidth(AText: string): ArrayOfSingle;
var
  pstr: pchar;
  left,charcode,charlen: integer;
  resultIndex,i,j: integer;
  w: single;
begin
  if AText = '' then exit;
  pstr := @AText[1];
  left := length(AText);
  setlength(result, utf8_Length(AText));
  resultIndex := 0;
  j:= 1;
  while j<=left do
  begin
    charcode :=utf8_GetID(pstr, 1, @charlen);
    charlen:= charlen-1;
    inc(pstr,charlen);
    inc(j,charlen);

    with Glyph[CharIndex[charcode]] do
    begin
      if FClearType then
        w := Advance/3
      else
        w := Advance;
    end;

    for i := 1 to charlen do
    begin
      result[resultIndex] := w;
      inc(resultIndex);
    end;
  end;
end;

procedure TFreeTypeFont.DestroyGlyphs;
var
 i: integer;
begin
 for i:=high(FGlyphTable) downto 0 do begin
  FGlyphTable[i].Glyph.Destroy;
 end;
 FGlyphTable:= nil;
end;

procedure TFreeTypeFont.RenderTextDirectly(x, y, tx: integer; data: pointer);
var
 psrc: pbyte;
 tempValue: byte;
 aalpha: byte;
begin
 psrc := pbyte(data);
 while tx > 0 do begin
  tempValue := psrc^;
  if tempValue <> 0 then begin
   aalpha:= fAlpha * tempValue div 255;
   if fBold then begin
    pr2d_Pixel(x-1, y, fColor, aalpha);
    pr2d_Pixel(x, y, fColor, aalpha);
   end else begin
    pr2d_Pixel(x, y, fColor, aalpha);
   end;
  end;
  inc(psrc);
  inc(x);
  dec(tx);
 end;
end;

procedure TFreeTypeFont.RenderTextClearDirectly(x, y, tx: integer; data: pointer);
var xb: integer;
    psrc: pbyte;
    Cr,Cg,Cb: byte;
    acolor: array[0..3] of byte;
begin
 //ClearType position in third of pixels horizontally (multiple of 3)
 x := x div 3;
 tx := tx div 3;
 //ensure rendering in bounds
 if tx=0 then exit;

 psrc := pbyte(data);
 Cr := (psrc^ + psrc^ + (psrc+1)^) div 3;
 Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
 if tx > 1 then
   Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3
 else
   Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
 if Cr+Cg+Cb <> 0 then begin
  acolor[0]:= Cr;
  acolor[1]:= Cg;
  acolor[2]:= Cb;
  acolor[3]:= fAlpha;
  pr2d_Pixel(x, y, LongWord(acolor), fAlpha);
 end;
 inc(x);
 inc(psrc,3);
 for xb := 1 to tx-2 do begin
  Cr := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
  Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
  Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3;
  if Cr+Cg+Cb <> 0 then begin
   acolor[0]:= Cr;
   acolor[1]:= Cg;
   acolor[2]:= Cb;
   acolor[3]:= fAlpha;
   pr2d_Pixel(x, y, LongWord(acolor), fAlpha);
  end;
  inc(x);
  inc(psrc,3);
 end;
 if tx > 1 then begin
  Cr := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
  Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
  Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
  if Cr+Cg+Cb <> 0 then begin
   acolor[0]:= Cr;
   acolor[1]:= Cg;
   acolor[2]:= Cb;
   acolor[3]:= fAlpha;
   pr2d_Pixel(x, y, LongWord(acolor), fAlpha);
  end;
 end;
end;

procedure TFreeTypeFont.FetchNames;
const
  maxNameIndex = 22;
var i,j: integer;
  nrPlatformID,nrEncodingID,nrLanguageID,nrNameID,len: integer;
  value,value2: string;

begin
  setlength(FNamesArray, maxNameIndex+1);
  if FFaceLoaded then
  begin
    for i := 0 to TT_Get_Name_Count(FFace)-1 do
    begin
      if TT_Get_Name_ID(FFace, i, nrPlatformID, nrEncodingID,
                        nrLanguageID, nrNameID) <> TT_Err_Ok then continue;

      if (nrNameID < 0) or (nrNameID > maxNameIndex) then continue;

        { check for Microsoft, Unicode, English }
      if ((nrPlatformID=3) and (nrEncodingID=1) and
         ((nrLanguageID=$0409) or (nrLanguageID=$0809) or
          (nrLanguageID=$0c09) or (nrLanguageID=$1009) or
          (nrLanguageID=$1409) or (nrLanguageID=$1809))) or
        { or for Unicode, English }
        ((nrPlatformID=0) and
         (nrLanguageID=0)) then
      begin
        value := TT_Get_Name_String(FFace, i);
        for j := 1 to length(value) div 2 do
          pword(@value[j*2-1])^ := BEtoN(pword(@value[j*2-1])^);
        setlength(value2, 3*(length(value) div 2) + 1); //maximum is 3-byte chars and NULL char at the end
        len := System.UnicodeToUtf8(@value2[1],length(value2),PUnicodeChar( @value[1] ),length(value) div 2);
        if len > 0 then
        begin
          setlength(value2, len-1 );
          value := value2;
        end;
        FNamesArray[nrNameID] := value;
      end;
    end;
  end;
end;

{ TFreeTypeGrayscaleMap }

procedure TFreeTypeGrayscaleMap.Init(AWidth, AHeight: integer);
begin
  map.Width := AWidth;
  map.Rows := AHeight;
  map.Cols:= (AWidth+3) and not 3;
  map.flow:= TT_Flow_Down;
  map.Size:= map.Rows*map.Cols;
  getmem(map.Buffer,map.Size);
  Clear;
  RenderQuality := grqHighQuality;
end;

function TFreeTypeGrayscaleMap.RenderGlyph(glyph: TFreeTypeGlyph; x, y: single): boolean;
var mono: TFreeTypeMonochromeMap;
    psrc,pdest: pbyte;
    xb,yb,tx: integer;
    curBit: byte;
begin
  case RenderQuality of
    grqMonochrome:
      begin
        tx := Width;
        mono := TFreeTypeMonochromeMap.Create(FRasterizer, tx,Height);
        result := mono.RenderGlyph(glyph,x,y);
        if result then
        begin
          for yb := mono.Height-1 downto 0 do
          begin
            psrc := mono.ScanLine[yb];
            pdest := self.ScanLine[yb];
            curBit := $80;
            for xb := tx-1 downto 0 do
            begin
              if psrc^ and curBit <> 0 then
                pdest^ := $ff;
              curBit := curBit shr 1;
              if curBit = 0 then
              begin
                curBit := $80;
                inc(psrc);
              end;
              inc(pdest);
            end;
          end;
        end;
        mono.Free;
      end;
    grqLowQuality:
      begin
        FRasterizer.Set_Raster_Palette(RegularGray5);
        result := TT_Get_Glyph_Pixmap(glyph.data, map, round(x*64), round((height-y)*64), false, FRasterizer) = TT_Err_Ok;
      end;
    grqHighQuality:
      begin
        result := TT_Get_Glyph_Pixmap_HQ(glyph.data, map, round(x*64), round((height-y)*64), false, FRasterizer) = TT_Err_Ok;
      end;
  end;
end;

procedure TFreeTypeGrayscaleMap.ScanMoveTo(x, y: integer);
begin
  ScanPtrStart := pbyte(ScanLine[y]);
  ScanX := x mod Width;
  if ScanX < 0 then inc(ScanX,Width);
end;

function TFreeTypeGrayscaleMap.ScanNextPixel: byte;
begin
  if ScanPtrStart = nil then
    result := 0
  else
  begin
    result := (ScanPtrStart+ScanX)^;
    inc(ScanX);
    if ScanX = map.Width then ScanX := 0;
  end;
end;

function TFreeTypeGrayscaleMap.GetPixel(x, y: integer): byte;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    result := 0
  else
    result := (pbyte(map.Buffer) + y*map.Cols + x)^;
end;

procedure TFreeTypeGrayscaleMap.SetPixel(x, y: integer; value: byte);
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    exit
  else
    (pbyte(map.Buffer) + y*map.Cols + x)^ := value;
end;

procedure TFreeTypeGrayscaleMap.XorPixel(x, y: integer; value: byte);
var p : pbyte;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    exit
  else
  begin
    p := (pbyte(map.Buffer) + y*map.Cols + x);
    p^ := p^ xor value;
  end;
end;

{ TFreeTypeRasterMap }

function TFreeTypeRasterMap.GetHeight: integer;
begin
  result := map.Rows;
end;

function TFreeTypeRasterMap.GetWidth: integer;
begin
  result := map.Width;
end;

function TFreeTypeRasterMap.GetScanLine(y: integer): pointer;
begin
  if (y <0) or (y >= height) then
    result := nil
  else
    Result:= pointer(pbyte(map.Buffer) + y*map.Cols);
end;

constructor TFreeTypeRasterMap.Create(AWidth, AHeight: integer);
begin
  FRasterizer := TTGetDefaultRasterizer;
  Init(AWidth,AHeight);
end;

constructor TFreeTypeRasterMap.Create(ARasterizer: TFreeTypeRasterizer; AWidth,
  AHeight: integer);
begin
  FRasterizer := ARasterizer;
  Init(AWidth,AHeight);
end;

procedure TFreeTypeRasterMap.Clear;
begin
  fillchar(map.Buffer^, map.Size, 0);
end;

procedure TFreeTypeRasterMap.Fill;
begin
  fillchar(map.Buffer^, map.Size, $ff);
end;

destructor TFreeTypeRasterMap.Destroy;
begin
  freemem(map.Buffer);
  inherited Destroy;
end;

{ TFreeTypeMonochromeMap }

function TFreeTypeMonochromeMap.RenderGlyph(glyph: TFreeTypeGlyph; x,y: single): boolean;
begin
  result := TT_Get_Glyph_Bitmap(glyph.data, map, round(x*64), round((height-y)*64), false, FRasterizer) = TT_Err_Ok;
end;

procedure TFreeTypeMonochromeMap.ScanMoveTo(x, y: integer);
begin
  ScanPtrStart := pbyte(ScanLine[y]);
  ScanX := x mod Width;
  if ScanX < 0 then inc(ScanX,Width);

  if ScanPtrStart <> nil then
  begin
    ScanPtrCur := ScanPtrStart + (ScanX shr 3);
    ScanBit := $80 shr (ScanX and 7);
  end else
  begin
    ScanPtrCur := nil;
    ScanBit := 0;
  end;
end;

function TFreeTypeMonochromeMap.ScanNextPixel: boolean;
begin
  if ScanPtrCur = nil then
    result := false
  else
  begin
    result := (pbyte(ScanPtrCur)^ and ScanBit) <> 0;
    inc(ScanX);
    if ScanX = map.Width then
    begin
      ScanX := 0;
      ScanBit := $80;
      ScanPtrCur := ScanPtrStart;
    end else
    begin
      ScanBit := ScanBit shr 1;
      if ScanBit = 0 then
      begin
        ScanBit := $80;
        inc(ScanPtrCur);
      end;
    end;
  end;
end;

function TFreeTypeMonochromeMap.GetPixel(x, y: integer): boolean;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    result := false
  else
    result := (pbyte(map.Buffer) + y*map.Cols + (x shr 3))^ and ($80 shr (x and 7)) <> 0;
end;

procedure TFreeTypeMonochromeMap.SetPixel(x, y: integer; value: boolean);
var p: pbyte;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    exit
  else
  begin
    p := pbyte(map.Buffer) + y*map.Cols + (x shr 3);
    if not value then
      p^ := p^ and not ($80 shr (x and 7))
    else
      p^ := p^ or ($80 shr (x and 7));
  end;
end;

function TFreeTypeMonochromeMap.GetPixelsInRect(x, y, x2, y2: integer): integer;
var yb: integer;
begin
  result := 0;

  if x < 0 then x := 0;
  if x2 > width then x2 := width;
  if x2 <= x then exit;

  if y < 0 then y := 0;
  if y2 > height then y2 := height;
  for yb := y to y2-1 do
    result := result + GetPixelsInHorizlineNoBoundsChecking(x,yb,x2-1);
end;

function TFreeTypeMonochromeMap.GetPixelsInHorizline(x, y, x2: integer): integer;
begin
  if x < 0 then x := 0;
  if x2 >= width then x2 := width-1;
  if x2 <= x then
  begin
    result := 0;
    exit;
  end;
  if (y < 0) or (y >= height) then
  begin
    result := 0;
    exit;
  end;

  result := GetPixelsInHorizlineNoBoundsChecking(x,y,x2);
end;

function TFreeTypeMonochromeMap.GetPixelsInHorizlineNoBoundsChecking(x, y, x2: integer
  ): integer;
var p: pbyte;
    ix,ix2: integer;
begin
  result := 0;
  ix := x shr 3;
  ix2 := x2 shr 3;
  p := pbyte(map.Buffer) + y*map.Cols + ix;
  if ix2 > ix then
  begin
    result := result + BitCountTable[ p^ and ($ff shr (x and 7)) ];
    inc(p^);
    inc(ix);
    while (ix2 > ix) do
    begin
      result := result + BitCountTable[p^];
      inc(ix);
      inc(p^);
    end;
    result := result + BitCountTable[ p^ and ($ff shl (x2 and 7 xor 7)) ];
  end else
    result := result + BitCountTable[ p^ and ($ff shr (x and 7)) and ($ff shl (x2 and 7 xor 7))];
end;

procedure TFreeTypeMonochromeMap.Init(AWidth, AHeight: integer);
begin
  map.Width := AWidth;
  map.Rows := AHeight;
  map.Cols:= (AWidth+7) shr 3;
  map.flow:= TT_Flow_Down;
  map.Size:= map.Rows*map.Cols;
  getmem(map.Buffer,map.Size);
  Clear;
end;

procedure TFreeTypeMonochromeMap.TogglePixel(x, y: integer);
var p: pbyte;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    exit
  else
  begin
    p := pbyte(map.Buffer) + y*map.Cols + (x shr 3);
    p^ := p^ xor ($80 shr (x and 7));
  end;
end;

procedure InitTables;
var i: integer;
begin
  for i := 0 to 255 do
  begin
    BitCountTable[i] := (i and 1) + (i shr 1 and 1) + (i shr 2 and 1) + (i shr 3 and 1) +
       (i shr 4 and 1) + (i shr 5 and 1) + (i shr 6 and 1) + (i shr 7 and 1);
  end;

  RegularGray5[0] := 0;
  RegularGray5[1] := $60;
  RegularGray5[2] := $a0;
  RegularGray5[3] := $d0;
  RegularGray5[4] := $ff;
end;

initialization

  FreeTypeInitialized := false;
  FreeTypeCannotInitialize := false;
  InitTables;

finalization

  if FreeTypeInitialized then
  begin
    TT_Done_FreeType;
    FreeTypeInitialized := false;
  end;

end.

