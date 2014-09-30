//HGE ������ʾ�·���
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//Author  - ΢���ƽ��(BOGY)
//Mail    - bogy.cn@gmail.com
//Home    - http://bogy.cn
//Porting to ZenGl by һ·����
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unit GfxFont;

interface
uses
  Windows,zgl_main,zgl_textures,zgl_sprite_2d,zgl_types,zgl_fx,Vcl.Graphics;

const
  Font_Count = High(Word);
type
  tagEngineFontGlyph = record
    t: ZglPTexture;
    w: Single;
    h: Single;
    x: Single;
    y: Single;
    c: Single;
  end;
  TENGINEFONTGLYPH = tagEngineFontGlyph;

  TGfxFont = class
  private
     procedure Print(X: Single; Y: Single; Text: PWideChar);
     function GetTextSize(Text: PWideChar): TSize;
  public
    constructor Create(const FontName: PWideChar; FaceSize: Integer; bBold: Boolean = False;
      bItalic: Boolean = False; bAntialias: Boolean = True);
    destructor Destroy(); override;
  public
    // ��Ⱦ�ı�
    procedure TextOut(x,y:Single;Text:string);
    // �������ȡ��ɫ
    procedure SetColor(Color: Cardinal);overload;
    Procedure SetColor(Color:TColor);overload;
    function GetColor: Cardinal;
    // ��ȡ�ı�����
    Function TextWidth(Text:String):Integer;
    Function TextHeight(Text:String):Integer;
    // ���������ȡ�ַ�
    function GetCharacterFromPos(Text: PWideChar; Pixel_X, Pixel_Y: Single): WideChar;
    // �����ּ��
    procedure SetKerningWidth(Kerning: Single);
    procedure SetKerningHeight(Kerning: Single);
    // ��ȡ�ּ��
    function GetKerningWidth(): Single;
    function GetKerningHeight(): Single;
    // �����С
    function GetFontSize(): Single;
  private
    m_Glyphs: array [0..Font_Count-1] of TENGINEFONTGLYPH;
    m_nAntialias: Cardinal;//�����
    m_nAscent: Integer;//����
    //m_dwFontColor: Cardinal;
    m_nFontSize: Single;
    m_nKerningWidth: Single;
    m_nKerningHeight: Single;
    //m_pHGE: IHGE;   //Zengl����Ҫ
    //m_pSprite: IHGESprite;
    m_pSprite:ZglPTexture;
    // GDI�豸
    m_hMemDC: HDC;
    m_hFont: HFONT;
    function GetGlyphByCharacter(C: WideChar): Cardinal;
    function GetWidthFromCharacter(C: WideChar; Original: Boolean = False): Single;
    procedure CacheCharacter(Idx: Cardinal; C: WideChar);
  end;
function RGBA(const R, G, B, A: Byte): Longword; inline;
implementation

{ TGfxFont }

const
  g_byAlphaLever: array [0..65 - 1] of Byte =
  (
    0,  4,  8,  12, 16, 20, 24, 28, 32, 36, 40, 44, 48,
      52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96, 100,
    104,108,112,116,120,124,128,132,136,140,144,148,152,
    156,160,164,168,172,176,180,184,188,192,196,200,204,
    208,212,216,220,224,228,232,236,240,244,248,252,255
  );

const
  WideNull  = WideChar(#0);
  WideCR    = WideChar(#13);
  WideLF    = WideChar(#10);
  WideCRLF  : WideString = #13#10;
  
type
  TByteAry = array [0..0] of Byte;
  PByteAry = ^TByteAry;
procedure TGfxFont.CacheCharacter(Idx: Cardinal; C: WideChar);
var
  nChar: Cardinal;
  mat: MAT2;
  gm: GLYPHMETRICS;
  nLen: Cardinal;
   // hTex: ITexture; //����
  hTex:zglPTexture;
  lpBuf: PByteAry;
  lpSrc: PByteAry;
  lpDst: PLongWord;
  lpTexData:PByteArray;
  nSrcPitch, nDstPitch: Cardinal;
  x, y, k, i,W,H: Cardinal;
begin
  if (Idx < Font_Count) and (m_Glyphs[Idx].t = nil) then
  begin
    nChar := Cardinal(C);
    mat.eM11.fract := 0; mat.eM11.value := 1;
    mat.eM12.fract := 0; mat.eM12.value := 0;
    mat.eM21.fract := 0; mat.eM21.value := 0;
    mat.eM22.fract := 0; mat.eM22.value := 1;
    nLen := GetGlyphOutlineW(m_hMemDC, nChar, m_nAntialias, gm, 0, nil, mat);
    hTex := tex_CreateZero( gm.gmBlackBoxX, gm.gmBlackBoxY,$FFFFFFFF);
    //hTex := m_pHGE.Texture_Create(gm.gmBlackBoxX, gm.gmBlackBoxY); //����һ�������ô������
    if hTex = nil then Exit;

    if nLen > 0 then
    begin
      GetMem(lpBuf, nLen);
      if nLen = GetGlyphOutlineW(m_hMemDC, nChar, m_nAntialias, gm, nLen, lpBuf,
        mat) then

      begin
        lpSrc := lpBuf;
        //lpDst := m_pHGE.Texture_Lock(hTex, False);
        //������ʵ����ҪGetData��Ϊ�ڲ��ǿյİ�
        {�����Ϊ�������Ч����������ġ�û��Ҫ��Ϊ��ȡһ�������������Դ��ڴ��¿���һ������}
        GetMem( lpDst, Round( htex.Width / htex.U ) * Round( htex.Height / htex.V ) * 4 );
        lpTexData:=Pointer(lpDst);

        if GGO_BITMAP = m_nAntialias then
        begin
         //��ΪBITMAP�����ģʽ
          if gm.gmBlackBoxX mod 32 = 0 then
            nSrcPitch := (gm.gmBlackBoxX div 32)*4
          else
            nSrcPitch := ((gm.gmBlackBoxX div 32) + 1)*4;
          //nDstPitch := m_pHGE.Texture_GetWidth(hTex);//���ش�������������
          nDstPitch := hTex.Width;
          H:=gm.gmBlackBoxY;
          W:=gm.gmBlackBoxX;
          for y := 0 to gm.gmBlackBoxY - 1 do
          begin
            x := 0;
            while x < gm.gmBlackBoxX do
            begin
              for k := 0 to 7 do
              begin
                i := 8*x + k;
                if i >= gm.gmBlackBoxX then
                begin
                  Inc(x, 7);
                  Break;
                end;
                //����ͬ����Ҫ������������
                if (lpSrc[x] shr (7 - k)) and 1 = 0 then
                  PCardinal(Cardinal(lpDst) + (W-i-1) * SizeOf(Integer))^ := 0
                else
                  PCardinal(Cardinal(lpDst) + (W-i-1) * SizeOf(Integer))^ := $FFFFFFFF;
              end;
              Inc(x);
            end;
            Inc(lpSrc, nSrcPitch);
            Inc(lpDst, nDstPitch);
          end;  
        end
        else
        begin
          if gm.gmBlackBoxX mod 4 = 0 then
            nSrcPitch := (gm.gmBlackBoxX div 4)*4
          else
            nSrcPitch := (gm.gmBlackBoxX div 4 + 1)*4;

          //nDstPitch := m_pHGE.Texture_GetWidth(hTex);
          nDstPitch := hTex.Width;
          //����DX��OpenGL������˳��ͬ,���Դ���벻�ӸĶ� ���Ƴ�������
          //�����Ҿ����� ������ת��180�� ,��ת������ �ҿ�����ת�Ż��ơ�
          //���Ǿ���ֻ�ܽ�������λ�����������
          H:=gm.gmBlackBoxY;
          W:=gm.gmBlackBoxX;
          for y := 0 to gm.gmBlackBoxY - 1 do
          begin
            for x := 0 to gm.gmBlackBoxX - 1 do

            PCardinal(Cardinal(lpDst) + (W-X-1) * SizeOf(Integer))^ :=RGBA($FF,$FF,$FF,g_byAlphaLever[lpSrc[x]]);
                //ARGB(g_byAlphaLever[lpSrc[x]], $FF, $FF, $FF);
            Inc(lpSrc, nSrcPitch);
            Inc(lpDst, nDstPitch);
          end;
        end;
        tex_SetData(hTex,lpTexData,0,0,gm.gmBlackBoxX,gm.gmBlackBoxY);
        FreeMem(lpTexData);
        //m_pHGE.Texture_Unlock(hTex);  //��������
      end;
      FreeMem(lpBuf);
    end
    else
    begin
      // ��������ʾ�ַ�
    end;
    m_Glyphs[Idx].t := hTex;
    m_Glyphs[Idx].w := gm.gmBlackBoxX;
    m_Glyphs[Idx].h := gm.gmBlackBoxY;
    m_Glyphs[Idx].x := -gm.gmptGlyphOrigin.X;
    m_Glyphs[Idx].y := gm.gmptGlyphOrigin.Y - m_nAscent;
    m_Glyphs[Idx].c := gm.gmCellIncX;
  end;
end;

constructor TGfxFont.Create(const FontName: PWideChar; FaceSize: Integer;
  bBold, bItalic, bAntialias: Boolean);
var
  h_DC: HDC;
  Bold: Integer;
  tm: TEXTMETRICW;
begin
 // m_pHGE := HGECreate(HGE_VERSION);
  // ����GDI����豸
 // h_DC := GetDC(m_pHGE.System_GetState(HGE_HWND));//�ڴ��ھ���ϴ���DC
  h_DC := GetDC(zgl_Get(WINDOW_HANDLE));
  m_hMemDC := CreateCompatibleDC(h_DC);
  if m_hMemDC = 0 then Exit;

  ReleaseDC(zgl_Get(WINDOW_HANDLE), h_DC); //�ͷŴ��ھ��
  SetMapMode(m_hMemDC, MM_TEXT);
  SetTextColor(m_hMemDC, RGB($FF, $FF, $FF)); //��ɫ
  SetBkColor(m_hMemDC, RGB(0, 0, 0));

  if bBold then
    Bold := FW_BOLD
  else
    Bold := FW_NORMAL;
  m_hFont := CreateFontW(
    -FaceSize,
    0,
    0,
    0,
    Bold,
    Integer(bItalic),
    Cardinal(False),
    Cardinal(False),
    DEFAULT_CHARSET,
    OUT_DEFAULT_PRECIS,
    CLIP_DEFAULT_PRECIS,
    DEFAULT_QUALITY,
    FF_DONTCARE or DEFAULT_PITCH,
    FontName);
  if m_hFont = 0 then Exit;

  SelectObject(m_hMemDC, m_hFont);
  FillChar(m_Glyphs, SizeOf(TENGINEFONTGLYPH)*Font_Count, 0);

  if bAntialias then
    m_nAntialias := GGO_GRAY8_BITMAP
  else
    m_nAntialias := GGO_BITMAP;
  GetTextMetricsW(m_hMemDC, tm);
  m_nAscent := tm.tmAscent;
  m_nFontSize := FaceSize;
  m_nKerningWidth := 0;
  m_nKerningHeight := 0;
  SetColor($FFFFFFFF);//����Ĭ����ɫΪ��ɫ
  //m_pSprite := THGESprite.Create(nil, 0, 0, 0, 0); //����һ������ ������������
 // m_pSprite.SetColor(ARGB($FF, $FF, $FF, $FF));   // ��ɫ͸��
  //m_pSprite:=tex_CreateZero(0,0,$FFFFFF);
  //������Ҫ�ǽ���һ�������� Ȼ����붥�����ݡ�����zgl�������ɫ�ǲ���Ҫ��
end;

destructor TGfxFont.Destroy;
var
  nIdx: Integer;
begin
  for nIdx := 0 to Font_Count - 1 do
    if m_Glyphs[nIdx].t <> nil then
      m_Glyphs[nIdx].t := nil;

  if m_hFont <> 0 then
    DeleteObject(m_hFont);
  if m_hMemDC <> 0 then
    DeleteDC(m_hMemDC);
 // if m_pSprite <> nil then m_pSprite := nil;
  //if m_pSprite <> nil then tex_Del(m_pSprite);
  if m_pSprite <> nil then m_pSprite := nil;
  inherited;
end;

function TGfxFont.GetCharacterFromPos(Text: PWideChar; Pixel_X,
  Pixel_Y: Single): WideChar;
var
  X, Y, W: Single;
begin
  X := 0; Y := 0;
  while Text^ <> WideNull do
  begin
    if (Text^ = WideCR) and (PWideChar(Integer(Text)+SizeOf(WideChar))^ = WideLF) then
    begin
      X := 0;
      Y := m_nFontSize + m_nKerningWidth;
      Inc(Text);
      if Text^ = WideNull then
        Break;
    end;  
    W := GetWidthFromCharacter(Text^);
    if (Pixel_X > X) and (Pixel_X <= X + W) and
      (Pixel_Y > Y) and (Pixel_Y <= Y + m_nFontSize) then
    begin
      Result := Text^;
      Exit;
    end;
    X := X + W + m_nKerningWidth;
    Inc(Text);
  end;
  Result := WideNull;
end;

function TGfxFont.GetColor: Cardinal;
var
R,G,B,A:Byte;
begin
  //Result := m_pSprite.GetColor(i);//������ɫ������ɫ
  R:=fx2dColor[0];
  G:=fx2dColor[1];
  B:=fx2dColor[2];
  A:=fx2dColor[3];
  Result:=RGBA(R,G,B,A);
end;

function TGfxFont.GetFontSize: Single;
begin
  Result := m_nFontSize;
end;

function TGfxFont.GetGlyphByCharacter(C: WideChar): Cardinal;
var
  Idx: Cardinal;
begin
  Idx := Cardinal(C);
  if m_Glyphs[Idx].t = nil then
    CacheCharacter(Idx, C);
  Result := Idx;
end;

function TGfxFont.GetKerningHeight: Single;
begin
  Result := m_nKerningHeight;
end;

function TGfxFont.GetKerningWidth: Single;
begin
  Result := m_nKerningWidth;
end;

function TGfxFont.GetTextSize(Text: PWideChar): TSize;
var
  Dim: TSize;
  nRowWidth: Single;
begin
  nRowWidth := 0;
  Dim.cx := 0;
  Dim.cy := Round(m_nFontSize);
  while Text^ <> WideNull do
  begin
    if (Text^ = WideCR) and (PWideChar(Integer(Text)+SizeOf(WideChar))^ = WideLF) then
    begin
      Dim.cy := Round(m_nFontSize + m_nKerningHeight);
      if Dim.cx < nRowWidth then
        Dim.cx := Round(nRowWidth);
      nRowWidth := 0;
      Inc(Text, 2);
    end
    else
    begin
      nRowWidth := nRowWidth + GetWidthFromCharacter(Text^) + m_nKerningWidth;
      Inc(Text);
    end;
  end;
  if Dim.cx < Round(nRowWidth) then
    Dim.cx := Round(nRowWidth);
  Result := Dim;
end;

function TGfxFont.GetWidthFromCharacter(C: WideChar;
  Original: Boolean): Single;
var
  Idx: Cardinal;
begin
  Idx := GetGlyphByCharacter(C);
  if Original and (Idx > 0) and (Idx < Font_Count) then
  begin
    Result := m_Glyphs[Idx].c;
    Exit;
  end;
  if Idx >= $2000 then
    Result := m_nFontSize
  else
    Result := m_nFontSize/2;
end;

procedure TGfxFont.TextOut(x, y: Single; Text: string);
var
  lx,ly:Single;
begin
  if Text='' then exit;
  lx:=x;
  ly:=y;
  Print(lx,ly,PWideChar(Text));
end;
procedure TGfxFont.Print(X, Y: Single; Text: PWideChar);
var
  OffsetX, OffsetY: Single;
  Idx: Cardinal;
begin
  OffsetX := X; OffsetY := Y;
  while Text^ <> WideNull do
  begin
    if (Text^ = WideCR) and (PWideChar(Integer(Text)+SizeOf(WideChar))^ = WideLF) then
    begin
      OffsetX := X;
      OffsetY := OffsetY + m_nFontSize + m_nKerningHeight;
      Inc(Text, 2);
    end
    else
    begin
      Idx := GetGlyphByCharacter(Text^);
      if Idx > 0 then
      begin
        //��������
        //m_pSprite.SetTexture(m_Glyphs[Idx].t);
        m_pSprite:=m_Glyphs[Idx].t;
        //����������Χ(Ӧ���ǰ����������õ�����������),��Ϊ��ֱ�Ӹ�ֵ���������ǲ���Ҫ��
        //m_pSprite.SetTextureRect(0, 0, m_Glyphs[Idx].w, m_Glyphs[Idx].h);
        //���Ƴ���,
        //m_pSprite.Render(OffsetX - m_Glyphs[Idx].x, OffsetY - m_Glyphs[Idx].y);
        ssprite2d_Draw(m_pSprite,
           OffsetX - m_Glyphs[Idx].x,OffsetY - m_Glyphs[Idx].y,
           m_Glyphs[Idx].w, m_Glyphs[Idx].h,180,fx2dColor[3],FX_BLEND or FX_COLOR);
        OffsetX := OffsetX + GetWidthFromCharacter(Text^) + m_nKerningWidth;
      end
      else
        OffsetX := OffsetX + GetWidthFromCharacter(Text^) + m_nKerningWidth;

      Inc(Text);
    end;
  end;
end;

procedure TGfxFont.SetColor(Color: Cardinal);
var
R,G,B,A:Byte;
begin
  //�ֽ���ɫ��RGBA������
  R:=Color shr 24;
  G:=Color shr 16;
  B:=Color shr 8;
  A:=Color and $FF;
  //����ɫ���ϸ�ֵ
  fx2dColor[0]:=R;
  fx2dColor[1]:=G;
  fx2dColor[2]:=B;
  fx2dColor[3]:=A;
  //m_pSprite.SetColor(Color, i);  //���ö�����ɫ�����IΪ-1��ô�������ĸ�������ɫΪcolor
end;

procedure TGfxFont.SetColor(Color: TColor);
var
R,G,B,A:Byte;
begin
  //TColor��ɫ˳����BGR
  B:=Color shr 16;
  G:=Color shr 8;
  R:=Color and $FF;
  A:=$FF;
  fx2dColor[0]:=R;
  fx2dColor[1]:=G;
  fx2dColor[2]:=B;
  fx2dColor[3]:=A;
end;

procedure TGfxFont.SetKerningHeight(Kerning: Single);
begin
  m_nKerningHeight := Kerning;
end;

procedure TGfxFont.SetKerningWidth(Kerning: Single);
begin
  m_nKerningWidth := Kerning;
end;

function TGfxFont.TextHeight(Text: String): Integer;
begin
  Result:=GetTextSize(PWideChar(Text)).cy;
end;

function TGfxFont.TextWidth(Text: String): Integer;
begin
  Result:=GetTextSize(PWideChar(Text)).cx;
end;

function RGBA(const R, G, B, A: Byte): Longword; inline;
begin
  Result := (A shl 24) or (R shl 16) or (G shl 8) or B;
end;
end.