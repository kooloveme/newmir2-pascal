unit Mir2Map;

interface
uses
Map;
Const
UNITX = 48;
UNITY = 32;
type
   TMapInfo = packed record
      wBkImg: Word;  //tiles 的值 最高位为1表示不可行走
      wMidImg: Word; //smtiles的值 最高位为一表示不可行走
      wFrImg: Word;
      btDoorIndex: byte;
      btDoorOffset: byte;
      btAniFrame: byte;
      btAniTick: byte;
      btArea: byte;
      btLight: byte;  //12
    end;
    pTMapInfo = ^TMapInfo;

   TNewMapInfo = record
    wBkImg: Word;
    wMidImg: Word;
    wFrImg: Word;
    btDoorIndex: byte;
    btDoorOffset: byte;
    btAniFrame: byte;
    btAniTick: byte;
    btArea: byte;
    btLight: byte; //0..1..4
    btBkIndex: Byte;
    btSmIndex: Byte;   //14
   end;
   pTNewMapInfo = ^TNewMapInfo;
    TMir2Map=class(TGameMap)
    Private
      m_Arr_TilesInfo : array of array of TNewMapInfo;  //二维数组
    Protected

    public
      Constructor Create(FileName:string;TextureWidth,TextureHeight:Integer);override;
      destructor Destroy; override;
      procedure LoadMap(sFileName:string);override;
      procedure DrawTile(x: Integer; y: Integer); override;//xy为地图的像素坐标
      procedure DrawObject(x: Integer; y: Integer); override;
    end;

implementation
uses
System.Classes,System.SysUtils,Share,Texture,ResManager,DrawEx,MondoZenGL;
{ TMir2Map }

constructor TMir2Map.Create(FileName: string; TextureWidth,
  TextureHeight: Integer);
begin
  inherited;
  loadMap(FileName);
end;

destructor TMir2Map.Destroy;
begin

  inherited;
end;

procedure TMir2Map.DrawObject(x, y: Integer);
var
  i,j:Integer;
  Xt,Yt:Integer;
  aX,aY:Integer;
  tex:TTexture;
  ImageIndex:Integer;
  FileIndex:Integer;
  NowRect:TMZRect;
  overLapeTex:TMZTexture;
  OverlapeRect:TMZRect;//被截取纹理的区域
  xx,yy:Single;
begin
  inherited;
  Xt:=(g_nClientWidth div UNITX) + 1;
  Yt:=(g_nClientHeight div UNITY ) + 1;
  if not Assigned(m_Scene) then Exit;
  // 计算本次绘制的区域。
  NowRect.X:=x;
  NowRect.Y:=0;
  NowRect.W:=TMZApplication.Instance.ScreenWidth;
  NowRect.H:=TMZApplication.Instance.ScreenHeight;
  //判断本次绘制的区域和上次绘制的区域是否存在重叠区域。
  if NowRect.Equals(m_LastDrawRect) then Exit;{如果本次区域和上次区域相同的话则退出}
  xx := abs(m_LastDrawRect.X-NowRect.X);
  yy := Abs(m_LastDrawRect.Y-NowRect.y);
  OverlapeRect.W := NowRect.W-xx;
  OverlapeRect.H := NowRect.H-yy;
  OverlapeRect.X := X-m_LastDrawRect.X;
  OverlapeRect.Y := Y-m_LastDrawRect.Y;
  if OverlapeRect.X < 0 then OverlapeRect.X:=0;
  if OverlapeRect.Y < 0 then OverlapeRect.Y:=0;
  //如果存在重叠区域。那么把重叠区域纹理取出来
  overLapeTex:=m_ObjsTarget.Texture;

  //把重叠区域绘制在目标纹理上。
 // m_Scene.Canvas.RenderTarget:=m_ObjsTarget;
  //绘制非重叠区域的纹理。

  for I := 0 to xt do
  begin
    for j := 0 to Yt do
    begin
      //如果I,J不在合法范围之内 则跳过
      aX := x+i;
      aY := y+j;
      if (aX < 0) or (aX >= m_nWidth)  then Continue;
      if (aY < 0) or (aY >= m_nHeight) then Continue;
      ImageIndex := m_Arr_TilesInfo[aX,aY].wFrImg and $7FFF;
      FileIndex := m_Arr_TilesInfo[aX,aY].btArea+1;
      tex := TResManager.GetInstance.GetTexture(MAPOBJ,FileIndex,ImageIndex);
      DrawTexture2Canvas(m_Scene.Canvas,Tex.m_Texture,i*UNITX,j*UNITY);
    end;
  end;

end;

procedure TMir2Map.DrawTile(x, y: Integer);
begin
  inherited;

end;

procedure TMir2Map.LoadMap(sFileName: string);
  var
  Header         :TMapHeader;
  FileStream     :TFileStream;
  x,y            :Integer;
  pinfo          :pTNewMapinfo;
  isNewMap       :Boolean;
begin
  inherited;
  //判断已经打开的地图文件是不是和当前地图文件一样。确保不要重复操作

  if sFileName=m_sMapPath then Exit;
   //读取地图大小。设置数组长度。
    m_sMapPath := sFileName;
  try
    FileStream := TFileStream.Create(m_sMapPath,fmOpenRead);
    FileStream.Read(Header,SizeOf(TMapHeader));
    if Header.btVersion = 15 then IsNewMap := True else IsNewMap:=False;
    SetLength(m_Arr_TilesInfo,Header.wWidth,Header.wHeight);
    SetLength(m_bArr_WalkFlag,Header.wWidth,Header.wHeight);
    m_nWidth  := Header.wWidth;
    m_nHeight := Header.wHeight;
    for x := 0 to Header.wWidth-1 do
      begin
        for y := 0 to Header.wHeight-1 do
          begin
            if isNewMap then
              FileStream.Read(m_Arr_TilesInfo[x,y],SizeOf(TNewMapInfo))
            else
            begin
              FileStream.Read(m_Arr_TilesInfo[x,y],SizeOf(TMapInfo));
              m_Arr_TilesInfo[x,y].btBkIndex:=0;
              m_Arr_TilesInfo[x,y].btSmIndex:=0;
            end;
            pinfo := @m_Arr_TilesInfo[x,y];
            with pinfo^ do
            begin   {三个字段中只要有一个字段的最高位是1就说明不能行走}
              if (wbkImg or wMidImg or wFrImg) >$7FFF then
                m_bArr_WalkFlag[x,y] := True
                else
                m_bArr_WalkFlag[x,y] := False;
            end;
          end;
      end;
  finally
    FileStream.Free;
  end;
end;

end.
