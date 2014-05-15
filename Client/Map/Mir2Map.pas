unit Mir2Map;

interface
uses
Map;
type
   TMapInfo = record
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

  TNewMapInfo =Packed record  //需要关闭结构体对齐
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
      m_nAniCount    :Cardinal;
      m_dwAniTime     :Cardinal;
    Protected

    public
      Constructor Create;override;
      destructor Destroy; override;
      procedure LoadMap(sFileName:string);override;
      procedure DrawTile(x: Integer; y: Integer); override;//xy为地图的像素坐标
      procedure DrawObject(x: Integer; y: Integer); override;
    end;

implementation
uses
windows,{GetTickCount需要}
System.Classes,
System.SysUtils,{文件流需要}
Texture,
ResManager, {获取纹理需要}
DrawEx, {绘制需要}
MondoZenGL,{渲染到纹理需要}
Math; {取整需要}
{ TMir2Map }

constructor TMir2Map.Create;
begin
  inherited;
  m_nAniCount:=0;
  m_dwAniTime:=GetTickCount;
end;

destructor TMir2Map.Destroy;
begin

  inherited;
end;

procedure TMir2Map.DrawObject(x, y: Integer);{此功能未实现}
var
  {i,j:Integer;
  Xt,Yt:Integer;
  aX,aY:Integer;
  NowRect:TMZRect;
  overLapeTex:TMZTexture;
  OverlapeRect:TMZRect;//被截取纹理的区域
  xx,yy:Single;}

  LoopX,LoopY,cX,cY:Integer;
  MapInfo:pTNewMapInfo;
  AniFrame:Byte;
  AniTick:Byte;
  NeedBlend:Boolean;
  tex:TTexture;
  ObjImageIndex:Integer;
  ObjFileIndex:Byte;
begin
  inherited;
 (* Xt:=(g_nClientWidth div UNITX) + 1;
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
  end; *)

  {处理动画相关，50毫秒播放一帧动画}
  if GetTickCount - m_dwAniTime >= 50 then
  begin
    m_dwAniTime := GetTickCount;
    Inc(m_nAniCount);
    if m_nAniCount > 100000 then m_nAniCount := 0;
  end;
  m_Scene.Canvas.RenderTarget:=m_ObjsTarget;
  cX := Floor(x / UNITX);
  cY := Floor(y / UNITY);
  for LoopX := cX to (m_ObjsTarget.Texture.Width div UNITX)+cX do
  begin
    for LoopY := cY to (m_ObjsTarget.Texture.Height div UNITY)+ cY + 15  do
    begin
      tex := nil;
      if (loopX < 0) or (loopX >= m_nWidth) or (loopY < 0) or (loopY >= m_nHeight)then Continue;
      MapInfo := @m_Arr_TilesInfo[LoopX,LoopY];
      ObjImageIndex := MapInfo.wFrImg and $7FFF;
      if ObjImageIndex > 0 then
      begin
        NeedBlend := False;
        ObjFileIndex := MapInfo.btArea + 1;//因为资源管理的最低下标为 1。
        if (MapInfo.btAniFrame and $80) > 0 then //最高位为1表示此帧需要Blend处理。实际有效帧最多能有128帧
        begin
          NeedBlend := TRUE;
          AniFrame := MapInfo.btAniFrame and $7F;
          if AniFrame > 0 then
          begin
            AniTick := MapInfo.btAniTick;
            ObjImageIndex := ObjImageIndex + (m_nAniCount mod (AniFrame + (AniFrame * AniTick))) div (1 + AniTick);
          end;
        end;


        if (MapInfo.btDoorOffset and $80) > 0 then // 门的最高位1表示此处有门
        begin
           if (MapInfo.btDoorIndex and $7F) > 0 then
              ObjImageIndex :=  ObjImageIndex + (MapInfo.btDoorOffset and $7F);
        end;

        Dec(ObjImageIndex);
        {按照如上处理方式，传奇地图优先处理 门，再是处理动画。最后才是普通的obj}
        tex:=TResManager.GetInstance.GetTexture(MAPOBJ,ObjFileIndex,ObjImageIndex);
        if tex = nil then Continue;

        if (tex.Texture.Width=UNITX) and (tex.Texture.Height=UNITY) then
        begin
          DrawTexture2Canvas(m_Scene.Canvas,tex.Texture,(LoopX-cX)*UNITX,(LoopY-cY)*UNITY);
        end else
        begin
          if NeedBlend then
          begin
            DrawTexture2Canvas(m_Scene.Canvas,tex.Texture,(LoopX-cX)*UNITX+tex.X-2 , (LoopY-cY)*UNITY - tex.Texture.Height+UNITY+tex.Y);
          end else
          begin
            DrawTexture2Canvas(m_Scene.Canvas,tex.Texture,(LoopX-cX)*UNITX+tex.X , (LoopY-cY)*UNITY-tex.Texture.Height+UNITY);
          end;

        end;

      end;
    end;

  end;
  m_Scene.Canvas.RenderTarget:=Nil;
  DrawTexture2Canvas(m_Scene.Canvas,m_ObjsTarget.Texture,0,0);

end;


procedure TMir2Map.DrawTile(x, y: Integer);
var
  cX,cY:Integer; //坐标的x,y
  loopX,loopY:Integer;//循环变量
  tex:TTexture;
  MapInfo:pTNewMapInfo;

  TilesImageIndex:Integer;
  TilesFileIndex :Byte;
begin
  inherited;
  {floor（取得小于等于X的最大的整数）
   如：floor(-123.55)=-124，floor(123.55)=123}
  //首先根据X,Y判断出地图所在的cX,cY值
  cX := Floor(x / UNITX);
  cY := Floor(y / UNITY);
  m_Scene.Canvas.RenderTarget:=m_TilesTarget;
  {画Tiles}
  for loopX := cX to (m_TilesTarget.Texture.Width div UNITX)+cX do
    begin
      for loopY := cY to (m_TilesTarget.Texture.Height div UNITY)+cY do
      begin
        {防止数组越界}
        if (loopX < 0) or (loopX >= m_nWidth) or (loopY < 0) or (loopY >= m_nHeight)then Continue;
        MapInfo := @m_Arr_TilesInfo[loopX,LoopY];
        {96*48的地砖只有在坐标xy都为 2的倍数的时候才有数据,所以这里画大地砖Tiles}
        if (loopX mod 2 = 0) and (loopY mod 2 = 0) then
        begin
          TilesImageIndex := MapInfo.wBkImg and $7FFF;
          if TilesImageIndex > 0 then
          begin
            tex := nil;
            Dec(TilesImageIndex);
            TilesFileIndex := (Mapinfo.btBkIndex and $7F) + 1; //因为Tiles数组下标是从 1开始
            tex := TResManager.GetInstance.GetTexture(TILES,TilesFileIndex,TilesImageIndex);
            if tex <> nil then DrawTexture2Canvas(m_Scene.Canvas,tex.Texture,(loopX-cX)*UNITX,(loopY-cY)*UNITY);
          end;
        end;
        {画小地砖Smtiles}
        TilesImageIndex := MapInfo.wMidImg and $7FFF;
        if TilesImageIndex > 0 then
        begin
          tex := nil;
          Dec(TilesImageIndex);
          TilesFileIndex := (MapInfo.btSmIndex and $7F) + 1;
          tex := TResManager.GetInstance.GetTexture(SMTILES,TilesFileIndex,TilesImageIndex);
          if tex <> nil then  DrawTexture2Canvas(m_Scene.Canvas,tex.Texture,(loopX-cX)*UNITX-2,(loopY-cY)*UNITY);
        end;
      end;
    end;
  m_Scene.Canvas.RenderTarget:=nil;
  DrawTexture2Canvas(m_Scene.Canvas,m_TilesTarget.Texture,0,0);
end;

procedure TMir2Map.LoadMap(sFileName: string);
  var
  Header         :TMapHeader;
  FileStream     :TFileStream;
  x,y            :Integer;
  pinfo          :pTNewMapinfo;
  isNewMap       :Boolean;
  RecordLength   :Integer;
begin
  inherited;
  //判断已经打开的地图文件是不是和当前地图文件一样。确保不要重复操作
  if sFileName=m_sMapPath then Exit;
   //读取地图大小。设置数组长度。
    m_sMapPath := sFileName;
  try
    FileStream := TFileStream.Create(m_sMapPath,fmOpenRead);
    FileStream.Read(Header,SizeOf(TMapHeader));
    if Header.btVersion = 15 then
    begin
      IsNewMap := True;
      RecordLength := SizeOf(TNewMapInfo);
    end else
    begin
      IsNewMap:=False;
      RecordLength := SizeOf(TMapInfo);
    end;
    SetLength(m_Arr_TilesInfo,Header.wWidth,Header.wHeight);
    SetLength(m_bArr_WalkFlag,Header.wWidth,Header.wHeight);
    m_nWidth  := Header.wWidth;
    m_nHeight := Header.wHeight;
    for x := 0 to Header.wWidth-1 do
      begin
        for y := 0 to Header.wHeight-1 do
          begin
            FileStream.Read(m_Arr_TilesInfo[x,y],RecordLength);
            if not isNewMap then
            begin
              m_Arr_TilesInfo[x,y].btBkIndex:=0;
              m_Arr_TilesInfo[x,y].btSmIndex:=0;
            end;

            pinfo := @m_Arr_TilesInfo[x,y];
            with pinfo^ do
            begin   {三个字段中只要有一个字段的最高位是1就说明不能行走}
              if (wbkImg or wMidImg or wFrImg) > $7FFF then
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
