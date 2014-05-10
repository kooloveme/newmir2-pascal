unit Mir2Map;

interface
uses
Map;
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
    end;

implementation
uses
System.Classes,System.SysUtils;
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
