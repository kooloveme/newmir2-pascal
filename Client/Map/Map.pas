unit Map;

interface
uses
MondoZengl;
type
  MapType=(Mir2,Mir2New,Mir3,Unknow);
  TMapHeader =packed record
     wWidth  : word;
     wHeight : word;
     btVersion:Byte;  //=15时候表示支持多个tiles 和smtiles
     Title: string[13];
     UpdateDate: TDateTime;
     Reserved  : array[0..24] of char;
  end;


  TGameMap=class
    Protected
      m_nWidth         :Integer;
      m_nHeight        :Integer;
      m_TargetTexture  :TMZRenderTarget; //地图表面的纹理
      m_sMapPath       :string; //地图文件路径。
      m_sMapCode       :string; //地图代码 M001.MAP 那么这个地图代码就是M001;
      m_bArr_WalkFlag  :array of array of Boolean;
      m_Scene          :TMZScene;
      Function GetWalkFlag(x,y :integer):Boolean;
    Public
       Constructor Create(FileName :string;TextureWidth,TextureHeight :Integer);virtual;
       destructor Destroy; override;
       Procedure LoadMap(sfilename:string);virtual;abstract;
      Procedure DrawTile(x,y :integer);virtual;abstract;
      Procedure DrawObject(x,y :Integer);virtual;abstract;
      property WalkFlag[x,y : integer] : Boolean Read GetWalkFlag;
      property Width  : Integer Read m_nWidth;
      Property Height : Integer Read m_nHeight;
    end;


Function EstimateMapType(sFileName:String):MapType;
implementation
uses
sysutils,System.Classes;

Function EstimateMapType(sFileName:String):MapType;
var
  FileStream:TFileStream;
  Header:TMapHeader;
begin
   Result:=Unknow;
   if FileExists(sFileName) then
   begin
     try
      FileStream := TFileStream.Create(sFileName,fmOpenRead);
      FileStream.Read(Header,SizeOf(TMapHeader));
      if Header.btVersion=15  then Result:= Mir2New
      else Result:=Mir2;
     finally
      FileStream.Free;
     end;

   end;

end;
{ TGameMap }

constructor TGameMap.Create(FileName: string; TextureWidth,
  TextureHeight: Integer);
begin
  m_sMapPath      := FileName;
  m_TargetTexture := TMZRenderTarget.Create(TMZTexture.Create(TextureWidth,TextureHeight,0,[]));
  m_sMapCode      := '';
end;

destructor TGameMap.Destroy;
begin
  m_TargetTexture.Free;
  SetLength(m_bArr_WalkFlag,0);
  inherited;
end;

function TGameMap.GetWalkFlag(x, y:integer): Boolean;
begin
  Result:=False;
     {首先要判断查询的坐标值是不是在合法范围,以免越界报错}
  if not ((x >= 0) and (x < m_nWidth)) then Exit;
  if not ((y >= 0) and (y < m_nHeight)) then Exit;
  Result := m_bArr_WalkFlag[x,y];
end;


end.
