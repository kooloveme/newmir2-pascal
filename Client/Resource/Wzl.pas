unit Wzl;

interface
uses
GameImage,Texture,Classes,SysUtils,Util32Ex,MondoZenGL,ZlibEx;
type
  TWzlImageHeader = record
    Title: string[40]; //'WEMADE Entertainment inc.'
    ImageCount: Integer;
    ColorCount: Integer;
    PaletteSize: Integer;
    VerFlag: Integer;
    Flag: Integer;
  end;
  PTWzlImageHeader = ^TWzlImageHeader;

  TWzlImageInfo = record
    bt1: Byte; //bt1=3 8bit bt1=5 16bit
    bt2: Byte;
    bt3: Byte;
    bt4: Byte;
    nWidth: SmallInt;
    nHeight: SmallInt;
    px: SmallInt;
    py: SmallInt;
    Length: Integer;
  end;
  PTWzlImageInfo = ^TWzlImageInfo;

  TWzlIndexHeader = record
    Title: string[40]; //'WEMADE Entertainment inc.'
    IndexCount: Integer;
  end;
  PTWzlIndexHeader = ^TWzlIndexHeader;

  TWzlImage=Class(TGameImage)
  Private
  protected
    function GetCacheTexture(idx: Integer): TTexture; override;
    //给一个指针。颜色位数。大小。转换出来已经转换成纹理的指针。以及大小。
    Procedure ConvertTextureDate(const P: Pointer;const Len: Integer;Colorbit: Byte; out Ptex: Pointer; out outLen:integer);
    Procedure LoadIndex;
  Public
    Constructor Create(FileName:String);
    destructor Destroy; override;
    procedure Init; override;
    procedure CreateFile; override;
    procedure Insert(idx: Integer; pData: Pointer; Len: Integer); override;
    function GetTexture(idx: Integer; AutoFree: Boolean = True): TTexture;
      override;
    End;
implementation
var
ColorPalette: array[0..255] of Cardinal = (
0,4278190208,4278222848,4278222976,4286578688,4286578816,4286611456,4290822336,
4288118869,4291344797,4285756283,4280887597,4283585114,4284111459,4281940290,4279769117,
4279242776,4279769129,4278716432,4285626866,4284442593,4284111615,4281414143,4283587286,
4278194324,4279773588,4278192185,4278194291,4278196405,4283589565,4279244866,4288260863,
4278194266,4280891763,4281420453,4285758356,4281422525,4279247186,4279775611,4279244845,
4281420428,4278200724,4278202813,4283593670,4279775595,4282543046,4278209230,4281951141,
4279775578,4278194218,4278192149,4278196282,4278190088,4278190121,4278190154,4278190237,
4278190300,4278190302,4278190331,4283593628,4283067284,4280896115,4279775570,4279782028,
4279321736,4278198602,4279244833,4284126422,4280380358,4278217711,4278220799,4286878885,
4280365378,4278718488,4278720553,4278194209,4279773497,4281951116,4279249218,4279779947,
4279782011,4278209172,4286284940,4284113771,4281942602,4279771433,4280891718,4287931829,
4284115835,4287934926,4285762725,4284117900,4285764789,4285769174,4283082223,4287416047,
4282540923,4281947755,4284126397,4278204771,4289578710,4280894034,4279788436,4289582831,
4284714149,4283062883,4286293437,4279779930,4281437373,4280889653,4284712084,4283067259,
4284124325,4280896090,4281957276,4279251266,4280397295,4278194200,4278198569,4278217628,
4284122260,4279779922,4280900203,4280378235,4280384412,4278232542,4281946714,4279249201,
4286299598,4281948771,4283073684,4280919494,4279802896,4283075650,4282551345,4280914960,
4279244808,4279769096,4279249160,4280893976,4289574309,4285756267,4280887576,4283056664,
4283056689,4292789859,4294958404,4293908108,4281953139,4281982711,4287426551,4278249463,
4284115819,4289039450,4293899577,4291730506,4290085937,4285223473,4292271838,4290100669,
4286876812,4292802551,4279764992,4281931784,4280881160,4278196232,4278200584,4289024512,
4292770560,4283050256,4285217040,4287386128,4289026593,4284100880,4286857744,4286861873,
4281409816,4286274122,4289030994,4284692777,4292758032,4280363305,4281944650,4279773481,
4280896074,4282547067,4283079836,4280900186,4279517762,4278204729,4278212953,4281087434,
4280382315,4278202665,4279253297,4279777585,4278209090,4279788370,4280906586,4279781937,
4278198552,4278202648,4279253272,4283073635,4283088235,4283086179,4283088227,4283079770,
4281961546,4283090531,4283094627,4283073618,4280906545,4284139107,4283088210,4278255376,
4279773464,4283074634,4283098954,4278213120,4278224896,4278227968,4278246912,4278251008,
4278254336,4287912522,4290081635,4292250747,4292246379,4294936695,4291741382,4288451732,
4291204252,4281938225,4286847017,4286840856,4283581002,4286267986,4285749859,4294424014,
4288445324,4291568247,4294945501,4280988912,4288610527,4289927139,4293983231,4288979104,
4286611584,4278190335,4278255360,4278255615,4294901760,4294902015,4294967040,4294967295);

    { TWzlImage }

procedure TWzlImage.ConvertTextureDate(const P: Pointer;const Len: Integer;Colorbit: Byte; out Ptex: Pointer; out outLen:Integer);
var
  i:Integer;
  Pixel:Integer;
  Pbits:PByte;
  ColorIndex:Byte;
  PBits16:PWord;
  Color565:Word;
  ptexData:PInteger;
begin
 {$POINTERMATH ON}
case Colorbit of
  8:begin
    outLen:=Len*4;
    GetMem(PtexData,OutLen);
    Pbits:=P;
    for I := 0 to Len-1 do
    begin
      ColorIndex:=pbits[i];
      Pixel:=ColorPalette[ColorIndex];
      ptexData[i]:=Pixel;
    end;
  end;
  16:begin
    outLen:=Len*2;
    GetMem(ptexData,OutLen);
    PBits16:=P;
    for i := 0 to (Len div 2) -1 do
    begin
      Color565:=Pbits16[i];
      Pixel:=RGB565ToABGR(Color565);
      ptexData[i]:=Pixel;
    end;

  end;
end;
 Ptex:=ptexData;
{$POINTERMATH OFF}
end;

constructor TWzlImage.Create(FileName: String);
begin
  inherited Create;
  m_sFileName:=FileName;
end;

procedure TWzlImage.CreateFile;
begin
  inherited;
end;

destructor TWzlImage.Destroy;
begin
  inherited;
end;

function TWzlImage.GetCacheTexture(idx: Integer): TTexture;
var
  Offset:Integer;
  ImageInfo:TWzlImageInfo;
  OrginalLen:Integer;
  PZipBits:PByte;
  Pbits:Pointer;
  Pbits16:PWord;
  ColorBit:Byte;
  Pixel:Cardinal;
  ColorPos:Byte;
  PtexData:Pointer;
  TexDataLen:Integer;
  OutZipLen:Integer;
  i: Integer;
  MZTex:TMZTexture;
  ZipLen:Cardinal;
begin
  Result:=nil;
  if  not ((idx >= 0) and (idx < ImageCount) and (m_FileStream <> Nil))  then Exit;
  Offset:=m_nArr_Index[idx];
  m_FileStream.Seek(Offset,soBeginning);
  m_FileStream.Read(ImageInfo,SizeOf(TWzlImageInfo));
  //计算原始长度
  case ImageInfo.bt1 of
    3:begin
      OrginalLen:=ImageInfo.nWidth*ImageInfo.nHeight;
      ColorBit:=8;
    end;
    5:begin
      OrginalLen:=ImageInfo.nWidth*ImageInfo.nHeight*2;
      ColorBit:=16;
    end;
  end;
  MZTex:=TMZTexture.create(ImageInfo.nWidth,ImageInfo.nHeight,$FFFFFFFF,[]);
  if ImageInfo.Length<=0 then //当内部记录长度为0时候说明此文件并未压缩。不需要解压。
  begin
    //直接读入不需要解压
    GetMem(Pbits,OrginalLen);
    m_FileStream.Read(Pbits^,OrginalLen);
    ConvertTextureDate(Pbits,OrginalLen,ColorBit,PtexData,TexDataLen);
    FreeMem(Pbits,OrginalLen);
  end else
  begin
    GetMem(PZipBits,ImageInfo.Length);
    m_FileStream.Read(PZipBits^,ImageInfo.Length);
    ZipLen:=OrginalLen;
    DecompressBuf(PZipBits,ImageInfo.Length,ZipLen,Pbits,OutZipLen);
    FreeMem(PZipBits,ImageInfo.Length);
    ConvertTextureDate(Pbits,OutZipLen,ColorBit,PtexData,TexDataLen);
    FreeMem(Pbits);
  end;

  //组合纹理
  MZTex.SetData(PTexData,0,0,ImageInfo.nWidth,ImageInfo.nHeight);
  FreeMem(PtexData,TexDataLen);
  Result:=TTexture.Create(MZTex,ImageInfo.px,ImageInfo.py);

end;

function TWzlImage.GetTexture(idx: Integer; AutoFree: Boolean): TTexture;
var
List:TList;
begin
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
  end
end;

procedure TWzlImage.Init;
var
  Header:TWzlImageHeader;
  List:TList;
begin
  inherited;
  if not FileExists(m_sFileName) then Exit;
  try
    m_FileStream:=TFileStream.Create(m_sFileName,fmOpenRead or fmShareExclusive);
    m_FileStream.Read(Header,SizeOf(TWzlImageHeader));
    m_nFImageCount:=Header.ImageCount;
    List:=m_TextureList.lockList;
    List.Count:=m_nFImageCount;
    m_TextureList.UnLockList;
    LoadIndex;
  finally

  end;
end;

procedure TWzlImage.Insert(idx: Integer; pData: Pointer; Len: Integer);
begin
  inherited;
end;

procedure TWzlImage.LoadIndex;
var
  S:TFileStream;
  Header:TWzlIndexHeader;
  idxFile:string;
  IndexCount:integer;
begin
  idxFile:=ExtractFilePath(m_sFileName) + ExtractFileNameOnly(m_sFileName) + '.WZX';
  if FileExists(idxFile) then
  begin
    try
      S:=TFileStream.Create(idxFile,fmOpenRead);
      s.Read(Header,SizeOf(TWzlIndexHeader));
      IndexCount:=Header.IndexCount;
      SetLength(m_nArr_Index,IndexCount);
      s.Read(m_nArr_Index[0],IndexCount*4);
    finally
      S.Free;
    end;
  end;
end;

end.
