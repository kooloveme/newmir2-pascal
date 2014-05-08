unit Map;

interface
uses
MondoZengl;
type
  MapType=(Mir2,Mir2New,Mir3);
  TMapHeader =packed record
     wWidth  : word;
     wHeight : word;
     btVersion:Byte;  //=15时候表示支持多个tiles 和smtiles
     Title: string[13];
     UpdateDate: TDateTime;
     Reserved  : array[0..24] of char;
  end;
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

    TGameMap=class
    Protected
      TargetTexture:TMZRenderTarget;
    Public
     // Constructor Create();
     // destructor Destroy; override;
      Procedure DrawTile(x,y:integer);virtual;abstract;
      Procedure DrawObject(x,y:Integer);virtual;abstract;
    end;


Function EstimateMapType(sFileName:String):MapType;
implementation

Function EstimateMapType(sFileName:String):MapType;
begin

end;
end.
