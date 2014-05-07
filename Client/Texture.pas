unit Texture;

interface
uses
MondoZenGL;
type
  TTexture=class
    Public
    Texture:TMZTexture;
    X:Smallint;
    Y:Smallint;
    LastCheckTime:Cardinal;
    constructor Create(Tex:TMZTexture;XX,YY:Smallint);
    destructor Destroy; override;
  end;

implementation
uses
Classes;

{ TTexture }


constructor TTexture.Create(Tex:TMZTexture;XX,YY:Smallint);
begin
Texture:=Tex;
X:=XX;
Y:=YY;
LastCheckTime:=TThread.GetTickCount;
end;

destructor TTexture.Destroy;
begin
  Texture.Free;
  inherited;
end;

end.
