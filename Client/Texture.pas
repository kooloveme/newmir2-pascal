unit Texture;

interface
uses
MondoZenGL;
type
  TTexture=class
    Public
    m_Texture:TMZTexture;
    m_nX:Smallint;
    m_nY:Smallint;
    m_nLastCheckTime:Cardinal;
    constructor Create(Tex:TMZTexture;X,Y:Smallint);
    destructor Destroy; override;
  end;

implementation
uses
Classes;

{ TTexture }


constructor TTexture.Create(Tex:TMZTexture;X,Y:Smallint);
begin
m_Texture:=Tex;
m_nX:=X;
m_nY:=Y;
m_nLastCheckTime:=TThread.GetTickCount;
end;

destructor TTexture.Destroy;
begin
  m_Texture.Free;
  inherited;
end;

end.
