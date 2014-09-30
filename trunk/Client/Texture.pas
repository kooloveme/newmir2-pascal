unit Texture;

interface

uses
  MondoZenGL;

type
  TTexture = class
  private
    m_Texture: TMZTexture;
    m_nX: Smallint;
    m_nY: Smallint;
    m_nLastCheckTime: Cardinal;
  Public
    constructor Create(Tex: TMZTexture; X, Y: Smallint);
    destructor Destroy; override;
    property Texture: TMZTexture Read m_Texture;
    property X: Smallint Read m_nX;
    property Y: Smallint Read m_nY;
    property CheckTime: Cardinal Read m_nLastCheckTime write m_nLastCheckTime;
  end;

implementation

uses
  Classes;

{ TTexture }

constructor TTexture.Create(Tex: TMZTexture; X, Y: Smallint);
begin
  m_Texture := Tex;
  m_nX := X;
  m_nY := Y;
  m_nLastCheckTime := TThread.GetTickCount;
end;

destructor TTexture.Destroy;
begin
  m_Texture.Free;
  inherited;
end;

end.
