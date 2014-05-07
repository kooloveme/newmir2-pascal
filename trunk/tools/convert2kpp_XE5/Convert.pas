unit Convert;

interface
uses
  classes, wil, wis, wzl, Mir2ImageFactory, GameImages, kpp;
type
  TConvertor = class(TThread)
  private
    FFilePath: string;
    FOutDir: string;
    FMir2: TGameImages;
    FKPP: TKPPFile;
  protected
    procedure Execute; override;
  public
    FinishedCount:integer;
    Imagecount:integer;
    constructor Create(sFile, sOutDir: string);
    destructor Destroy; override;
  end;
var
  Percent: integer = 0;
implementation
uses Hutil32, PngImage, graphics;
{ TConvertor }

constructor TConvertor.Create(sFile, sOutDir: string);
begin
  inherited Create(True);
  FreeOnTerminate:=False;
  FFilePath := sfile;
  FOutDir := soutdir;
  FreeOnTerminate := False;
  FMir2 := GetGameImage(FFilePath);
  FKPP := TKPPFile.Create(FOutDir + '\' + ExtractFileNameOnly(FFilePath) +
    '.KPP');
  FKPP.CreateFile;
  Imagecount:=FMir2.ImageCount;
end;

destructor TConvertor.Destroy;
begin
  FKPP.Free;
  FMir2.Free;
  inherited;
end;

procedure TConvertor.Execute;
var

  Png: TPngImage;
  BMP: TBitMap;
  i, j, x, y: integer;
  coor: TCoordinate;
begin
  inherited;
  Png := TPNGImage.Create;
  for j := 0 to FMir2.ImageCount - 1 do
  begin
    BMP := FMir2.GetBitmap(j, x, y);
    Png.Assign(BMP);
    Png.TransparentColor := clBlack;
    FKPP.Append(Png);
    coor.X := x;
    coor.y := y;
    FKPP.Coordinate[j] := Coor;
    BMP.Free;
    Percent:=Percent+1;
    FinishedCount:=j+1;
  end;
  Png.Free;

end;

end.

