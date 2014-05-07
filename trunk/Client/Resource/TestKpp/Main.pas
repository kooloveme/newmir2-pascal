unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, StdCtrls, ComCtrls,KPP,Vcl.Imaging.pngimage,zgl_memory;

type
  TForm2 = class(TForm)
    img: TImage;
    mm: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    stat: TStatusBar;
    btn1: TButton;
    btn2: TButton;
    lbl1: TLabel;
    N4: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    scrlbx1: TScrollBox;
    N5: TMenuItem;
    N6: TMenuItem;
    ZglPmemory1: TMenuItem;
    procedure N2Click(Sender: TObject);
    procedure N3Click(Sender: TObject);
    procedure N4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure N6Click(Sender: TObject);
    procedure ZglPmemory1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;
  KPP:TKPPFile;
  Pos:Integer;
  Png:TPNGImage;

implementation

{$R *.dfm}

procedure TForm2.btn1Click(Sender: TObject);
begin
if Pos=0 then Exit;
Pos:=Pos-1;
Png:=KPP.Png[Pos];
img.Canvas.FillRect(img.ClientRect);
img.Picture.Bitmap.Assign(Png);
lbl1.Caption:=IntToStr(Pos+1)+'/'+InttoStr(KPP.Count);
stat.Panels[0].Text:='X:'+IntToStr(KPP.Coordinate[pos].X);
stat.Panels[1].Text:='Y:'+IntToStr(KPP.Coordinate[pos].y);

end;

procedure TForm2.btn2Click(Sender: TObject);
begin
if Pos=KPP.Count-1 then Exit;
Pos:=Pos+1;
Png:=KPP.Png[Pos];
img.Canvas.FillRect(img.ClientRect);
img.Picture.Bitmap.Assign(Png);
lbl1.Caption:=IntToStr(Pos+1)+'/'+InttoStr(KPP.Count);
stat.Panels[0].Text:='X:'+IntToStr(KPP.Coordinate[pos].X);
stat.Panels[1].Text:='Y:'+IntToStr(KPP.Coordinate[pos].y);
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
KPP.Free;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
KPP:=nil;
Png:=nil;
end;

procedure TForm2.N2Click(Sender: TObject);
begin
if KPP<>nil then KPP.Free;
if dlgOpen.Execute then
begin
  if Png<>nil then Png.Free;
  Pos:=0;
  KPP:=TKPPFile.Create(dlgOpen.FileName);
  KPP.LoadFile;
  Png:=KPP.Png[0];
  img.Picture.Bitmap.Assign(Png);
  lbl1.Caption:=IntToStr(Pos+1)+'/'+InttoStr(KPP.Count);
end;
end;

procedure TForm2.N3Click(Sender: TObject);
begin
if KPP<>nil then KPP.Free;
if dlgSave.Execute then
begin
  KPP:=TKPPFile.Create(dlgSave.FileName);
  KPP.CreateFile;
end;
end;

procedure TForm2.N4Click(Sender: TObject);
begin
if KPP=nil then Exit;
if dlgOpen.Execute then
begin
  if Png=nil then Png:=TPNGImage.create;
  Png.LoadFromFile(dlgOpen.FileName);
  KPP.Append(Png);
end;
end;

procedure TForm2.N6Click(Sender: TObject);
var
i:Integer;
C:TCoordinate;
begin
if KPP=nil then Exit;
for i := 0 to KPP.Count - 1 do
  begin
  c.X:=i;
  c.y:=i;
  KPP.Coordinate[i]:=c;

  end;

end;

procedure TForm2.ZglPmemory1Click(Sender: TObject);
var
F:TMemoryStream;
p:TPNGImage;
begin

end;

end.
