unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, RzCommon, RzSelDir, RzShellDialogs, KPP, pngimage, GameImages, Mir2ImageFactory, Wil, Wzl
  , HUtil32;
type
  TMainForm = class(TForm)
    edt_Src: TEdit;
    edt_Dest: TEdit;
    btn_convert: TButton;
    lbl1: TLabel;
    lbl2: TLabel;
    btn_src: TButton;
    btn_dest: TButton;
    pb: TProgressBar;
    lst: TListBox;
    SelDestDir: TRzSelectFolderDialog;
    SelSrcDir: TRzSelectFolderDialog;
    rb1: TRadioButton;
    rb2: TRadioButton;
    rb3: TRadioButton;
    stat: TStatusBar;
    procedure btn_srcClick(Sender: TObject);
    procedure btn_destClick(Sender: TObject);
    procedure rb3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rb2Click(Sender: TObject);
    procedure rb1Click(Sender: TObject);
    procedure btn_convertClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  FileList: TStringList;
procedure MakeFileList(Path, FileExt: string; var List: TStringList);
implementation

{$R *.dfm}

procedure TMainForm.btn_convertClick(Sender: TObject);
var
  Mir2: TGameImages;
  KPP: TKPPFile;
  I, j: integer;
  sFile: string;
  Png: TPNGObject;
  BMP: TBitmap;
  x, y: Integer;
  coor: TCoordinate;
begin
//循环打开文件列表。创建MIR2资源对象
  pb.Max := FileList.Count - 1;
  pb.Step := 1;
  pb.Position := 0;
  for i := 0 to FileList.Count - 1 do
  begin
    sFile := FileList[i];
    Mir2 := GetGameImage(sFile);
    KPP := TKPPFile.Create(edt_Dest.Text + '\' + ExtractFileNameOnly(sFile) + '.KPP');
    KPP.CreateFile;
    Png := TPNGObject.Create;
    for j := 0 to Mir2.ImageCount - 1 do
    begin
      BMP := Mir2.GetBitmap(j, x, y);
      Png.Assign(BMP);
      Png.TransparentColor := clBlack;
      KPP.Append(Png);
      coor.X := x;
      coor.y := y;
      KPP.Coordinate[j] := Coor;
      stat.Panels[0].Text := sFile;
      stat.Panels[1].text := IntToStr(j) + '/' + Inttostr(Mir2.ImageCount - 1);
      Application.ProcessMessages;
      BMP.Free;
    end;
    KPP.Free;
    Mir2.Free;
    pb.Position := i;
  end;

end;

procedure TMainForm.btn_destClick(Sender: TObject);
begin
  if SelDestDir.Execute then
  begin
    edt_Dest.Text := SelDestDir.SelectedPathName;
  end;
end;

procedure TMainForm.btn_srcClick(Sender: TObject);
begin
  if SelSrcDir.Execute then
  begin
    edt_Src.Text := SelSrcDir.SelectedPathName;
    rb2.Checked := False;
    rb1.Checked := False;
    rb3.Checked := True;
    rb3Click(nil);
  end;

end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  FileList := TStringList.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FileList.Free;
end;

procedure TMainForm.rb1Click(Sender: TObject);
begin
  FileList.Clear;
  MakeFileList(edt_Src.Text, '.wil', FileList);
  lst.Items.Assign(FileList);
end;

procedure TMainForm.rb2Click(Sender: TObject);
begin
  FileList.Clear;
  MakeFileList(edt_Src.Text, '.wzl', FileList);
  lst.Items.Assign(FileList);
end;

procedure TMainForm.rb3Click(Sender: TObject);
begin
  FileList.Clear;
  MakeFileList(edt_Src.Text, '.wil', FileList);
  MakeFileList(edt_Src.Text, '.wzl', FileList);
  lst.Items.Assign(FileList);
end;

procedure MakeFileList(Path, FileExt: string; var List: TStringList);
var
  sch: TSearchrec;
begin
  if List = nil then List := TStringlist.Create;
  Path := Trim(Path);
  if Path[Length(Path)] <> '\' then Path := Path + '\';

  if not DirectoryExists(Path) then
  begin
    List.Clear;
    exit;
  end;
  if FindFirst(Path + '*', faAnyfile, sch) = 0 then
  begin
    repeat
      Application.ProcessMessages;
      if ((sch.Name = '.') or (sch.Name = '..')) then Continue; //如果找到的是当前目录 就继续下一次查找

      if DirectoryExists(Path + sch.Name) then //如果某个目录存在，则进入这个目录递归找到文件
      begin
        MakeFileList(Path + sch.Name, FileExt, List);
      end

      else

      begin
        if (UpperCase(extractfileext(Path + sch.Name)) = UpperCase(FileExt)) or (FileExt = '.*') then
          List.Add(Path + sch.Name);
      end;
    until FindNext(sch) <> 0;
    SysUtils.FindClose(sch);
  end;
end;

end.

