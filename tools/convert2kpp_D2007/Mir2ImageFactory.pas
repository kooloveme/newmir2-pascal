unit Mir2ImageFactory;

interface
uses
GameImages,SysUtils,Wil,Wzl;
function GetGameImage(sFile:String):TGameImages;
implementation
function GetGameImage(sFile:String):TGameImages;
var
ext:string;
begin
ext:=ExtractFileExt(sFile);
if UpperCase(ext)='.WZL' then
begin
  Result:=TWzlImages.Create;
  Result.FileName:=sFile;
  Result.Initialize;
end;

if UpperCase(ext)='.WIL' then
begin
  Result:=TWilImages.Create;
  Result.FileName:=sFile;
  Result.Initialize;
end;
end;


end.
