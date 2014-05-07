unit Util32Ex;

interface
function ExtractFileNameOnly(const fname: string): string;
implementation
uses
SysUtils;
function ExtractFileNameOnly(const fname: string): string;
var
  extpos: Integer;
  ext, fn: string;
begin
  ext := ExtractFileExt(fname);
  fn := ExtractFileName(fname);
  if ext <> '' then begin
    extpos := Pos(ext, fn);
    Result := Copy(fn, 1, extpos - 1);
  end else
    Result := fn;
end;
end.
