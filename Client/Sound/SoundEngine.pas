unit SoundEngine;

interface
 uses
 Classes,MondoZenGL,Generics.Collections;

 Type
     TGameSound=Class
     Private
       FSound:TMZStaticSound;
       FFileName:string;
     Public
       FLastPlayTime:Cardinal;
       Constructor Create(FileName:string);
       destructor Destroy; override;
       Procedure Play (X,Y,Z:Single;Looped:Boolean);
       Procedure FreeSoundMem;
       Procedure ReLoadMem;
     End;

     TSoundEngine=Class
     Private
       FX,FY,FZ:Single;
       FSoundList:TDictionary<string,TGameSound>;
       Class var FInstance:TSoundEngine;
       constructor Create;
     Public
       Class Var Volume:Integer;
       destructor Destroy; override;
       Function RegSound(Desc:string;FileName:String):TGameSound;
       Procedure PlaySound(Desc: String;Looped:Boolean=False);
       Procedure SetXYZ(X,Y,Z:Single);
       class Function GetInstance:TSoundEngine;
     End;
 Procedure PlaySound(Desc:string;Looped:Boolean=False);
implementation
uses
Windows;
Procedure PlaySound(Desc:string;Looped:Boolean=False);
begin
  TSoundEngine.GetInstance.PlaySound(Desc,Looped);
end;
{ TSoundEngine }

constructor TSoundEngine.Create;
begin
FSoundList:=TDictionary<String,TGameSound>.Create();
FX:=0;
FY:=0;
FZ:=0;
end;

destructor TSoundEngine.Destroy;
begin
  FSoundList.Free;
  inherited;
end;

Class function TSoundEngine.GetInstance: TSoundEngine;
begin
if not Assigned(FInstance) then FInstance:=TSoundEngine.Create;
Result:=FInstance;
end;

procedure TSoundEngine.PlaySound(Desc: String;Looped:Boolean);
var
Sound:TGameSound;
begin
Sound:=FSoundList[Desc];
if Assigned(Sound) then Sound.Play(FX,FY,FZ,Looped);
end;

function TSoundEngine.RegSound(Desc, FileName: String): TGameSound;
begin
  Result:=TGameSound.Create(FileName);
  FSoundList.Add(Desc,Result);
end;

procedure TSoundEngine.SetXYZ(X, Y, Z: Single);
begin
FX:=X;
FY:=Y;
FZ:=Z;
end;

{ TGameSound }

constructor TGameSound.Create(FileName: string);
begin
FSound:=TMZStaticSound.Create(FileName);
FFileName:=FileName;
FLastPlayTime:=GetTickCount;
end;

destructor TGameSound.Destroy;
begin
  if Assigned(FSound) then FSound.Free;
  inherited;
end;

procedure TGameSound.FreeSoundMem;
begin
FSound.Free;
end;

procedure TGameSound.Play(X,Y,Z:Single;Looped:Boolean);
begin
 if  not Assigned(FSound) then ReLoadMem;
 if  Assigned(FSound) then
 begin
   FSound.Play(Looped,x,y,z);
   FLastPlayTime:=GetTickCount;
 end
 
end;


procedure TGameSound.ReLoadMem;
begin
if not Assigned(FSound) then FSound:=TMZStaticSound.Create(FFileName);
end;

end.
