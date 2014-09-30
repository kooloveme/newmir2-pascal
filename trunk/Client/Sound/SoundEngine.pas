unit SoundEngine;

interface

uses
  Classes, MondoZenGL, Generics.Collections,zgl_sound;

Type
  TGameSound = Class
  Private
    FSound : zglPSound;
    FFileName: string;
  Public
    FLastPlayTime: Cardinal;
    Constructor Create(FileName: string);
    destructor Destroy; override;
    Procedure Play(X, Y, Z: Single; Looped: Boolean);
  End;

  TSoundEngine = Class
  Private
    FX, FY, FZ: Single;
    FSoundList: TDictionary<string, TGameSound>;
    Class var FInstance: TSoundEngine;
    constructor Create;
  Public
    Class Var Volume: Integer;
    destructor Destroy; override;
    Function RegSound(Desc: string; FileName: String): TGameSound;
    Procedure PlaySound(Desc: String; Looped: Boolean = False);
    Procedure SetXYZ(X, Y, Z: Single);
    class Function GetInstance: TSoundEngine;
  End;

Procedure PlaySound(Desc: string; Looped: Boolean = False);

implementation

uses
  Windows;

Procedure PlaySound(Desc: string; Looped: Boolean = False);
begin
  TSoundEngine.GetInstance.PlaySound(Desc, Looped);
end;

{ TSoundEngine }

constructor TSoundEngine.Create;
begin
  FSoundList := TDictionary<String, TGameSound>.Create();
  FX := 0;
  FY := 0;
  FZ := 0;
end;

destructor TSoundEngine.Destroy;
var
  Sound:TGameSound;
  I:Integer;
begin
  for I := FSoundList.Count - 1 Downto 0 do
  begin
    Sound :=FSoundList.Values.ToArray[i];
    Sound.Free;
  end;
  FSoundList.Free;
  inherited;
end;

Class function TSoundEngine.GetInstance: TSoundEngine;
begin
  if not Assigned(FInstance) then
    FInstance := TSoundEngine.Create;
  Result := FInstance;
end;

procedure TSoundEngine.PlaySound(Desc: String; Looped: Boolean);
var
  Sound: TGameSound;
begin
  Sound := FSoundList[Desc];
  if Assigned(Sound) then
    Sound.Play(FX, FY, FZ, Looped);
end;

function TSoundEngine.RegSound(Desc, FileName: String): TGameSound;
begin
  Result := TGameSound.Create(FileName);
  FSoundList.Add(Desc, Result);
end;

procedure TSoundEngine.SetXYZ(X, Y, Z: Single);
begin
  FX := X;
  FY := Y;
  FZ := Z;
end;

{ TGameSound }

constructor TGameSound.Create(FileName: string);
var
  sSoundFile:UTF8String;
begin
  sSoundFile := UTF8Encode(FileName);
  FSound := snd_LoadFromFile(sSoundFile, 1);
  FFileName := FileName;
  FLastPlayTime := GetTickCount;
end;

destructor TGameSound.Destroy;
begin
  if Assigned(FSound) then
  begin
    snd_Stop(FSound,-1);
    snd_Del(FSound);
  end;
  inherited;
end;

procedure TGameSound.Play(X, Y, Z: Single; Looped: Boolean);
begin
  if Assigned(FSound) then
  begin
    snd_Play(FSound, Looped, X, Y, Z);
    FLastPlayTime := GetTickCount;
  end

end;

initialization
finalization
end.
