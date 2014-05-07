unit DrawEx;

interface
uses
MondoZenGL,Texture;
//X Y ÎªÆÁÄ»µÄÎ»ÖÃ
Procedure DrawTexture2Canvas(Canvas:TMZcanvas;Texture:TMZTexture;X,Y:integer);
//Procedure DrawText2Canvas(Canvas:TMZCanvas;Font:TMZFont;x,y:Integer;str:string);
implementation

Procedure DrawTexture2Canvas(Canvas:TMZcanvas;Texture:TMZTexture;X,Y:integer);
var
coor:TMZTextureCoordinates;
rect:TMZRect;
begin
  if not Assigned(Texture) then exit;

  rect.X:=X;
  rect.y:=Y;
  rect.w:=Texture.Width;
  rect.h:=Texture.Height;
  coor[0].X:=0;
  coor[0].y:=1;
  coor[1].X:=1;
  coor[1].y:=1;
  coor[2].X:=1;
  coor[2].Y:=0;
  coor[3].X:=0;
  coor[3].y:=0;
  Canvas.DrawTexture(Texture,coor,rect,0,$FF,[efBlend]);
end;

end.
