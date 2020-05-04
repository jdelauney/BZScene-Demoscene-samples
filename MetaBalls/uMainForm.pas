unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch;

Const
  cMetaBallCount = 16;
  cMetaBallsRadius = 160;
Type
  TMetaBall = record
    x,y, adx, ady, r: integer;
  end;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
  private
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;
    FDisplayBuffer : TBZBitmap;
    FMetaBalls : Array[0..cMetaBallCount-1] of TMetaBall;
    FDensity : Array of Array of Integer;
    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    procedure InitScene;
    procedure RenderScene;
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

uses
  BZMath;

{ TMainForm }

procedure TMainForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  FCadencer.Enabled := False;
  FStopWatch.Stop;
  CanClose := true;
end;

procedure TMainForm.FormCreate(Sender : TObject);
begin
  FDisplayBuffer := TBZBitmap.Create(Width,Height);
  FDisplayBuffer.Clear(clrBlack);

  FCadencer := TBZCadencer.Create(Self);
  FCadencer.Enabled := False;
  FCadencer.OnProgress := @CadencerProgress;

  FStopWatch := TBZStopWatch.Create(self);
end;

procedure TMainForm.FormDestroy(Sender : TObject);
var i : integer;
begin
  For i:= 0 to FDisplayBuffer.MaxHeight do
  begin
    SetLength(FDensity[i],0);
    FDensity[i] := nil;
  end;
  SetLength(FDensity,0);
  FDensity := Nil;

  FreeAndNil(FStopWatch);
  FreeAndNil(FCadencer);
  FreeAndNil(FDisplayBuffer);
end;

procedure TMainForm.FormShow(Sender : TObject);
begin
  InitScene;
  DoubleBuffered:=true;
  FStopWatch.Start;
  FCadencer.Enabled := True;
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  RenderScene;
  //FDisplayBuffer.BlurFilter.LinearBlur;
  FDisplayBuffer.DrawToCanvas(Canvas, ClientRect);
  Caption:='BZScene - MetaBalls Demo : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
end;

procedure TMainForm.InitScene;

Var
  i,x,y : Integer;
  Distance, q : Single;
begin
  Randomize;
  For i:=0 to cMetaBallCount-1 do
  Begin
    FMetaBalls[i].x := Random(Width);
    FMetaBalls[i].y := Random(Height);
    FMetaBalls[i].r := Random(60)+32;  //cMetaBallsRadius;
    FMetaBalls[i].adx := Random(5)+1;
    FMetaBalls[i].ady := Random(5)+1;
  end;

  SetLength(FDensity, FDisplayBuffer.Height);
  For y:= 0 to FDisplayBuffer.MaxHeight do
  begin
    SetLength(FDensity[y],FDisplayBuffer.Width);
    For x := 0 to FDisplayBuffer.MaxWidth do
    begin
      //distance := System.sqrt( sqr((cMetaBallsRadius*2)-x) + sqr((cMetaBallsRadius*2)-y) ) ;
      distance := System.sqrt(sqr(x) + sqr(y));
      if distance > cMetaBallsRadius then q:=1 //-1
      else q :=  distance / cMetaBallsRadius ;
      FDensity[y,x]:= round(sqr(q-1)*256); // round(q*16);// FDensity[y,x]:= round(sqr(1-q)*256);

    end;
  end;
end;

procedure TMainForm.RenderScene;
var
  i, x, y: integer;
  distance, density, u, v: integer;
  brightness, c : byte;
  fraction: Single;
  OutColor : TBZColor;
begin
  //FDisplayBuffer.Clear(clrNavy);
  y:=0;
  While (y < FDisplayBuffer.MaxHeight) do
  //for y := 0 to FDisplayBuffer.MaxHeight do
  begin
    x := 0;
    //for x := 0 to FDisplayBuffer.MaxWidth do
    While (x < FDisplayBuffer.MaxWidth) do
    begin
      Brightness:=0;
      Density := 0;
      for i := 0 to cMetaBallCount - 1 do
      begin
       (* Version non optimisée
        distance := round(sqrt(sqr(FMetaBalls[i].x-x) + sqr(FMetaBalls[i].y-y)));
        if distance > FMetaBalls[i].r then density:=0
        else
        begin
          fraction := sqr(distance/FMetaBalls[i].r);
          density := round(sqr(1-fraction)*256);
        end;

        Brightness:=(Brightness+density) mod 160; //
      end; *)

        //u := abs(x - FMetaBalls[i].x);
        //v := abs(y - FMetaBalls[i].y);
        u := Abs(FMetaBalls[i].x-x);
        v := Abs(FMetaBalls[i].y-y);
        Density := FDensity[v,u]; //Density +

        Brightness:= (Brightness+Density) and 255; //mod 160;
      end;

      //FDisplayBuffer.SetPixel(x,y,BZColor(Brightness, Brightness, Brightness));
      //if BrightNess <= 96 then c := 255-Brightness else
      c := Brightness;// mod 160;

      //if (c > 0) then
      //begin
        OutColor.Create(c,ClampByte(c+32),ClampByte(c+92));
        FDisplayBuffer.SetPixel(x,y,OutColor);
        FDisplayBuffer.SetPixel(x+1,y,OutColor);
        FDisplayBuffer.SetPixel(x,y+1,OutColor);
        FDisplayBuffer.SetPixel(x+1,y+1,OutColor);
      //end;
      inc(x,2);
    end;
    inc(y,2);
  end;

  for i := 0 to cMetaBallCount-1 do
  begin
      if (FMetaBalls[i].x+FMetaBalls[i].r > FDisplayBuffer.MaxWidth) then
      begin
        FMetaBalls[i].adx := -FMetaBalls[i].adx;
        FMetaBalls[i].x := FDisplayBuffer.Width-FMetaBalls[i].r
      end;

      if (FMetaBalls[i].x-FMetaBalls[i].r < 0) then
      begin
        FMetaBalls[i].adx := -FMetaBalls[i].adx;
        FMetaBalls[i].x := FMetaBalls[i].r
      end;

      FMetaBalls[i].x:=FMetaBalls[i].x + FMetaBalls[i].adx;

      if (FMetaBalls[i].y+FMetaBalls[i].r > FDisplayBuffer.MaxHeight) then
      begin
        FMetaBalls[i].ady := -FMetaBalls[i].ady;
        FMetaBalls[i].y := FDisplayBuffer.Height-FMetaBalls[i].r
      end;

      if (FMetaBalls[i].y-FMetaBalls[i].r < 0) then
      begin
        FMetaBalls[i].ady := -FMetaBalls[i].ady;
        FMetaBalls[i].y := FMetaBalls[i].r
      end;

      FMetaBalls[i].y := FMetaBalls[i].y + FMetaBalls[i].ady;
    end;

end;

end.

