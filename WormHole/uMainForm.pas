unit uMainForm;

{$mode objfpc}{$H+}

// Original code by Bas van Gaalen

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch;

Const
  cFormCaption : String = 'BZScene Demo';
  cAStep = 1;


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

    FSintab:array[0..449] of integer;
    FStab,FCtab:array[0..255] of integer;
    FStep, FX, FY, FXSt, FYSt : Integer;
    FC : TBZColor;
    ColorDelta : Byte;
    FPalette : TBZPaletteEntries;


    procedure PutPolarPixel(xo,yo,r,a:Integer; c:TBZColor);

    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    procedure InitEngine;
    procedure StartEngine;
    procedure StopEngine;
    procedure DoneEngine;
    procedure InitScene;
    procedure InitColorMap;
    procedure RenderScene;
    procedure DoneScene;
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  StopEngine;
  CanClose := True;
end;

procedure TMainForm.FormCreate(Sender : TObject);
begin
  InitEngine;
  InitScene;
end;

procedure TMainForm.FormShow(Sender : TObject);
begin
  StartEngine;
end;

procedure TMainForm.FormDestroy(Sender : TObject);
begin
  DoneScene;
  DoneEngine;
end;

procedure TMainForm.PutPolarPixel(xo, yo, r, a : Integer; c : TBZColor);
var
  x,y:word;
  Radius : Byte;
begin
  x := FDisplayBuffer.CenterX + xo +(r * FSintab[90+a]) div 54; //108;
  y := FDisplayBuffer.CenterY + yo +(r * FSintab[a]) div 64;//128;
  Radius := (ColorDelta div 96) + 1 ;// ColorDelta div 16;
  //if (Radius <= 12) and (Radius > 2) then
    With FDisplayBuffer.Canvas do
    begin
      Pen.Style := ssClear;
      Brush.Color := C;
      Brush.Style := bsSolid;
      Circle(x,y,Radius);
    end;

//  FDisplayBuffer.SetPixel(x,y,c);
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  RenderScene;
  FDisplayBuffer.DrawToCanvas(Canvas, ClientRect);
  Caption:=cFormCaption + ' : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
end;

procedure TMainForm.InitEngine;
begin
  FCadencer := TBZCadencer.Create(self);
  FCadencer.Enabled := False;
  FCadencer.OnProgress := @CadencerProgress;
  FStopWatch := TBZStopWatch.Create(self);
  FDisplayBuffer := TBZBitmap.Create(Width, Height);
end;

procedure TMainForm.StartEngine;
begin
  DoubleBuffered:=true;
  FStopWatch.Start;
  FCadencer.Enabled := True;
end;

procedure TMainForm.StopEngine;
begin
  FStopWatch.Stop;
  FCadencer.Enabled := False;
end;

procedure TMainForm.DoneEngine;
begin
  FreeAndNil(FStopWatch);
  FreeAndNil(FCadencer);
  FreeAndNil(FDisplayBuffer);
end;

procedure TMainForm.InitScene;
// Initalisation des données de la scene
Var
  i : Integer;
begin
  for i:=0 to 255 do
  begin
     FCtab[i]:=round(System.Cos(cPI*i/128)*195);
     FStab[i]:=round(System.Sin(cPI*i/128)*95);

  end;
  for i:=0 to 449 do FSintab[i]:=round(System.Sin(c2PI*i/360)*255);

  FX := 30;
  FY := 20;
  FXSt:=2;
  FYSt:=3;


  InitColorMap;
end;

procedure TMainForm.InitColorMap;
var
  i : Integer;
begin
  // Initalisation de la palette de couleur, si utilisée
  for i:=1 to 255 do
    FPalette[i] := BZColor(16 + i shr 2,32 + i shr 1, 32+i);
end;

procedure TMainForm.RenderScene;
// Coeur du rendu de la scene
Var
  i, j,k : Integer;
begin
  FDisplayBuffer.Clear(clrBlack);
  J:=5;
  ColorDelta:=5;
  FStep:=1;
  while (j<(FDisplayBuffer.Height-5)) do
  begin
    FC := BZColor(ColorDelta,ColorDelta,ColorDelta);//FPalette[ColorDelta];
    i:=0;
    while i<360 do
    begin
      PutPolarPixel(FCtab[(FX+(FDisplayBuffer.Height-j)) mod 255],FStab[(FY+(FDisplayBuffer.Height-j)) mod 255],j,i,FC);
      inc(i,cAStep);
    end;
    inc(j,FStep);
    if (j mod 2)=0 then
    begin
       inc(FStep);
       inc(ColorDelta,10);
       //if ColorDelta>254 then ColorDelta:=8;
    end;
  end;
  FX:=FXst+FX mod 255;
  FY:=FYst+FY mod 255;
  k:=0;
 // repeat inc(k); until k>4000000;
end;

procedure TMainForm.DoneScene;
begin
  // Finalisation de la scene
end;

end.

