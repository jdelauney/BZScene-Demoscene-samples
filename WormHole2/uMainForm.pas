unit uMainForm;

{$mode objfpc}{$H+}

// Original code by Bas van Gaalen

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Math,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch, BZBitmapIO;

Const
  cFormCaption : String = 'BZScene Worm Hole 2 Demo ';
  cDivs = 4800;
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
    FTextureMap : TBZBitmap;  //Max 16x16


    FWormBuffer : PByte;

    FSinLUT, FCosLUT, FWormLUT : Array[0..cDivs-1] of single;

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

uses BZLogger;
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
  i,j,x,y : Integer;
  z, cx,cy,xx,yy: Single;
  p : PByte;
begin

  FTextureMap := TBZBitmap.Create;
  FTextureMap.LoadFromFile('..\..\..\..\media\images\wormhole05.png');

  For i := 0 to cDivs-1 do
  begin
    FWormLUT[i] := c2PI*i / cDivs;
    FSinLUT[i] := System.Sin(FWormLUT[i]);
    FCosLUT[i] := System.Cos(FWormLUT[i]);
  end;

  FWormBuffer := nil;
  GetMem(FWormBuffer, FDisplayBuffer.Width * FDisplayBuffer.Height);

  for j:=1 to cDivs do
  begin
    z  := -1.0 + (System.ln(2.0 * j / cDivs));
		cx := (FDisplayBuffer.Width*j /cDivs);
    cy := (FDisplayBuffer.Height*j /cDivs);
    for i:=0 to cDivs-1 do
    begin
      xx := cx * FCosLUT[i];
      yy := cy * FSinLUT[i];
      yy := yy - (45 * z);
      x := Round(xx + FDisplayBuffer.Width div 2);
      y := Round(yy + ((FDisplayBuffer.Height div 2) div 2));
      if ((x >= 0) and (x < FDisplayBuffer.Width) And (y >= 0) And (y < FDisplayBuffer.Height)) then
      begin
        P := PByte(FWormBuffer + (y * FDisplayBuffer.Width) + x);
        P^ := ((Round(i / 10) Mod FTextureMap.Width) + (FTextureMap.Width * (Round(j / 9) mod FTextureMap.Height))) and 255;
      end;
    end;
  end;
  InitColorMap;
end;

procedure TMainForm.InitColorMap;
begin
  // Initalisation de la palette de couleur, si utilisée

end;

procedure TMainForm.RenderScene;
// Coeur du rendu de la scene
var
  y: Integer;
  OutColor : TBZColor;
  p : PByte;
  DstLine: PBZColor;

begin
  //FDisplayBuffer.Clear(clrBlack);
  y := 0;
  DstLine := FDisplayBuffer.GetScanLine(0);
  P := FWormBuffer;
  While (y<=FDisplayBuffer.MaxSize) do
  begin
    OutColor := FTextureMap.getPixelOffset(p^);
    DstLine^ := OutColor;
    inc(p);
    inc(DstLine);
    inc(y);
  end;

  FTextureMap.ShiftLeft;
  FTextureMap.ShiftUp;

 // x:=0; while x<4000000 do begin inc(x); end;
end;

procedure TMainForm.DoneScene;
begin
  // Finalisation de la scene
  FreeAndNil(FTextureMap);
  FreeMem(FWormBuffer);
  FWormBuffer := nil;
end;

end.

