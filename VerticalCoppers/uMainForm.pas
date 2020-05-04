unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch;

Const
  cFormCaption : String = 'BZScene Vertical Coppers Demo';
  cCopperWidthA : Byte = 35;
  cCopperWidthB : Byte = 27;
  cCopperWidthC : Byte = 17;

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

    FSinLUT1, FSinLUT2, FSinLUT3 : Array[0..255] of Integer;

    FCopperPalA  : TBZPaletteEntries;
    FCopperPalB  : TBZPaletteEntries;
    FCopperPalC  : TBZPaletteEntries;

    FAddY : Integer;
    FAdd1, FAdd2, FAdd3 : Word;

    procedure RenderCopper(x, y, w : Integer; Const ColorPal : TBZPaletteEntries);

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

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  RenderScene;
  FDisplayBuffer.DrawToCanvas(Canvas, ClientRect);
  Caption:=cFormCaption + ' : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
end;

procedure TMainForm.RenderCopper(x, y, w : Integer; const ColorPal : TBZPaletteEntries);
Var
  dw,px, py, sx, MaxW : Integer;
  OutColor : TBZColor;
  PixPtr : PBZColor;
  IncW : Integer;
  Idx : Integer;
begin
  dw := W;
  MaxW := x + dw;
  sx := x;
  IncW := SX + (FDisplayBuffer.Width - (SX + DW));
  PixPtr := FDisplayBuffer.GetScanLine(y);
  inc(PixPtr,sx);
  For py := y to FDisplayBuffer.MaxHeight do
  begin
    Idx := 0;
    For px := sx to MaxW do
    begin
      if (px>=0) and (px< FDisplayBuffer.Width) then
      begin
        OutColor :=ColorPal[Idx];
        PixPtr^:= OutColor;
        Inc(Idx);
        if (px < MaxW) then Inc(PixPtr);
      end;
    end;
    Inc(PixPtr, IncW);
  end;
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
  I : Integer;
begin
  for I := 0 to 255 do begin
   FSinLUT1[I] := round(1.7* (sin(I*(360/256) * cPiDiv180) )*92);
   FSinLUT2[I] := round(1.9* (Cos(I*(360/256) * cPiDiv180) )*16);
   FSinLUT3[I] := round(1.2 * (Sin(I*(360/128) * cPiDiv180) )*32);
  end;

  FAddY := 8;
  FAdd1 := 0;
  FAdd2 := 0;
   FAdd3 := 0;

  InitColorMap;
end;

procedure TMainForm.InitColorMap;
// Initalisation de la palette de couleur, si utilisée
Var
  Delta, Step : Single;
  i : Integer;
begin
  Delta := 0;
  Step := 1.0 / (cCopperWidthA *0.5);
  For i:=0 to (cCopperWidthA div 2)-1 do
  begin
    FCopperPalA[I] := clrBlue.Lerp(clrWhite, Delta,0, itLinear,False);
    Delta := Delta + Step;
  end;
  Delta := 0;
  For i:= (cCopperWidthA div 2) to cCopperWidthA-1 do
  begin
    FCopperPalA[I] := clrWhite.Lerp(clrBlue, Delta,0, itLinear,False);
    Delta := Delta + Step;
  end;

  Delta := 0;
  Step := 1.0 / (cCopperWidthB*0.5);
  For i:=0 to (cCopperWidthB div 2)-1 do
  begin
    FCopperPalB[I] := clrRed.Lerp(clrWhite, Delta,0, itLinear,False);
    Delta := Delta + Step;
  end;
  Delta := 0;
  For i:= (cCopperWidthB div 2) to cCopperWidthB-1 do
  begin
    FCopperPalB[I] := clrWhite.Lerp(clrRed, Delta,0, itLinear,False);
    Delta := Delta + Step;
  end;

  Delta := 0;
  Step := 1.0 / (cCopperWidthC*0.5);
  For i:=0 to (cCopperWidthC div 2)-1 do
  begin
    FCopperPalC[I] := BZColor(85,57,22).Lerp(clrWhite, Delta,0, itSin,False);
    Delta := Delta + Step;
  end;
  Delta := 0;
  For i:= (cCopperWidthC div 2) to cCopperWidthC-1 do
  begin
    FCopperPalC[I] := clrWhite.Lerp(BZColor(85,57,22), Delta,0, itSinAlt,False);
    Delta := Delta + Step;
  end;
end;

procedure TMainForm.RenderScene;
// Coeur du rendu de la scene
Var
  X,Y : Integer;
begin
  FDisplayBuffer.Clear(clrBlack);
  Y:=0;
  While (Y<FDisplayBuffer.Height) do
  begin
    X:=(FDisplayBuffer.CenterX-20)+FSinLUT2[ (FAdd1+Y) and $ff ] + FSinLUT1[ (FAdd1+Y+32) and $ff];
    RenderCopper(X ,Y,cCopperWidthA,FCopperPalA);
    X:=(FDisplayBuffer.CenterX - 40) +FSinLUT3[ (FAdd2+Y) and $ff ] + FSinLUT1[ (FAdd2+Y+16) and $ff];
    RenderCopper(X,Y,cCopperWidthB,FCopperPalB);

    X:=(FDisplayBuffer.CenterX - 10) +FSinLUT3[ (FAdd3+Y+8) and $ff ] + FSinLUT2[ (FAdd3+Y+16) and $ff];
    RenderCopper(X,Y,cCopperWidthC,FCopperPalC);

    Inc(Y, FAddY);
  end;
  FAdd1 := FAdd1+2;
  FAdd2 := FAdd2+3;
  FAdd3 := FAdd2+5;
end;

procedure TMainForm.DoneScene;
// Finalisation de la scene
begin
  // Ne fait rien
end;

end.

