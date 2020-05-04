unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormDestroy(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
  private
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;

    FDisplayBuffer,FPlasmaBuffer : TBZBitmap;
    FTableLUT1, FTableLUT2 : Array of Single;
    SinLUT, CosLUT : Array[0..1023] of integer;
    FVPal : Array[0..1023] of Word;

    FLG,FHT : Integer;
    FAnimAng1, FAnimAng2, FAnimAng3, FDelta : Integer;

    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    procedure InitSinCosLUT;
    procedure InitTableLUT1;
    procedure InitTableLUT2;

    procedure InitScene;

    procedure ChangePal;
    procedure RenderPlasma(x1,y1,x2,y2,x3,y3,x4,y4 : Integer);
    procedure RenderScene;

    procedure DoubleStrechWidth;
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

{ TMainForm }
uses
  BZLogger, BZMath;
//  BZMath;

procedure TMainForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  FCadencer.Enabled := False;
  FStopWatch.Stop;
  CanClose := True;
end;

procedure TMainForm.FormDestroy(Sender : TObject);
begin
  SetLength(FTableLUT1,0);
  FTableLUT1 := nil;
  SetLength(FTableLUT2,0);
  FTableLUT2 := nil;
  FreeAndNil(FStopWatch);
  FreeAndNil(FCadencer);
  FreeAndNil(FPlasmaBuffer);
  FreeAndNil(FDisplayBuffer);
end;

procedure TMainForm.FormCreate(Sender : TObject);
begin
  InitScene;
  DoubleBuffered:=true;
end;

procedure TMainForm.FormShow(Sender : TObject);
begin
  FStopWatch.Start;
  FCadencer.Enabled := True;
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  RenderScene;
  FDisplayBuffer.DrawToCanvas(Canvas,ClientRect);
  Caption:='BZScene - Circular Plasma Demo : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
end;

procedure TMainForm.InitSinCosLUT;
var
  i: Integer;
begin
  for i:=0 to 1023 do
  begin
    CosLUT[i] := Round(System.Cos((i*(360/1024))*0.017453293)* 1024);
    SinLUT[i] := Round(System.Sin((i*(360/1024))*0.017453293)* 1024);
  end;
end;

procedure TMainForm.InitTableLUT1;
var
  x, y : Integer;
begin
  SetLength(FTableLUT1, ((FHT * 2) * (FLG * 2)));
  For y := 0 to (FHT * 2)-1 do
  begin
    For x := 0 to (FLG*2) - 1 do
    begin
      FTableLUT1[x+y*(FLG * 2)] := (System.Sqrt(Sqr((FLG)-x)+Sqr((FHT) -y))*12);
    end;
  end;
end;

procedure TMainForm.InitTableLUT2;
var
  x, y : Integer;
begin
  SetLength(FTableLUT2,((FHT * 2) * (FLG * 2)));
  For y := 0 to (FHT * 2)-1 do
  begin
    For x := 0 to (FLG*2) - 1 do
    begin
      FTableLUT2[x+y*(FLG * 2)] := (1+System.Sin(System.Sqrt(Sqr((FLG)-x)+Sqr((FHT) -y))/12) * (FLG / 2));
    end;
  end;
end;

procedure TMainForm.InitScene;
Var
  i : Integer;
begin
  FCadencer := TBZCadencer.Create(self);
  FCadencer.Enabled := False;
  FCadencer.SleepLength := 15;   // Petit pause car l'effet estt très rapide à calculer
  FCadencer.OnProgress := @CadencerProgress;
  FStopWatch := TBZStopWatch.Create(self);

  FLG := Width div 2;
  FHT := Height;
  FPlasmaBuffer := TBZBitmap.Create(FLG,FHT);

  FPlasmaBuffer.UsePalette := True;
  FPlasmaBuffer.ColorManager.CreateColors(256);
  For i := 0 to 255 do
  begin
    FVPal[i] := Round(32+(System.Sin(i*(360/1024)*cPI/180)) *32* 1024);
  end;

  FDisplayBuffer := TBZBitmap.Create(Width, Height);
  InitSinCosLUT;
  InitTableLUT1;
  InitTableLUT2;
end;

procedure TMainForm.ChangePal;
Var
  i : Integer;
  r0,g0,b0,r,g,b : Word;
begin
  r0 := (256 *2) + 77;
  g0 := (256*10) + 128;
  b0 := (256*5) + 256;

  r := r0 shr 8;
  g := g0 shr 8;
  b := b0 shr 8;
  For i := 0 to 255 do
  begin
    FPlasmaBuffer.ColorManager.Palette.Colors[i].Red := (FVPal[(i+r) and 255]) shr 8;
    FPlasmaBuffer.ColorManager.Palette.Colors[i].Green := 64+g +(FVPal[(i+g) and 255]) shr 8;
    FPlasmaBuffer.ColorManager.Palette.Colors[i].Blue :=b + (FVPal[(i+b) and 255]) shr 8;
    //FPlasmaBuffer.ColorManager.Palette.Colors[i].Red :=32+(FVPal[(i+g) and 255]) shr 8;
    //FPlasmaBuffer.ColorManager.Palette.Colors[i].Green := 64+(FVPal[(i+b) and 255]) shr 8;
    //FPlasmaBuffer.ColorManager.Palette.Colors[i].Blue := 92+(FVPal[(i+r) and 255]) shr 8;
    FPlasmaBuffer.ColorManager.Palette.Colors[i].Alpha := 255;
  end;
end;

procedure TMainForm.RenderPlasma(x1, y1, x2, y2, x3, y3, x4, y4 : Integer);
var
  x,y, px1,px2,px3,px4, offs1, offs2, offs3, offs4, k : Integer;
begin
  K := 0;
  px1 := x1 + y1 * (FLG*2);
  px2 := x2 + y2 * (FLG*2);
  px3 := x3 + y3 * (FLG*2);
  px4 := x4 + y4 * (FLG*2);
  For y := 0 to FHT-1 do
  begin
    Offs1 := px1 + K;
    Offs2 := px2 + K;
    Offs3 := px3 + K;
    Offs4 := px4 + K;
    For x := 0 to (FLG)-1 do
    begin
      //FPlasmaBuffer.setPixel(x,y,FPlasmaBuffer.ColorManager.Palette.Colors[246].Value);
      FPlasmaBuffer.Colors[x,y] := Round(FTableLUT1[Offs1] + FTableLUT2[Offs2]  + FTableLUT2[Offs3] + FTableLUT2[Offs4]) and 255;
      Inc(Offs1);
      Inc(Offs2);
      Inc(Offs3);
      Inc(Offs4);
    end;
    inc(k,(FLG*2));
  end;
end;

procedure TMainForm.RenderScene;
var
  d,w,h : Integer;
  px1,py1,px2,py2,px3,py3,px4,py4 : Integer;
begin
  ChangePal;

  d := FDelta;
  FAnimAng1 := d and 1023;
  FAnimAng2 := (d + d) and 1023;
  FAnimAng3 := (FAnimAng2 + d) and 1023;

  w := (FLG div 2);
  h := (FHT div 2);
  px1 := (w*1024 + CosLUT[FAnimAng1] * w) div 1024;
  py1 := (h*1024 + SinLUT[FAnimAng2] * h) div 1024;
  px2 := (w*1024 + SinLUT[FAnimAng1] * w) div 1024;
  py2 := (h*1024 + SinLUT[FAnimAng3] * h) div 1024;
  px3 := (w*1024 + SinLUT[FAnimAng3] * w) div 1024;
  py3 := (h*1024 + CosLUT[FAnimAng1] * h) div 1024;
  px4 := (w*1024 + CosLUT[FAnimAng3] * w) div 1024;
  py4 := (h*1024 + CosLUT[FAnimAng2] * h) div 1024;

  RenderPlasma(px1,py1,px2,py2,px3,py3,px4,py4);
  inc(FDelta);
  DoubleStrechWidth;
end;

procedure TMainForm.DoubleStrechWidth;
var
  x, y : Integer;
  SrcPtr, DstPtr : PBZColor;
  Col : TBZColor;
begin
  For y := 0 to FPlasmaBuffer.MaxHeight do
  begin
    SrcPtr := FPlasmaBuffer.GetScanLine(y);
    DstPtr := FDisplayBuffer.GetScanLine(y);
    For x := 0 to FPlasmaBuffer.MaxWidth do
    begin
      Col := SrcPtr^;
      DstPtr^:= Col;
      Inc(DstPtr);
      DstPtr^:= Col;
      Inc(DstPtr);
      Inc(SrcPtr);
    end;
  end;
end;

end.

