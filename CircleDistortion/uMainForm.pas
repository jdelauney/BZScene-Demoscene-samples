unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch, {%H-}BZBitmapIO,
  BZParallelThread;

Const
  cFormCaption : String = 'BZScene Circle Distortion Demo [Tap Keys: 0..7] - FrameRate';

type
  TPrecalcBuffer = record
    Tick, Pulse, Pulse2, CenX,CenY, Yy, YySquare : Single;
  end;
  PPrecalcBuffer = ^TPrecalcBuffer;
  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormKeyPress(Sender : TObject; var Key : char);
  private
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;
    FDisplayBuffer : TBZBitmap;
    FTextureMap : TBZBitmap;
    FFXMode : Byte;
    FPrecalcBuffer : PPrecalcBuffer;

    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);

    procedure RenderCircleDistortionLine(y : Integer;  Var Source : TPrecalcBuffer);
    procedure RenderCircleDistortionParalelleProc(Sender: TObject; Index: Integer; Data : Pointer);

  public
    procedure InitEngine;
    procedure StartEngine;
    procedure StopEngine;
    procedure DoneEngine;
    procedure InitScene;
    procedure InitColorMap;
    procedure RenderScene(NewTime,DeltaTime : Double);
    procedure DoneScene;
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

uses
  BZTypesHelpers,
  BZRandomGenerator;

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

procedure TMainForm.FormKeyPress(Sender : TObject; var Key : char);
begin
  if (key in ['0'..'6']) then FFXMode.Parse(Key)
  else FFXMode := 99;
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  RenderScene(NewTime, DeltaTime);
  FDisplayBuffer.DrawToCanvas(Canvas, ClientRect);
  Caption:=cFormCaption + ' : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
end;

procedure TMainForm.RenderCircleDistortionLine(y : Integer; var Source : TPrecalcBuffer);
Var
  DstPtr : PBZColor;
  X, U, V: integer;
  Dist, Dx, Dy, Diff, yy, yySquare : single;
  InColor, OutColor : TBZColor;
begin
  DstPtr := FDisplayBuffer.GetScanLine(y);
  yy := (FPreCalcBuffer^.CenY-Y);
  yySquare := yy * yy;
  for X := 0 to FTextureMap.MaxWidth do
  begin
    case FFXMode of
     0 : Dist := BZMath.sqrt(sqr(Source.CenX-X)+YySquare);
     1 : Dist := BZMath.sqrt(sqr(Source.CenX-X)+YySquare+(X*Y));
     2 : Dist := BZMath.sqrt(sqr(Source.CenX-X)+YySquare+((Source.Tick+X*Y)*Source.pulse));
     3 : Dist := (abs(Source.CenX-X)+abs(Yy));
     4 : Dist := Source.Cenx-x-Yy;
     5 : Dist := Sin(Source.Tick+(Yy))/Source.Pulse2;
     6 : Dist := Cos(Source.Tick+(Source.Cenx-x))*Source.Pulse;
    else
      Dist := GlobalRandomGen.Random(511)+1;
   end;

    Dx := BZMath.Sin(Dist*Source.Pulse2)*Source.Pulse;
    Dy := BZMath.Cos(Dist*Source.Pulse2)*Source.Pulse;

    U := round(X+Dx);
    U := Max(0, Min(u, FTextureMap.MaxWidth));
    V := round(Y+Dy);
    V := Max(0, Min(v, FTextureMap.MaxHeight));
    Diff := (Dx+Dy);

    InColor := FTextureMap.getPixel(u,v);
    OutColor.Red := BZMath.Min(255, BZMath.Max(Round(InColor.Red-Diff), 0));
    OutColor.Green := BZMath.Min(255, BZMath.Max(Round(InColor.Green-Diff), 0));
    OutColor.Blue := BZMath.Min(255, BZMath.Max(Round(InColor.Blue-Diff), 0));
    OutColor.Alpha := InColor.Alpha;

    DstPtr^:=OutColor;
    inc(DstPtr);
  end;
end;

procedure TMainForm.RenderCircleDistortionParalelleProc(Sender : TObject; Index : Integer; Data : Pointer);
begin
  RenderCircleDistortionLine(Index mod FDisplayBuffer.Height, TPrecalcBuffer(Data^));
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
begin
  FTextureMap := TBZBitmap.Create;
  FTextureMap.LoadFromFile('..\..\..\..\media\images\BZScene_Background_01.png');
  FFXMode := 0;
  GetMem(FPreCalcBuffer, Sizeof(TPreCalcBuffer));
  InitColorMap;
end;

procedure TMainForm.InitColorMap;
begin
  // Initalisation de la palette de couleur, si utilisée
end;

//procedure TMainForm.RenderScene(NewTime, DeltaTime : Double);
//// Coeur du rendu de la scene
//var
//  DstPtr : PBZColor;
//  X,Y, U, V: integer;
//  yy, Tick, Pulse, Pulse2, CenX,CenY, Dist, Dx, Dy, s,c, Diff : single;
//  InColor, OutColor : TBZColor;
//begin
//   Tick := NewTime*100;
//   s := BZMath.Sin(c2Pi*(tick+(DeltaTime*10))/512);
//   c := BZMath.Cos(c2Pi*Tick/512);
//   //s  := BZMath.Sin((Tick-DeltaTime*10)*20+(3/c2Pi));
//   //c := BZMath.Sin(NewTime*18);
//   Pulse  := s*10+10;
//   Pulse2 := 1/(s*5+10.1);
//
//   //CenX := FTextureMap.CenterX;// + (System.sin(NewTime*10)*FTextureMap.CenterX);
//   //CenY := FTextureMap.CenterY;// + (System.Cos(NewTime*13)*FTextureMap.CenterY);
//   CenX := FTextureMap.CenterX+ (s*FTextureMap.CenterX);
//   CenY := FTextureMap.CenterY+ (c*FTextureMap.CenterY);
//
//   DstPtr := FDisplayBuffer.GetScanLine(0);
//   for Y := 0 to FTextureMap.MaxHeight do
//   begin
//     yy := (CenY-Y);
//     yy := yy * yy;
//     for X := 0 to FTextureMap.MaxWidth do
//     begin
//       case FFXMode of
//         0 : Dist := BZMath.sqrt(sqr(CenX-X)+yy);
//         1 : Dist := BZMath.sqrt(sqr(CenX-X)+yy+(X*Y));
//         2 : Dist := (abs(CenX-X)+abs(CenY-Y));
//         3 : Dist :=Cenx-x-Ceny-y;
//         4 : Dist := Sin(Tick+(Ceny-y))/Pulse2;
//         5 : Dist := Cos(Tick+(Cenx-x))*Pulse;
//        else
//          Dist := Random(511)+1;
//       end;
//
//       //Dx := System.Sin(Tick+Dist*Pulse2)*Pulse;
//       //Dy := System.Cos(Tick+Dist*Pulse2)*Pulse;
//       Dx := BZMath.Sin(Dist*Pulse2)*Pulse;
//       Dy := BZMath.Cos(Dist*Pulse2)*Pulse;
//
//       U := round(X+Dx);
//       U := Max(0, Min(u, FTextureMap.MaxWidth));
//       V := round(Y+Dy);
//       V := Max(0, Min(v, FTextureMap.MaxHeight));
//       Diff := (Dx+Dy);
//
//       InColor := FTextureMap.getPixel(u,v);
//       OutColor.Red := BZMath.Min(255, BZMath.Max(Round(InColor.Red-Diff), 0));
//       OutColor.Green := BZMath.Min(255, BZMath.Max(Round(InColor.Green-Diff), 0));
//       OutColor.Blue := BZMath.Min(255, BZMath.Max(Round(InColor.Blue-Diff), 0));
//       //OutColor := InColor - Diff;
//       OutColor.Alpha := InColor.Alpha;
//
//       DstPtr^:=OutColor;
//       inc(DstPtr);
//     end;
//   end;
//end;

procedure TMainForm.RenderScene(NewTime, DeltaTime : Double);
// Coeur du rendu de la scene
var
  s,c: single;
begin
   FPreCalcBuffer^.Tick := NewTime*100;
   s := BZMath.Sin(c2Pi*(FPreCalcBuffer^.tick+(DeltaTime*10))/512);
   c := BZMath.Cos(c2Pi*FPreCalcBuffer^.Tick/512);

   FPreCalcBuffer^.Pulse  := s*10+10;
   FPreCalcBuffer^.Pulse2 := 1/(s*5+10.1);

   FPreCalcBuffer^.CenX := FTextureMap.CenterX+ (s*FTextureMap.CenterX);
   FPreCalcBuffer^.CenY := FTextureMap.CenterY+ (c*FTextureMap.CenterY);

   ParallelFor(0, FDisplayBuffer.Height, @RenderCircleDistortionParalelleProc, Pointer(FPreCalcBuffer));
end;

procedure TMainForm.DoneScene;
// Finalisation de la scene
begin
  FreeAndNil(FTextureMap);
  FreeMem(FPreCalcBuffer);
end;

end.

