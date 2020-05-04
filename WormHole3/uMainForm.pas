unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch;


{
Tips 3D to 2D

  //This would crash if FovAngle = +-180 degrees (since Tan is undefined there)
 if FovAngle < 180 then
 FovAspect := Tan(Pi/180*(FovAngle)/2);

 if ImageHeight < ImageWidth then
    AspectRatio := (ImageHeight/ImageWidth)*2
 else
    AspectRatio := (ImageWidth/ImageHeight)*2;

 For each 3D point:
     //For speed, 1 divide and 2 mults are faster than 2 divides.
     relZ := 1/(Z-CameraZ)*AspectRatio*FovAspect*ScalingFactor;
     ScreenX := (X-CameraX)*relZ+(HalfImageWidth);
     ScreenY := (Y-CameraY)*relZ+(HalfImageHeight);
}
Const
  cFormCaption : String = 'BZScene Demo';
  cMinRadius = 120;
  cMaxRadius = 512;
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

    FSinLUT : Array[0..255] of integer;
    FCosLUT : Array[0..255] of integer;
    FRadiusDiff : Integer;
    FStartX, FStartY : Integer;
    FFixedCenterX, FFixedCenterY : Integer;
    FStep : Integer;
    FDelta : Single;

    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    procedure InitEngine;
    procedure StartEngine;
    procedure StopEngine;
    procedure DoneEngine;
    procedure InitScene;
    procedure InitColorMap;
    procedure RenderScene(NewTime : Double);
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
  RenderScene(NewTime);
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
var
  i : Integer;
  d : single;
begin
  d := 360 / 255;
  //for i :=0 to 255 do FSinLUT[i] := round(System.Sin(c2PI*i/255)*256);
  //for i :=0 to 255 do FCosLUT[i] := round(System.Cos(c2PI*i/255)*256);
  for i :=0 to 255 do FSinLUT[i] := Round(System.Sin((i*d)*0.017453293)*255);
  for i :=0 to 255 do FCosLUT[i] := Round(System.Cos((i*d)*0.017453293)*255);

  FRadiusDiff := cMaxRadius - cMinRadius;
  FStartX := FDisplayBuffer.CenterX - cMinRadius;
  FStartY := FDisplayBuffer.CenterY - cMinRadius;

  FFixedCenterX := FDisplayBuffer.CenterX * 255;
  FFixedCenterY := FDisplayBuffer.CenterY * 255;

  FStep := 0;
  FDelta := FDisplayBuffer.Width / FDisplayBuffer.Height;
  InitColorMap;
end;

procedure TMainForm.InitColorMap;
begin
  // Initalisation de la palette de couleur, si utilisée
end;

procedure TMainForm.RenderScene(NewTime : Double);
// Coeur du rendu de la scene
var
  i,j, r : Integer;
  cx,cy, x,y: Integer;
  mx,my : Single;
  ink : Byte;
begin
  FDisplayBuffer.Clear(clrBlack);
  ink := 0;
  i := cMinRadius;
  While (i < cMaxRadius) do
  begin
    r := (i-(cMinRadius));
    mx := FStartX - (FStartX*(r / FRadiusDiff));
    my := FStartY - (FStartY*(r / FRadiusDiff));
    cx :=  FFixedCenterX + Round(FCosLUT[(FStep*3+i ) and 255] * mx);
    cy :=  FFixedCenterY + Round(FSinLUT[((FStep shl 1) + (i shl 1))   and 255] * my);
    inc(Ink);

    j:=0;
    While (j<256) do
    begin
      x := (cx + Round(FCosLUT[j] * FDelta) * i) shr 8;
      y := (cy + FSinLUT[j]* i) shr 8;

      if FDisplayBuffer.CheckPixelBound(x,y) then FDisplayBuffer.SetPixel(x,y, BZColor(Ink,Ink,Ink));
      inc(j,4);
    end;
    inc(i,2);
  end;
  Inc(FStep);
  sleep(10);
end;

procedure TMainForm.DoneScene;
begin
  // Finalisation de la scene
end;

end.

