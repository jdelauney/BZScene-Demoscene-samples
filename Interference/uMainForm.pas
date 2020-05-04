unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch;

Const
  cFormCaption : String = 'BZScene Demo';

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

    FCircleBufferA : TBZBitmap;
    FCircleBufferB : TBZBitmap;
    FCircleBufferC : TBZBitmap;



    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    procedure InitEngine;
    procedure StartEngine;
    procedure StopEngine;
    procedure DoneEngine;

    procedure GenerateCircleMap(Bmp : TBZBitmap; aWidth : Byte; aColor : TBZColor);

    procedure InitScene;
    procedure InitColorMap;
    procedure RenderScene(DeltaTime : Double);
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

procedure TMainForm.GenerateCircleMap(Bmp : TBZBitmap; aWidth : Byte; aColor : TBZColor);
Var
  x,y : Integer;
  Radius : Single;
  modulo : Integer;
begin
  for y :=-Bmp.CenterY to Bmp.CenterY do
  begin
    for x := -Bmp.CenterX to Bmp.CenterX do
    begin
       Radius := System.Sqrt(x*x+y*y);
       Modulo := Round(Abs(Radius)) mod aWidth;
       if Modulo > (aWidth div 2) then
         Bmp.SetPixel(Bmp.CenterX + X, Bmp.CenterY + Y, aColor)
    end;
  end;
  Bmp.BlurFilter.GaussianSplitBlur(2);
end;

procedure TMainForm.InitScene;
// Initalisation des données de la scene
begin
  FCircleBufferA := TBZBitmap.Create(Width * 2, Height * 2);
  FCircleBufferA.Clear(BZColor(0, 0, 128));
  GenerateCircleMap(FCircleBufferA, 92, BZColor(255, 0, 128));

  FCircleBufferB := TBZBitmap.Create(Width * 2, Height * 2);
  //FCircleBufferB.Clear(clrTransparent);
  FCircleBufferB.Clear(BZColor(0, 0, 128));
  GenerateCircleMap(FCircleBufferB, 64, BZColor(255, 0, 128));    //BZColor(255, 255, 128));

  FCircleBufferC := TBZBitmap.Create(Width * 2, Height * 2);
  FCircleBufferC.Clear(clrTransparent);
  GenerateCircleMap(FCircleBufferC, 48,  BZColor(255, 255, 128));   //BZColor(255, 0, 128)); //BZColor(128, 255, 128));
  InitColorMap;
end;

procedure TMainForm.InitColorMap;
begin
  // Initalisation de la palette de couleur, si utilisée
end;

procedure TMainForm.RenderScene(DeltaTime : Double);
// Coeur du rendu de la scene
var
  x1,y1,x2,y2,x3,y3,cx,cy : Integer;
  v: Single;
begin
 // FDisplayBuffer.Clear(clrBlack);

  cx := FDisplayBuffer.CenterX;
  cy := FDisplayBuffer.CenterY;

	x1 := cX + Round(cX * BZMath.Cos(DeltaTime));
	y1 := cY + Round(cY * BZMath.Sin(DeltaTime * 1.3));

  v:= DeltaTime + 1.33;
  x2 := cX + Round(cX * BZMath.Cos(-v));
  y2 := cY + Round(cY * BZMath.Sin(v * 2.3));

  //v := DeltaTime + 1.13;
  //x3 := cX + Round(cX * BZMath.Cos(v));
  //y3 := cY + Round(cY * BZMath.Sin(-v * 1.3));

  FDisplayBuffer.PutImage(FCircleBufferB,x1,y1,FDisplayBuffer.Width, FDisplayBuffer.Height,0,0,dmSet,amNone);
  FDisplayBuffer.PutImage(FCircleBufferC,x2,y2,FDisplayBuffer.Width, FDisplayBuffer.Height,0,0,dmCombine,amNone,255,cmXOR);
 // FDisplayBuffer.PutImage(FCircleBufferC,x1,y2,FDisplayBuffer.Width, FDisplayBuffer.Height,0,0,dmCombine,amNone,255,cmXOR);
end;

procedure TMainForm.DoneScene;
begin
  // Finalisation de la scene
end;

end.

