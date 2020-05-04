unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch, BZBitmapIO;

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
    FTextureMap : TBZBitmap;
    FMxLUT,FMyLUT:   array[Word]of Integer;

    FMiddleX, FMiddleY : Integer; // Fixed Point
    FBlur : Boolean;
    FDeltaStretch : Single;

    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    procedure InitEngine;
    procedure StartEngine;
    procedure StopEngine;
    procedure DoneEngine;
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
  RenderScene(NewTime*5);
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
  x,y,i : Integer;
begin

  FTextureMap := TBZBitmap.Create;
  FTextureMap.LoadFromFile('..\..\..\..\media\images\clonemeb.png');

  FMiddleX := ((FTextureMap.Width shl 16) - (FDisplayBuffer.Width shl 16)) div 2;
  FMiddleY := ((FTextureMap.Height shl 16) - (FDisplayBuffer.Height shl 16)) div 2;
  FBlur := false;

  x:=0; y:=0;
  for i:=0 to 65535 do
  begin
    FMxLUT[i]:=x;
    FMyLUT[i]:=y;
    Inc(x);
    if (x = FTextureMap.MaxWidth) then x := 0;
    Inc(y);
    if (y = FTextureMap.MaxHeight) then y := 0;
  end;
  FDeltaStretch := 0.01;

  InitColorMap;
end;

procedure TMainForm.InitColorMap;
begin
  // Initalisation de la palette de couleur, si utilisée
end;

procedure TMainForm.RenderScene(DeltaTime : Double);
// Coeur du rendu de la scene
var
  x,y,dx,dy,cx,cy,
  sdx,sdy,isin,icos,ax,ay, r,g,b: Integer;
  c00,c01,c10,c11: TBZColor;
  DestPtr,SrcPtr: PBZColor;
begin

    cx := FDisplayBuffer.CenterX;
    cy := FDisplayBuffer.CenterY;

    isin:=Round((Sin(Cos(150+DeltaTime/360) * 15) * 65536) * Sin(150+DeltaTime/360)); // Fixed point
    icos:=Round((Cos(Sin(150+DeltaTime/180) * 35) * 65536) / Cos(150+DeltaTime/90));

    ax := ((cx shl 16) - (icos * cx));
    ay := ((cy shl 16) - (isin * cx)) ;

    DestPtr := FDisplayBuffer.GetScanLine(0);
    for y:=0 to FDisplayBuffer.MaxHeight do
    begin
      dy := (cy-y);
      //sdx := dy; //(ax + (isin * dy)) + FMiddleX;
      sdx := ax + (isin * dy) + FMiddleX;
      sdy := ay - (iCos * dy) + FMiddleY; //iCos* dy; //(ay - (icos * dy)) + FMiddleY;

      //sdx := (ax + (isin * Round(dy * (DeltaTime*0.01)))*4) + FMiddleX;
      //sdy := (ay - (icos * Round(dy * (DeltaTime*0.01)))*4) + FMiddleY;
      for x:=0 to FDisplayBuffer.MaxWidth do
      begin
        //dx := (cx -  x);
        //sdx := (ax + (isin * dx)) + FMiddleX;
        //sdx := ax + dx - FMiddleX;
        dx := Smallint(sdx shr 16);
        dy := Smallint(sdy shr 16);
        if (dx < 0) then dx := FTextureMap.Width-FMxLUT[-dx]-2 else if (dx>FTextureMap.Width-2) then dx := FMxLUT[dx];
        if (dy < 0) then dy := FTextureMap.Height-FMyLUT[-dy]-2 else if (dy>FTextureMap.Height-2) then dy := FMyLUT[dy];
        if FBlur then
        begin
          SrcPtr := FTextureMap.GetPixelPtr(dx,dy);
          c00 := SrcPtr^;
          Inc(SrcPtr);
          c01 := SrcPtr^;
          Inc(SrcPtr,FTextureMap.MaxWidth);
          c10 := SrcPtr^;
          Inc(SrcPtr);
          c11 := SrcPtr^;

          r := (c00.Red + c01.Red + c10.Red + c11.Red) shr 2;
          g := (c00.Green + c01.Green + c10.Green + c11.Green) shr 2;
          b := (c00.Blue + c01.Blue + c10.Blue + c11.Blue) shr 2;

          DestPtr^ := BZColor(r,g,b);
        end
        else DestPtr^ := FTextureMap.GetPixel(dx,dy);

        Inc(sdx,icos);
        Inc(sdy,isin);
        Inc(DestPtr);
      end;

    end;
end;

procedure TMainForm.DoneScene;
begin
  // Finalisation de la scene
  FreeAndNil(FTextureMap);
end;

end.

