unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch, {%H-}BZBitmapIO,
  BZParallelThread;

Const
  cFormCaption : String = 'BZScene 2D Bump Map Demo';

type
  TBumpLights = Array of Integer;
  //TBumpLights = record
  //  lpx1, lpy1 : Integer;
  //  lpx2, lpy2 : Integer;
  //  zoom : Integer;
  //end;

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

    //FlightMap : Array of byte;
    FlightMap, FColorMap, FBumpMap, FSpecularMap, FNormalMap : TBZBitmap;

    FBumpLights : TBumpLights;


    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    procedure InitEngine;
    procedure StartEngine;
    procedure StopEngine;
    procedure DoneEngine;
    procedure InitScene;
    procedure InitColorMap;

    procedure RenderBumpLine(yy : Integer; var lights : TBumpLights);
    procedure RenderBumpLineParalelleProc(Sender: TObject; Index: Integer; Data : Pointer);

    procedure RenderBump(Lpx1,Lpy1, Lpx2, Lpy2, Zoom : Integer);

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
  RenderScene(NewTime*100);
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
begin
  // Initalisation des données de la scene
  FColorMap := TBZBitmap.Create;
  FColorMap.LoadFromFile('..\..\..\..\media\images\bump_diffuseOriginal02.png');
  FBumpMap := TBZBitmap.Create;
  FBumpMap.LoadFromFile('..\..\..\..\media\images\bump_heightmap02.png');
  FSpecularMap := TBZBitmap.Create;
  FSpecularMap.LoadFromFile('..\..\..\..\media\images\bump_metallic.png');
  FNormalMap := TBZBitmap.Create;
  FNormalMap.LoadFromFile('..\..\..\..\media\images\bump_normmap01.png');
  FLightMap := TBZBitmap.Create;
  FLightMap.LoadFromFile('..\..\..\..\media\images\phongmap.png');
  InitColorMap;
  SetLength(FBumpLights,5);
end;

procedure TMainForm.InitColorMap;
begin
  // Initalisation de la palette de couleur, si utilisée
end;

procedure TMainForm.RenderBumpLine(yy : Integer; var lights : TBumpLights);
Var
  cx, cy, nx, ny, px, py, x, y, i, j : Integer;
  NormalColor, SpecularColor, TextureColor, LightColor, OutColor : TBZColor;
begin
  j := yy;
  for i := 1 to FDisplayBuffer.MaxWidth-1 do
  begin
    nx := FBumpMap.GetPixel(i + 1, j).Red - FBumpMap.GetPixel(i - 1, j).Red;
    ny := FBumpMap.GetPixel(i, j + 1).Red - FBumpMap.GetPixel(i, j - 1).Red;

    NormalColor := FNormalMap.GetPixel(i,j);
    nx := nx - NormalColor.Red;
    ny := ny - NormalColor.Green;

    cx := (i * Lights[4] shr 8);
    cy := (j * Lights[4] shr 8);

    // Lumiere 1
    px := cx - Lights[0];
    py := cy - Lights[1];
    x := (nx - px);
    y := (ny - py);

    LightColor := clrTransparent;
    SpecularColor := clrTransparent;

    //if ((y>=0) and (y<FLightMap.Height) and (x>=0) and (x<FLightMap.Width))  then
    if FLightMap.CheckPixelBound(x,y) then
    begin
      LightColor := FLightMap.GetPixel(x,y);
      SpecularColor := FSpecularMap.GetPixel(x,y);
    end;

    // Lumiere 2
    px := cx - Lights[2];
    py := cy - Lights[3];
    x := (nx - px);
    y := (ny - py);

    if FLightMap.CheckPixelBound(x,y) then
    begin
      LightColor := LightColor + FLightMap.GetPixel(x,y);
      SpecularColor := SpecularColor + FSpecularMap.GetPixel(x,y);
    end;

    LightColor := LightColor + SpecularColor;
    TextureColor  := FColorMap.GetPixel(i,j);
    OutColor := TextureColor + LightColor;

    FDisplayBuffer.SetPixel(i,j,OutColor);
  end;
end;

procedure TMainForm.RenderBumpLineParalelleProc(Sender : TObject; Index : Integer; Data : Pointer);
begin
  RenderBumpLine(Index mod FDisplayBuffer.Height, TBumpLights(Data));
end;

procedure TMainForm.RenderBump(Lpx1, Lpy1, Lpx2, Lpy2, Zoom : Integer);
var
  i,j, x, y, cx,cy, px, py, nx, ny : Integer;
  OutColor, LightColor, SpecularColor, TextureColor, NormalColor : TBZColor;
begin
//   Offs := FColorMpa.Width;
   For j := 1 to FDisplayBuffer.MaxHeight-1 do
   begin
     for i := 1 to FDisplayBuffer.MaxWidth-1 do
     begin
       nx := FBumpMap.GetPixel(i + 1, j).Red - FBumpMap.GetPixel(i - 1, j).Red;
       ny := FBumpMap.GetPixel(i, j + 1).Red - FBumpMap.GetPixel(i, j - 1).Red;

       NormalColor := FNormalMap.GetPixel(i,j);
       nx := nx - NormalColor.Red;
       ny := ny - NormalColor.Green;

       cx := (i * zoom shr 8);
       cy := (j * zoom shr 8);

       // Lumiere 1
       px := cx - lpx1;
       py := cy - lpy1;
       x := (nx - px);
       y := (ny - py);

       LightColor := clrTransparent;
       SpecularColor := clrTransparent;

       //if ((y>=0) and (y<FLightMap.Height) and (x>=0) and (x<FLightMap.Width))  then
       if FLightMap.CheckPixelBound(x,y) then
       begin
         LightColor := FLightMap.GetPixel(x,y);
         SpecularColor := FSpecularMap.GetPixel(x,y);
       end;

       // Lumiere 2
       px := cx - lpx2;
       py := cy - lpy2;
       x := (nx - px);
       y := (ny - py);

       if FLightMap.CheckPixelBound(x,y) then
       begin
         LightColor := LightColor + FLightMap.GetPixel(x,y);
         SpecularColor := SpecularColor + FSpecularMap.GetPixel(x,y);
       end;

       LightColor := LightColor + SpecularColor;
       TextureColor  := FColorMap.GetPixel(i,j);
       OutColor := TextureColor + LightColor;

       FDisplayBuffer.SetPixel(i,j,OutColor);
     end;
   end;
end;

procedure TMainForm.RenderScene(DeltaTime : Double);
// Coeur du rendu de la scene
//var
//  z, x1,y1,x2,y2 : Integer;
begin

  //x1 := FDisplayBuffer.CenterX + Round(128 * BZMath.Cos( DeltaTime/64 ) - 20);
  //y1 := FDisplayBuffer.CenterY + Round(128 * BZMath.Sin(-DeltaTime/45 ) + 20);
  //x2 := FDisplayBuffer.CenterX + Round(128 * BZMath.Cos(-DeltaTime/51 ) - 20);
  //y2 := FDisplayBuffer.CenterY + Round(128 * BZMath.Sin( DeltaTime/71 ) + 20);
  //z := 192 + Round(128 * BZMath.Sin(DeltaTime/112));
  //RenderBump(x1,y1,x2,y2,z);

  FBumpLights[0] := FDisplayBuffer.CenterX + Round(192 * BZMath.Cos( DeltaTime/64 ) - 40);
  FBumpLights[1] := FDisplayBuffer.CenterY + Round(192 * BZMath.Sin(-DeltaTime/45 ) + 40);
  FBumpLights[2] := FDisplayBuffer.CenterX + Round(128 * BZMath.Cos(-DeltaTime/51 ) - 40);
  FBumpLights[3] := FDisplayBuffer.CenterY + Round(128 * BZMath.Sin( DeltaTime/71 ) + 40);
  FBumpLights[4] := 240 + Round(128 * BZMath.Sin(DeltaTime/112));
  ParallelFor(1, FDisplayBuffer.Height, @RenderBumpLineParalelleProc, Pointer(FBumpLights));

end;

procedure TMainForm.DoneScene;
// Finalisation de la scene
begin
  SetLength(FBumpLights,0);
  FBumpLights := nil;
  FreeAndNil(FLightMap);
  FreeAndNil(FNormalMap);
  FreeAndNil(FSpecularMap);
  FreeAndNil(FBumpMap);
  FreeAndNil(FColorMap);
end;

end.

