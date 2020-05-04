unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch, BZBitmapIO;

Const
  cFormCaption : String = 'BZScene Shade Blobs Demo';
  cMaxBlobs = 79;
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

    FBlobMaps : array[0..3] of TBZBitmap;
    FCx, FCy : Integer;

    procedure DrawBlobAt(px,py,i : Integer);

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

procedure TMainForm.DrawBlobAt(px, py, i : Integer);
var
  x,y : Integer;
  PixPtr, DstPtr : PBZColor;
  OutColor, InColor : TBZColor;
begin
  i := i and 3;
   If Not(FDisplayBuffer.CheckPixelBound(px, py)) Then exit;
  DstPtr := FDisplayBuffer.GetPixelPtr(pX, pY);
  PixPtr := FBlobMaps[i].GetScanLine(0);
  y := 0;
  x := 0;
  while (y<FBlobMaps[i].MaxHeight) do
  //for y := 0 to FBlobMaps[i].MaxHeight do
  begin
    //for x := 0 to FBlobMaps[i].MaxWidth do
    //begin
      OutColor := PixPtr^;
      if OutColor.Alpha>0 then
      begin
        InColor := DstPtr^;
        //DstPtr^ := DstPtr^ + OutColor;
        if (InColor<>clrBlack) then DstPtr^ := InColor + OutColor
        else DstPtr^ := OutColor;
      end;
      inc(DstPtr);
      inc(PixPtr);
      inc(x);
    //end;
    if (x>FBlobMaps[i].MaxWidth) then
    begin
      Inc(DstPtr,(FDisplayBuffer.Width- FBlobMaps[i].Width));
      inc(y);
      x := 0;
    end;
  end;
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
begin
  for i := 0 to 3 do FBlobMaps[i] := TBZBitmap.Create;
  FBlobMaps[0].LoadFromFile('..\..\..\..\media\images\blob.png');
  FBlobMaps[1].LoadFromFile('..\..\..\..\media\images\blob2.png');
  FBlobMaps[2].LoadFromFile('..\..\..\..\media\images\blob3.png');
  FBlobMaps[3].LoadFromFile('..\..\..\..\media\images\blob4.png');
  FCx := FDisplayBuffer.CenterX-FBlobMaps[0].CenterX;
  FCy := FDisplayBuffer.CenterY-FBlobMaps[0].CenterY;
  InitColorMap;
end;
procedure TMainForm.InitColorMap;
begin
  // Initalisation de la palette de couleur, si utilisée
end;

procedure TMainForm.RenderScene(NewTime : Double);
// Coeur du rendu de la scene
var
 i,cx,cy  : Integer;
 t1,t2 : Single;
begin
  //FDisplayBuffer.Clear(clrBlack);
  FDisplayBuffer.ColorFilter.AdjustBrightness(-0.20);
  t1 := cPi*NewTime;
  t2 := c2Pi*NewTime;
  For i:= 0 to cMaxBlobs do
  begin
    cx := FCx + Round(BZMath.cos((i*t1) * 0.00999) * 400);
    cy := FCy + Round(BZMath.sin((i*t2)* 0.00519) * 350);
    DrawBlobAt(cx,cy,i);
    //FDisplayBuffer.ClipRect.Create(cx,cy,cx+64,cy+64);
    //FDisplayBuffer.Clipping := True;
    //FDisplayBuffer.BlurFilter.FastBlur;
    //FDisplayBuffer.Clipping := False;
  end;

end;

procedure TMainForm.DoneScene;
var
 i : Integer;
begin
  // Finalisation de la scene
  for i := 3 downto 3 do FreeAndNil(FBlobMaps[i]);
end;

end.

