unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch, {%H-}BZBitmapIO,
  BZParallelThread;

Const
  cFormCaption : String = 'BZScene Drop Water Reflection Demo';
  cWaterDensity : Single   = 4.0;   // Densité
  cDamp : Single           = 1/4;   // Reciprocal de la densité
  cWaterReflection: Single = 18.0;	// Indice de Reflection
  cWaterReflectionIntensity	: Byte	   = 92; // Profondeur
  cWaterDepth	: Single	   = 150.0; // Profondeur
type

  { TMainForm }
  TWaterBuffer =  Array of Single;
  TMainForm = class(TForm)
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
  private
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;
    FDisplayBuffer : TBZBitmap;

    FBackgroundImage : TBZBitmap;

    FWaterBufferA : TWaterBuffer;
    FWaterBufferB : TWaterBuffer;

    FWaterBufferSize , FWaterBufferWidth,  FWaterBufferHeight : Integer;
    FWaterBufferStartOffset, FWaterBufferEndOffset, FWaterBufferOffsetWidth, FWaterBufferOffsetHeight : Integer;
    FCounterDrop : Byte;
    FSwapBuffer : Boolean;
    FScanLineOffset : TBZScanLIneLUT;

    procedure WaveDrop(x,y : Integer; var Source : TWaterBuffer);
    procedure ComputeWater(Var Source, Dest : TWaterBuffer);
    procedure BlurWater(Var Source : TWaterBuffer);
    procedure RenderWater(Source : TWaterBuffer);
    procedure RenderWaterLine(y : Integer;  Var Source : TWaterBuffer);
    procedure RenderWaterLineParalelleProc(Sender: TObject; Index: Integer; Data : Pointer);

    procedure CadencerProgress(Sender : TObject; const {%H-}deltaTime, {%H-}newTime : Double);
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

procedure TMainForm.WaveDrop(x, y : Integer; var Source : TWaterBuffer);
Var
  v : Single;
  Offs : DWord;
begin
  if ((x > 0) and (x < FDisplayBuffer.MaxWidth)) and (( y > 1) and (y < FDisplayBuffer.MaxHeight)) then
  begin
    Offs := FScanLineOffset[y] + (x-1); //(y * FWaterBufferWidth + x);
    v := Source[Offs];
    Source[Offs] := v - cWaterDepth;
  end;
end;

procedure TMainForm.ComputeWater(var Source, Dest : TWaterBuffer);
Var
  Offs : DWord;
  w : Single;
begin


  Offs := FWaterBufferStartOffset;
  While (Offs < FWaterBufferEndOffset) do
  begin
     w :=  (((Source[Offs - 1] + Source[Offs + 1] +
              Source[Offs - FWaterBufferWidth] + Source[Offs + FWaterBufferWidth])  * 0.5 -
              Dest[Offs]) * cDamp);
    Dest[Offs] := w * cWaterDensity;
    Inc(Offs);
  end;
end;

procedure TMainForm.BlurWater(var Source : TWaterBuffer);
Var
  Offs : DWord;
begin
  Offs := FWaterBufferStartOffset;
  While (Offs < FWaterBufferEndOffset) do
  begin
    Source[Offs] := (Source[Offs] + Source[Offs - 1] + Source[Offs + 1] +
                     Source[Offs - FWaterBufferWidth] + Source[Offs + FWaterBufferWidth])  * 0.20;
    Inc(Offs);
  end;
end;

procedure TMainForm.RenderWater(Source : TWaterBuffer);
Var
  x, y,  rx, ry, s : Integer;
  Offs : DWord;
  InColor, OutColor : TBZColor;
  PixPtr : PBZColor;
  nx, ny : Single;
  //nx, ny, w : Integer;
begin

  // 1ere Passe : Physique de l'eau
  //For y := 1 to FDisplayBuffer.MaxHeight-1 do
  //begin
  //  Offs := y * FWaterBufferWidth; //FScanLineOffset[y];
  //  For x:= 1 to FDisplayBuffer.MaxWidth-1 do
  //  begin
  //   FWaterBuffer[Offs+ FWaterBufferSize] := ((FWaterBuffer[Offs - 1] + FWaterBuffer[Offs + 1] +
  //                                             FWaterBuffer[Offs - FWaterBufferWidth] + FWaterBuffer[Offs + FWaterBufferWidth]) * 0.5 -
  //                                             FWaterBuffer[Offs + FWaterBufferSize]) * cDamp;
  //   Inc(Offs);
  //  end;
  //end;

  // Version Optimisée :
  //Offs := FWaterBufferWidth + 1;
  //While (Offs < (FWaterBufferSize-FWaterBufferWidth-2)) do
  //begin
  //   w :=  (((FWaterBuffer[Offs - 1] + FWaterBuffer[Offs + 1] +
  //                                            FWaterBuffer[Offs - FWaterBufferWidth] + FWaterBuffer[Offs + FWaterBufferWidth])  * 0.5 -
  //                                            FWaterBuffer[Offs + FWaterBufferSize]) * cDamp);
  //  FWaterBuffer[Offs + FWaterBufferSize] := w * cWaterDensity;
  //  Inc(Offs);
  //end;


  // 2eme passe : Réfraction et rendu
  //For y := 1 to FDisplayBuffer.MaxHeight-1 do
  //begin
  //  PixPtr := FDisplayBuffer.GetScanLine(y);
  //  Offs := y * FWaterBufferWidth + 1; //yi := FScanLineOffset[y];
  //  For x:= 1 to FDisplayBuffer.MaxWidth-1 do
  //  begin
  //    nx := FWaterBuffer[Offs + 1 + FWaterBufferSize] - FWaterBuffer[Offs - 1 + FWaterBufferSize];
	 // 	ny := FWaterBuffer[Offs + FWaterBufferWidth + FWaterBufferSize-1]  - FWaterBuffer[Offs - FWaterBufferWidth + FWaterBufferSize-1];
  //
  //    rx := x - Round(nx * cWaterReflection);
  //    ry := y - Round(ny * cWaterReflection);
  //    if (rx < 1) then rx := 1;
  //    if (ry < 1) then ry := 1;
  //    if (rx > FWaterBufferWidth - 2) then rx := FWaterBufferWidth - 2;
  //    if (ry > FWaterBufferHeight - 2) then ry := FWaterBufferHeight - 2;
  // 		s := Clamp(Trunc(ny * 128.0),0,128);
  //    InColor := FBackgroundImage.getPixel(rx,ry);
  //    OutColor := InColor + s;
  //    //OutColor.Red := ClampByte(InColor.Red + s);
  //    //OutColor.Green := ClampByte(InColor.Green + s);
  //    //OutColor.Blue := ClampByte(InColor.Blue + s);
  //    PixPtr^:= OutColor;
  //    Inc(PixPtr);
  //    Inc(Offs);
  //  end;
  //end;

  // Version Optimisée :
  //Offs := FWaterBufferWidth + 1;
  //x:= 1;
  //y := 1;
  //PixPtr := FDisplayBuffer.GetScanLine(y);
  //Inc(PixPtr);
  //While (Offs < (FWaterBufferSize - FWaterBufferWidth - 2)) do
  //begin
  //  Delta := Offs + FWaterBufferSize;
  //  nx := FWaterBuffer[Delta + 1] - FWaterBuffer[Delta - 1];
  //	ny := FWaterBuffer[Delta + FWaterBufferWidth]  - FWaterBuffer[Delta - FWaterBufferWidth];
  //
  //  rx := x - Trunc(nx * cWaterReflection);
  //  ry := y - Trunc(ny * cWaterReflection);
  //  if (rx < 1) then rx := 1;
  //  if (ry < 1) then ry := 1;
  //  if (rx > FWaterBufferWidth - 2) then rx := FWaterBufferWidth - 2;
  //  if (ry > FWaterBufferHeight - 2) then ry := FWaterBufferHeight - 2;
 	//	s := Clamp(Trunc(ny * 128.0),0,128);
  //  InColor := FBackgroundImage.getPixel(rx,ry);
  //  OutColor := InColor + s;
  //  //OutColor.Red := ClampByte(InColor.Red + s);
  //  //OutColor.Green := ClampByte(InColor.Green + s);
  //  //OutColor.Blue := ClampByte(InColor.Blue + s);
  //  PixPtr^:= OutColor;
  //  Inc(PixPtr);
  //  Inc(Offs);
  //  Inc(x);
  //  if (x > (FDisplayBuffer.MaxWidth-1)) then
  //  begin
  //    x := 1;
  //    Inc(y);
  //    Inc(PixPtr,2);
  //    Inc(Offs,2);
  //  end;
  //end;


  Offs := FWaterBufferStartOffset;
  x:= 1;
  y := 1;
  PixPtr := FDisplayBuffer.GetScanLine(y);
  Inc(PixPtr);
  While (Offs < FWaterBufferEndOffset) do
  begin
    nx := Source[Offs + 1] - Source[Offs - 1];
  	ny := Source[Offs + FWaterBufferWidth]  - Source[Offs - FWaterBufferWidth];
    //rx := x + Trunc(nx );
    //ry := y + Trunc(ny );
    rx := Clamp(x - Trunc(nx * cWaterReflection),1, FWaterBufferOffsetWidth);
    ry := Clamp(y - Trunc(ny * cWaterReflection),1, FWaterBufferOffsetHeight);

    //if (rx < 1) then rx := 1;
    //if (ry < 1) then ry := 1;
    //if (rx > FWaterBufferWidth - 2) then rx := FWaterBufferWidth - 2;
    //if (ry > FWaterBufferHeight - 2) then ry := FWaterBufferHeight - 2;



    s := Clamp(Trunc(ny * cWaterReflectionIntensity),0,cWaterReflectionIntensity);
    InColor := FBackgroundImage.getPixel(rx,ry);
    OutColor := InColor + s;
    PixPtr^:= OutColor;

    Inc(PixPtr);
    Inc(Offs);
    Inc(x);
    if (x > (FDisplayBuffer.MaxWidth-1)) then
    begin
      x := 1;
      Inc(y);
      Inc(PixPtr,2);
      Inc(Offs,2);
    end;
  end;


  // 3eme passe : Copie du tampon
  //For y := 1 to FDisplayBuffer.MaxHeight-1 do
  //begin
  //  Offs := y * FWaterBufferWidth + 1; //yi := FScanLineOffset[y];
  //  For x:= 1 to FDisplayBuffer.MaxWidth-1 do
  //  begin
  //    w := FWaterBuffer[Offs + FWaterBufferSize];
  //    FWaterBuffer[Offs + FWaterBufferSize] := FWaterBuffer[Offs];
  //    FWaterBuffer[Offs] := w;
  //
  //    Inc(Offs);
  //  end;
  //end;

  // Version Optimisée :
  //Offs := 1;
  //While (Offs < (FWaterBufferSize-FWaterBufferWidth-1)) do
  //begin
  //  w := FWaterBuffer[Offs + FWaterBufferSize];
  //  FWaterBuffer[Offs + FWaterBufferSize] := FWaterBuffer[Offs];
  //  FWaterBuffer[Offs] := w;
  //  Inc(Offs);
  //end;

  // 4eme passe : Blur (Optionnel)
  //For y := 1 to FDisplayBuffer.MaxHeight-1 do
  //begin
  //  Offs := y * FWaterBufferWidth + 1; //yi := y * FScanLineOffset[y];
  //  For x:= 1 to FDisplayBuffer.MaxWidth-1 do
  //  begin
  //    FWaterBuffer[Offs] := (FWaterBuffer[Offs] + FWaterBuffer[Offs - 1] +
  //                          FWaterBuffer[Offs + 1] + FWaterBuffer[Offs - FWaterBufferWidth] +
  //                          FWaterBuffer[Offs + FWaterBufferWidth]) * 0.20;
  //    Inc(Offs);
  //  end;
  //end;

  // Version Optimisée :
  //Offs := FWaterBufferWidth + 1;
  //While (Offs < (FWaterBufferSize-FWaterBufferWidth)) do
  //begin
  //  FWaterBuffer[Offs] := (FWaterBuffer[Offs] + FWaterBuffer[Offs - 1] +
  //                         FWaterBuffer[Offs + 1] + FWaterBuffer[Offs - FWaterBufferWidth] +
  //                         FWaterBuffer[Offs + FWaterBufferWidth])  * 0.20;
  //  Inc(Offs);
  //end;

end;

procedure TMainForm.RenderWaterLine(y : Integer; var Source : TWaterBuffer);
Var
  x, rx, ry, s : Integer;
  Offs : DWord;
  InColor, OutColor : TBZColor;
  PixPtr : PBZColor;
  nx, ny : Single;
begin
  x:=1;
  PixPtr := FDisplayBuffer.GetScanLine(y+1);
  Offs := FScanLineOffset[y]; //y * FWaterBufferWidth + 1;
  Inc(PixPtr);
  While (x < FWaterBufferOffsetWidth) do
  begin
    nx := Source[Offs + 1] - Source[Offs - 1];
    ny := Source[Offs + FWaterBufferWidth]  - Source[Offs - FWaterBufferWidth];
    rx := Clamp(x + Trunc(nx * cWaterReflection),1, FWaterBufferOffsetWidth);
    ry := Clamp(y + Trunc(ny * cWaterReflection),1, FWaterBufferOffsetHeight);
    s := Clamp(Trunc(ny * cWaterReflectionIntensity),0,cWaterReflectionIntensity);
    InColor := FBackgroundImage.getPixel(rx,ry);
    OutColor := InColor + s;
    PixPtr^:= OutColor;
    Inc(PixPtr);
    Inc(Offs);
    Inc(x);
  end;
end;

procedure TMainForm.RenderWaterLineParalelleProc(Sender : TObject; Index : Integer; Data : Pointer);
begin
  RenderWaterLine(Index mod FWaterBufferOffsetHeight, TWaterBuffer(Data));
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
// Initalisation des données de la scene;
Var
  i : Integer;
begin
  FBackGroundImage := TBZBitmap.Create;
  FBackGroundImage.LoadFromFile('../../../../media/images/blue-water.jpg');
  FWaterBufferSize := (FDisplayBuffer.Width * FDisplayBuffer.Height);
  FWaterBufferWidth := FDisplayBuffer.Width;
  FWaterBufferHeight := FDisplayBuffer.Height;
  FWaterBufferOffsetWidth := FWaterBufferWidth - 2;
  FWaterBufferOffsetHeight := FWaterBufferHeight - 2;
  FWaterBufferStartOffset := FWaterBufferWidth + 1;
  FWaterBufferEndOffset := FWaterBufferSize - FWaterBufferWidth - 2;
  //SetLength(FWaterBuffer, FWaterBufferSize * 2);
  SetLength(FWaterBufferA, FWaterBufferSize);
  SetLength(FWaterBufferB, FWaterBufferSize);
  SetLength(FScanLineOffset,FWaterBufferHeight);
  //FScanLineOffset := FBackGroundImage.GetScanLineLUT;
  For i:= 0 to FWaterBufferHeight - 1 do
  begin
    FScanLineOffset[i] := (i * FWaterBufferWidth) + 1; // +1 car on commence au pixel 1 et pas 0
  end;

  FCounterDrop := 0;
  FSwapBuffer := False;
  Randomize;

  InitColorMap;
end;

procedure TMainForm.InitColorMap;
begin
  // Initalisation de la palette de couleur, si utilisée
end;

procedure TMainForm.RenderScene;
begin
  // Coeur du rendu de la scene
  if FCounterDrop > 8 then
  begin
    WaveDrop(Random(FDisplayBuffer.MaxWidth), Random(FDisplayBuffer.MaxHeight), FWaterBufferA);
    FCounterDrop := 0;
  end;

  if FSwapBuffer then
  begin
    Computewater(FWaterBufferA,  FWaterBufferB);
    //RenderWater(FWaterBufferB);
    ParallelFor(1, FWaterBufferOffsetHeight, @RenderWaterLineParalelleProc, Pointer(FWaterBufferB));
    BlurWater(FWaterBufferB);
  end
  else
  begin
    Computewater(FWaterBufferB,  FWaterBufferA);
    //RenderWater(FWaterBufferA);
    ParallelFor(1, FWaterBufferOffsetHeight, @RenderWaterLineParalelleProc, Pointer(FWaterBufferA));
    BlurWater(FWaterBufferA);
  end;
  FSwapBuffer := not(FSwapBuffer);
  inc(FCounterDrop);
end;

procedure TMainForm.DoneScene;
// Finalisation de la scene
begin
  SetLength(FScanLineOffset,0);
  FScanLineOffset := nil;
  SetLength(FWaterBufferB, 0);
  FWaterBufferB := Nil;
  SetLength(FWaterBufferA, 0);
  FWaterBufferA := Nil;
  FreeAndNil(FBackgroundImage);
end;

end.
