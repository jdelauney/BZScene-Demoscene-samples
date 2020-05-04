//======================= [ BZScene Demo FIREZ ]===============================
{ Description : Remake of an old school demo scene fx : The Fire
  This demo show how to :
   - create an attached gradient palette to a bitmap
   - Read or write a pixel color palette indice
   - Draw a bitmap on a Canvas
   - Use bitmap's canvas for text out
   - Animate with the Cadencer component
   - Use TBZStopWatch addon component for timing animation in FPS

     Note : By default on OS VSync is enabled so the max FPS will always synchronize around 60fps
	 This decrease, denpendantly of the complexity of the Frame rendering
}
//==============================================================================
unit uMainForm;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZColors, BZGraphic, BZBitmap, BZStopWatch, BZCadencer;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormShow(Sender : TObject);
  private
    FBitmapBuffer : TBZBitmap;
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;
  protected
    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);

  public
    Procedure RenderScene;
    Procedure InitScene;
    Procedure InitColorMap;
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

uses BZUtils, BZMath, BZParallelThread, SyncObjs, BZLogger;
{ TMainForm }

procedure TMainForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  FStopWatch.Stop;
  FCadencer.Enabled := False;
  CanClose := True;
end;

procedure TMainForm.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  FreeAndNil(FStopWatch);
  FreeAndNil(FCadencer);
  FreeAndNil(FBitmapBuffer);
end;

procedure TMainForm.FormShow(Sender : TObject);
begin
  InitScene;
  DoubleBuffered:=true;
  FStopWatch.Start;
  FCadencer.Enabled := True;
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  RenderScene;
  With FBitmapBuffer.Canvas do
  begin
    Font.Size := 64;
    Font.Color := clrYellow;
    Font.Color.Alpha :=64;
    DrawMode.PixelMode := dmSet;
    DrawMode.AlphaMode := amAlpha;
    TextOut(FBitmapBuffer.CenterX-64,320,'FIRE');
  end;
  FBitmapBuffer.DrawToCanvas(Canvas, ClientRect);
  Caption:='BZScene FireZ Demo : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
end;

procedure TMainForm.RenderScene;
var
  tmp:integer;
  i,x,y:integer;
  c:integer;
begin
   c:=0;
   for i:= 0 to 511 do
    begin
      x:=random(FBitmapBuffer.Width);
      y:=FBitmapBuffer.MaxHeight;
      c:=c+round(random(3)*0.8);

      FBitmapBuffer.Colors[x,y]:=Clamp((round((sin(c)+1)*0.5)*1023),0,1023);
    end;

  // Calcul du feu
  i:=FBitmapBuffer.MaxHeight-320;

  For y:= FBitmapBuffer.MaxHeight - 1 Downto 1 do
  begin
    for x:=1 to FBitmapBuffer.MaxWidth-1 do
    begin
      // on fait la moyenne des intensités des pixels (blur)
      tmp:=FBitmapBuffer.Colors[x+1,y+1]+
           FBitmapBuffer.Colors[x-1,y+1]+
           FBitmapBuffer.Colors[x,y+1]+
           FBitmapBuffer.Colors[x,y-1];
      tmp:=tmp shr 2;

      //if (y<i) then if (tmp>384) then tmp := tmp - 384;
      FBitmapBuffer.Colors[x,y]:= Clamp(tmp,0,1023);
    end;
  end;

end;

procedure TMainForm.InitScene;
begin

  FCadencer := TBZCadencer.Create(self);
  FCadencer.Enabled := False;
  FCadencer.OnProgress := @CadencerProgress;

  FStopWatch := TBZStopWatch.Create(self);

  Randomize;
  FBitmapBuffer := TBZBitmap.Create(Width,Height);
  FBitmapBuffer.Clear(clrBlack);
  FBitmapBuffer.UsePalette := True;


  // Create Color Map
  InitColorMap;

end;

procedure TMainForm.InitColorMap;
Var
    c1 : TBZColor;
    inv : Single;
begin

  inv := 1/1024; //256 couleurs
  c1.Create(0,0,0,0);
  FBitmapBuffer.ColorManager.CreateGradientColor(c1,0);
  c1.Create(16,0,0,32);
  FBitmapBuffer.ColorManager.CreateGradientColor(c1,192*Inv); //192
  c1.Create(32,0,0,128);
  FBitmapBuffer.ColorManager.CreateGradientColor(c1,320*Inv);  //288
  c1.Create(196,0,0,255);
  FBitmapBuffer.ColorManager.CreateGradientColor(c1,448*Inv);   //384
  c1.Create(255,255,0,255);
  FBitmapBuffer.ColorManager.CreateGradientColor(c1,768*Inv); //768
  c1 := clrWhite;
  FBitmapBuffer.ColorManager.CreateGradientColor(c1,1.0);

  FBitmapBuffer.ColorManager.MakeGradientPalette(1024);
end;



end.

