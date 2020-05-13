unit uMainForm;

{$mode objfpc}{$H+}

// ALIGNEMENT
{$ifdef cpu64}
  {$ALIGN 16}

  {$CODEALIGN CONSTMIN=16}
  {$CODEALIGN LOCALMIN=16}
  {$CODEALIGN VARMIN=16}
{$endif}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZThreadTimer, BZStopWatch, BZClasses;

Const
  cAmp  : Integer = 118;
  cAmp2 : Integer = 174;
  cVara  : Integer = -128;
  cVarb : Integer = 128;

type

  { TMainForm }

  TMainForm = class(TForm)
    BZCadencer: TBZCadencer;
    procedure BZCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormKeyPress(Sender : TObject; var Key : char);
    procedure FormShow(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
  private
    FBitmapBuffer : TBZBitmap;
    FStopWatch : TBZStopWatch;
  protected
    CosLUT, SinLUT : Array[0..255] of Byte;
    FrameCounter :DWord;
    FPSCount : Single;
  public
    Procedure RenderScene(aDeltaTime: DWord);
    Procedure InitScene;
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

uses BZUtils, BZMath;

{ TMainForm }

procedure TMainForm.RenderScene(aDeltaTime : DWord);
var tmp:integer;
    C,i2,j2,x,y:integer;
begin
  For Y:=0 to FBitmapBuffer.MaxHeight Do
    Begin
      i2:=SinLUT[(aDeltaTime+y) mod 255];
      //j2:=SinLUT[(FrameCounter) mod 255];
      For X:=0 To FBitmapBuffer.MaxWidth Do
        Begin
          j2:=CosLUT[(aDeltaTime+x) mod 255];
          c:=SinLUT[(x+i2) mod 255]+CosLUT[(y+j2) mod 255];
          tmp:=(c mod 255);
          if tmp>0 then  FBitmapBuffer.SetPixelColorIndex(x,y,tmp);
          //FBitmapBuffer.Colors[x,y] := tmp;
        End;
    End;
end;

procedure TMainForm.InitScene;
var
  i:Integer;
  nc : TBZColor;
begin

  for i:= 0 to 255 do
  begin
    SinLUT[i]:=(round(sin(c2pi*(i*cInv255))*cAmp)+cVara);
    CosLUT[i]:= (round(cos(c2pi*(i*cInv255))*cAmp2)+cVarb);
  end;

  FStopWatch := TBZStopWatch.Create(self);

  Randomize;
  FBitmapBuffer := TBZBitmap.Create;
  FBitmapBuffer.SetSize(ClientWidth,ClientHeight);
  FBitmapBuffer.Clear(clrBlack);
  FBitmapBuffer.UsePalette := True;


  // Create Color Map

  FBitmapBuffer.ColorManager.CreateColor(clrBlack);
  for i:=1 to 63 do
  begin
    nc.Create(i div 4,i div 2,i div 2);
    FBitmapBuffer.ColorManager.CreateColor(nc);
  end;
  for i:=64 to 127 do
  begin
    nc.Create(i div 4,(i div 4),i div 2);
    FBitmapBuffer.ColorManager.CreateColor(nc);
  end;
  for i:=128 to 192 do
  begin
    nc.Create((i-63) div 4,(i-63) div 2,(i div 2));
    FBitmapBuffer.ColorManager.CreateColor(nc);
  end;
  for i:=193 to 230 do
  begin
    nc.Create((i-127) div 3,i div 2,i);
    FBitmapBuffer.ColorManager.CreateColor(nc);
  end;
  for i:=231 to 255 do
  begin
    nc.Create(i,i,i);
    FBitmapBuffer.ColorManager.CreateColor(nc);
  end;
  FBitmapBuffer.ColorManager.CreateColor(clrWhite);

  FPSCount := 0;
  FrameCounter:=0;

end;

procedure TMainForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  FStopWatch.Stop;
  BZCadencer.Enabled := False;
  CanClose := True;
end;

procedure TMainForm.BZCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  RenderScene(Round(NewTime*60));
  FBitmapBuffer.DrawToCanvas(Canvas, ClientRect);
  Caption:='BZScene PlasmaZ Demo : '+Format('%.*f FPS', [3,FStopWatch.getFPS]);
end;

procedure TMainForm.FormKeyPress(Sender : TObject; var Key : char);
begin
  if key = #27 then Close;
end;

procedure TMainForm.FormShow(Sender : TObject);
begin
  InitScene;
  DoubleBuffered:=true;
  FStopWatch.Start;
  //BZThreadTimer1.Enabled := True;
  BZCadencer.Enabled := True;
end;

procedure TMainForm.FormDestroy(Sender : TObject);
begin
  FreeAndNil(FStopWatch);
  FreeAndNil(FBitmapBuffer);
end;

end.

