unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch, BZMath;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormShow(Sender : TObject);
  private
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;
    FDisplayBuffer : TBZBitmap;

    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    AnimAng, AnimAmp, AnimDamp : Single;
    MidWidth : Integer;

    procedure RenderTwister(Amp, Ang, Damp : Single; px, py, AWidth, aHeight : Integer; Const Vertical : Boolean = true);
    procedure RenderScene;
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender : TObject);
begin
  FCadencer := TBZCadencer.Create(self);
  FCadencer.Enabled := False;
  FCadencer.OnProgress := @CadencerProgress;
  FStopWatch := TBZStopWatch.Create(self);
  FDisplayBuffer := TBZBitmap.Create(Width, Height);
  AnimAng := 0.2;
  AnimAmp := 0;
  AnimDamp := 0.02;
  MidWidth := Width div 2;
end;

procedure TMainForm.FormDestroy(Sender : TObject);
begin
  FreeAndNil(FStopWatch);
  FreeAndNil(FCadencer);
  FreeAndNil(FDisplayBuffer);
end;

procedure TMainForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  FStopWatch.Stop;
  FCadencer.Enabled := False;
  CanClose := True;
end;

procedure TMainForm.FormShow(Sender : TObject);
begin
  DoubleBuffered:=true;
  FStopWatch.Start;
  FCadencer.Enabled := True;
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  RenderScene;
  FDisplayBuffer.DrawToCanvas(Canvas, ClientRect);
  Caption:='BZScene - Twister Demo : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
end;

procedure TMainForm.RenderTwister(Amp, Ang, Damp : Single; px, py, AWidth, aHeight : Integer; Const Vertical : Boolean = true);
var
  i, p1, p2, p3, p4, m : Integer;
begin
  With FDisplayBuffer.Canvas do
  begin
    if Vertical then
    begin
      m:= py + aHeight-1;
      For i := py To m do
      begin
        p1 :=Round(((Sin(((i*Amp)*cPi/180)+Ang))*aWidth)+MidWidth);
     	  p2:= Round(((Sin(((i*amp)*CPi/180)+ang+90*cPi/180))*aWidth)+MidWidth);
        p3:= Round(((Sin(((i*amp)*cPi/180)+ang+90*2*cPi/180))*aWidth)+MidWidth);
        p4:= Round(((Sin(((i*amp)*cPi/180)+ang+90*3*cPi/180))*aWidth)+MidWidth);

        Pen.Color:=clrRed;
        If p1<p2 then
        begin
          MoveTo(p1,i);
          LineTo(p2,i);
        End;

        Pen.Color:=clrYellow;
        If p2<p3 then
        begin
          MoveTo(p2,i);
          LineTo(p3,i);
        End;

        Pen.Color:=clrGreen;
        If p3<p4 then
        begin
          MoveTo(p3,i);
          LineTo(p4,i);
        End;

        Pen.Color:=clrBlue;
        If p4<p1 then
        begin
          MoveTo(p4,i);
          LineTo(p1,i);
        End;
      end;
    end
    else
    begin
      m:= px + aWidth-1;
      For i := px To m do
      begin
        p1 :=Round(((Sin(((i*Amp)*cPi/180)+Ang))*aHeight)+py);
     	  p2:= Round(((Sin(((i*amp)*pi/180)+ang+90*cPi/180))*aHeight)+py);
        p3:= Round(((Sin(((i*amp)*pi/180)+ang+90*2*cPi/180))*aHeight)+py);
        p4:= Round(((Sin(((i*amp)*pi/180)+ang+90*3*cPi/180))*aHeight)+py);

        Pen.Color:=clrRed;
        If p1<p2 then
        begin
          MoveTo(i, p1);
          LineTo(i, p2);
        End;

        Pen.Color:=clrYellow;
        If p2<p3 then
        begin
          MoveTo(i,p2);
          LineTo(i,p3);
        End;

        Pen.Color:=clrGreen;
        If p3<p4 then
        begin
          MoveTo(i,p3);
          LineTo(i,p4);
        End;

        Pen.Color:=clrBlue;
        If p4<p1 then
        begin
          MoveTo(i,p4);
          LineTo(i,p1);
        End;
      end;
    end;
  end;
end;

procedure TMainForm.RenderScene;
begin
  FDisplayBuffer.Clear(clrBlack);
  RenderTwister(AnimAmp, AnimAng, AnimDamp,(Width div 2),0,80,Height,True);
  RenderTwister(AnimAmp, AnimAng, AnimDamp,0,(Height div 2),Width,120,false);
  AnimAng := AnimAng+0.3;
  AnimAmp := AnimAmp+ AnimDamp;
  if (AnimAmp>1) or (AnimAmp<-1) then AnimDamp := -AnimDamp;
  Sleep(8);
end;

end.

