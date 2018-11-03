unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  cm_logutils,
  cm_system;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure println(msg: string);
  public
    { public declarations }
    procedure SelfTest;
  end;

  { TA }

  TA = class
  public
    procedure xx;
  end;

procedure test;

var
  Form1: TForm1;
  Log: TCMJointFileLogger;
  t: TThread;
  A: TA;

implementation

procedure test;
begin
  Log.Info('---- test ----');
end;

{$R *.frm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Log := TCMJointFileLogger.Create(Self);
  Log.FileName := 'aa.log';
  Log.Info('====================================================');
  A := TA.Create;
end;

procedure TForm1.println(msg: string);
begin
  Memo1.Lines.Add(msg);
end;

procedure TForm1.SelfTest;
begin
  Log.Info('---- SelfTest ----');
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i, j: Integer;
begin
  //t := TThread.CreateAnonymousThread(@test);
  //println('-----------');

  for i:=0 to 10 do
    begin
      for j:=0 to 10 do
        TThread.CreateAnonymousThread(@test);
      println(IntToStr( GetProcessMemorySize(GetProcessID) ));
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  println(BoolToStr(Assigned(t), True));
  println(BoolToStr(t = nil, True));
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i, j: Integer;
begin
  //t := TThread.ExecuteInThread(@SelfTest);
  //println('-----------');


  for i:=0 to 100 do
    begin
      for j:=0 to 200 do
        //TThread.ExecuteInThread(@SelfTest);
        TThread.ExecuteInThread(@A.xx);
      println(IntToStr( GetProcessMemorySize(GetProcessID) ));
    end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  println(IntToStr(t.GetHashCode));
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  //println(IntToStr(GetProcessID));

  println(IntToStr( GetProcessMemorySize(GetProcessID) ));
end;

{ TA }

var
  k: Integer;

procedure TA.xx;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa');
  Log.Info('>>' + IntToStr(k));
  k := k + 1;
  ss.Free;
end;

end.

