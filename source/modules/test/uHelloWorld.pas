unit uHelloWorld;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_messager,
  uTest, cm_Plat,
  cm_awt;

type

  { THelloWorld }

  THelloWorld = class(TCMMessageable, ITest)
  public
    procedure Test;
    //procedure UseTest;
    procedure Test2;
    procedure aa;
  end;

  { TTestThread }

  TTestThread = class(TThread)
  public
    procedure Execute; override;
  end;


implementation

{ TTestThread }

procedure TTestThread.Execute;
begin
  DefaultMessager.Debug('Execute()...');
end;

{ THelloWorld }

procedure THelloWorld.Test;
var
  f: TAForm;
  p: TAPanel;
  e: TAEdit;
  l: TALabel;
begin
  Messager.Debug('Test()...');
  // Do something you need to do.


  f := TAForm.Create(nil);
  p := TAPanel.Create(f);
  p.Color := $119999;
  p.Parent := f;
  p.Height := 100;
  p.Width := 200;

  l := TALabel.Create(f);
  l.Parent := p;
  l.Caption := '测试:';
  l.Left := 10;
  l.Top := 40;

  e := TAEdit.Create(f);
  e.Parent := p;
  e.Left := 100;
  e.Top := 40;

  f.Left := 100;
  f.Width := 600;
  f.ShowModal;

  Messager.Debug('Test().');
end;

procedure THelloWorld.Test2;
begin
  TThread.Synchronize(TTestThread.Create(False), @aa);
end;

procedure THelloWorld.aa;
begin
  Messager.Debug('------------------------ aa -----------------------');
end;

//procedure THelloWorld.UseTest;
//var
//  test: ITest;
//begin
//  if InterfaceRegister.OutInterface(ITest, test) then
//    test.Test;
//end;

end.

