unit uHelloWorld;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_messager,
  uTest, cm_plat, uSystem,
  cm_AWT, cm_AWTBase,
  cm_AWTEvent;

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

  TTestKeyLi = class(TKeyAdapter)
  public
    procedure KeyPressed(e: IKeyEvent); override;
    procedure KeyReleased(e: IKeyEvent); override;
  end;


implementation

{ TTestThread }

procedure TTestThread.Execute;
begin
  DefaultMessager.Debug('Execute()...');
end;

{ THelloWorld }

var
  f: TAForm;
  e: TAEdit;
  p: TAPanel;
  l: TALabel;

procedure THelloWorld.Test;
var
  i: Integer;
  afont: TAFont;
begin
  try


  f := TAForm.Create(nil);

  //AppSystem.GetMsgBar.ShowMessage(etInfo, Self.FindComponent('Label1').Name);

  f.Color := $ff0000;
  f.Left := 100;
  f.Top := 100;
  f.Width := 660;
  f.Caption := 'haha';

  e := TAEdit.Create(f);
  e.Top := 20;
  e.Left := 200;
  e.Width := 200;
  e.Text := 'hello world';
  e.Parent := f;
  e.Clear;

  p := TAPanel.Create(f);
  p.Parent := f;
  p.Width := 600;
  p.Height := 200;
  p.Color := $cccccc;
  p.Align := alBottom;

  e.Parent := p;

  l := TALabel.Create(f);
  l.Parent := p;
  l.Top := 20;
  l.Caption := '调试 test:';
  l.Font.Color := $0000ff;
  l.Font.Name := '黑体';

  afont := TAFont.Create;
  afont.Color := $00ff00;
  afont.Size := 20;
  afont.Name := '黑体';

  //l.Font := afont;

  p.Canvas.TextOut(1, 1, '123');

  e.AddKeyListener(TTestKeyLi.Create);

  f.ShowModal;

  Messager.Debug('Test().');


  except
    on e: Exception do
      AppSystem.GetMsgBox.ShowMessage(e.ClassName+#10+e.Message);
  end;
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


{ TTestKeyLi }

procedure TTestKeyLi.KeyPressed(e: IKeyEvent);
var
  b: TACustomBitmap;
begin
  AppSystem.GetMsgBar.ShowMessage(etInfo, 'input:' + e.GetKeyChar);
  p.Canvas.Font.Color := $ff0000;
  p.Canvas.Font.Size := 20;
  p.Canvas.TextOut(1, 1, '123');

  p.Canvas.Brush.Color := $ffff00;

  b := TABitmap.Create;
  //AppSystem.GetMsgBox.ShowMessage( b.ClassName );
  //AppSystem.GetMsgBox.ShowMessage( booltostr( Assigned(b.GetPeer), true) );
  //AppSystem.GetMsgBox.ShowMessage( booltostr( Assigned(b.GetPeer.GetDelegate), true) );
  //AppSystem.GetMsgBox.ShowMessage( b.GetPeer.GetDelegate.ClassName );

  p.Canvas.Brush.Bitmap := b;
  p.Canvas.Brush.Bitmap.LoadFromFile('d:/a.bmp');
  p.Canvas.FillRect(33,33,222,222);
end;

procedure TTestKeyLi.KeyReleased(e: IKeyEvent);
begin
  AppSystem.GetMsgBar.ShowMessage(etError, 'input:' + e.GetKeyChar);
end;

end.

