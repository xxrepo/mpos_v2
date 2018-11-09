unit uFrameTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics,
  cm_theme, cm_Plat,
  uSale, uSaleDTO,
  uSystem,
  cm_AWTEvent, cm_AWTBase,
  uAForm;

type

  { TTestFrame }

  TTestFrame = class(TFrame)
    edtShopCode: TEdit;
    edtTermCode: TEdit;
    edtTermUUID: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure Panel4Click(Sender: TObject);
    procedure Panel5Click(Sender: TObject);
    procedure Panel6Click(Sender: TObject);
    procedure Panel7Click(Sender: TObject);
    procedure Panel8Click(Sender: TObject);
    procedure Panel9Click(Sender: TObject);
  private

  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TTestKeyLi }

  TTestKeyLi = class(TKeyAdapter)
  public
    procedure KeyPressed(e: IKeyEvent); override;
    procedure KeyReleased(e: IKeyEvent); override;
  end;

implementation

uses
  uDBInitialize, cm_messager, uPOS, cm_awt, uTest;

{$R *.frm}



{ TTestFrame }
constructor TTestFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TTestFrame.Panel1Click(Sender: TObject);
var
  ic: IThemeController;
  currName: string;
  tnl: TStrings;
  index: integer;
begin
  if InterfaceRegister.OutInterface(IThemeController, ic) then
  begin
    currName := ic.GetCurrTheme.GetName;
    tnl := ic.GetThemeNames;
    index := tnl.IndexOf(currName);
    if (index >= 0) and (index < tnl.Count - 1) then
      ic.SwitchTheme(tnl[index + 1])
    else if tnl.Count > 0 then
      ic.SwitchTheme(tnl[0]);
  end;
end;


procedure TTestFrame.Panel2Click(Sender: TObject);
var
  DBInit: IDBInitialize;
begin
  if InterfaceRegister.OutInterface(IDBInitialize, DBInit) then
  begin
    DBInit.DBInitialize;
  end;
end;

procedure TTestFrame.Panel5Click(Sender: TObject);
begin
  edtShopCode.Enabled := not edtShopCode.Enabled;
  edtTermCode.Enabled := not edtTermCode.Enabled;
  edtTermUUID.Enabled := not edtTermUUID.Enabled;
end;

procedure TTestFrame.Panel6Click(Sender: TObject);
var
  vo: TShowItem;
  vos: TShowItemList;
  i: integer;
  sb: ISaleBoard;
begin
  vos := TShowItemList.Create(False);
  for i := 0 to 800 do
  begin
    vo := TShowItem.Create;
    vo.Name := '椰树椰子汁 1000ml/罐';
    vo.BarCode := '632014412014';
    vo.Price := 6.98;
    vo.Quantity := i;
    vos.Add(vo.UUID, vo);
  end;
  if InterfaceRegister.OutInterface(ISaleBoard, sb) then
    sb.SetShowItemList(vos);
  vos.Free;
end;

procedure TTestFrame.Panel7Click(Sender: TObject);
var
  vo: TShowItem;
  sb: ISaleBoard;
begin
  vo := TShowItem.Create;
  vo.Name := '椰树椰子汁 1000ml/罐';
  vo.BarCode := '632014412014';
  vo.Price := 6.98;
  vo.Quantity := Random(88);
  if InterfaceRegister.OutInterface(ISaleBoard, sb) then
    sb.AddShowItem(vo);
end;

procedure TTestFrame.Panel8Click(Sender: TObject);
begin
  Panel8.Canvas.Brush.Bitmap := TBitmap.Create;
  Panel8.Canvas.Brush.Bitmap.LoadFromFile('d:/a.bmp');
  Panel8.Canvas.FillRect(11,11,111,111);
end;


var
  f: TAForm;
  e: TAEdit;
  p: TAPanel;
  l: TALabel;

procedure TTestFrame.Panel3Click(Sender: TObject);
var
  i: Integer;
  afont: TAFont;
begin
  f := TAForm.Create(nil);

  AppSystem.GetMsgBar.ShowMessage(etInfo, Self.FindComponent('Label1').Name);

  f.Color := clBlue;
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
  p.Color := clGray;

  e.Parent := p;

  l := TALabel.Create(f);
  l.Parent := p;
  l.Top := 20;
  l.Caption := '调试 test:';
  l.Font.Color := clRed;
  l.Font.Name := '黑体';

  afont := TAFont.Create;
  afont.Color := clGreen;
  afont.Size := 20;
  afont.Name := '黑体';

  //l.Font := afont;

  p.Canvas.TextOut(1, 1, '123');

  e.AddKeyListener(TTestKeyLi.Create);

  f.ShowModal;

  for i:=0 to f.ControlCount-1 do
    begin
      DefaultMessager.Info(f.Controls[i].Name);
    end;
end;

procedure TTestFrame.Panel4Click(Sender: TObject);
var
  f: TAPOSForm;
begin
  f := TAPOSForm.Create(nil);
  f.SetTitle('你好，世界！');
  f.BoundsRect := AppSystem.GetServiceRect;
  f.BorderStyle := TAFormBorderStyle.bsNone;
  f.ShowModal;

end;

procedure TTestFrame.Panel9Click(Sender: TObject);
var
  test: ITest;
begin
  if InterfaceRegister.OutInterface(ITest, test) then
    test.Test;
  //
  //Panel1.BorderStyle := ;
  //TForm.BorderStyle := ;
end;

{ TTestKeyLi }

procedure TTestKeyLi.KeyPressed(e: IKeyEvent);
var
  b: TACustomBitmap;
begin
  AppSystem.GetMsgBar.ShowMessage(etInfo, 'input:' + e.GetKeyChar);
  p.Canvas.Font.Color := clRed;
  p.Canvas.Font.Size := 20;
  p.Canvas.TextOut(1, 1, '123');

  p.Canvas.Brush.Color := clYellow;

  b := TABitmap.Create;
  p.Canvas.Brush.Bitmap := b;
  p.Canvas.Brush.Bitmap.LoadFromFile('d:/a.bmp');
  p.Canvas.FillRect(33,33,222,222);
end;

procedure TTestKeyLi.KeyReleased(e: IKeyEvent);
begin
  AppSystem.GetMsgBar.ShowMessage(etError, 'input:' + e.GetKeyChar);
end;



end.
