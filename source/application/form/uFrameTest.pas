unit uFrameTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics,
  cm_theme, cm_Plat,
  uSale, uSaleDTO,
  uSystem,
  cm_AWTEvent;

type

  { TTestFrame }

  TTestFrame = class(TFrame)
    edtShopCode: TEdit;
    edtTermCode: TEdit;
    edtTermUUID: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure Panel4Click(Sender: TObject);
    procedure Panel5Click(Sender: TObject);
    procedure Panel6Click(Sender: TObject);
    procedure Panel7Click(Sender: TObject);
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
  uDBInitialize, cm_messager, uPOS, cm_awt;

{$R *.frm}

{ TTestKeyLi }

procedure TTestKeyLi.KeyPressed(e: IKeyEvent);
begin
  AppSystem.GetMsgBar.ShowMessage(etInfo, 'input:' + e.GetKeyChar);
end;

procedure TTestKeyLi.KeyReleased(e: IKeyEvent);
begin
  AppSystem.GetMsgBar.ShowMessage(etError, 'input:' + e.GetKeyChar);
end;

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

var
  f: TAForm;
  e: TAEdit;
  p: TAPanel;

procedure TTestFrame.Panel3Click(Sender: TObject);
var
  i: Integer;
begin
  f := TAForm.Create(nil);

  //Self.Name := ;
  //Self.Controls[];
  //Self.Owner;
  //TEdit.Text := ;
  //TEdit.Parent;
  //TEdit.OnKeyDown := ;
  //TPanel
  //Self.ControlCount;
  //Self.Parent;

  AppSystem.GetMsgBar.ShowMessage(etInfo, Self.FindComponent('Label1').Name);

  f.Color := clBlue;
  f.Left := 100;
  f.Top := 100;
  f.Width := 600;
  f.Caption := 'haha';

  e := TAEdit.Create(f);
  e.Top := 20;
  e.Left := 80;
  e.Width := 200;
  e.Text := 'hello world';
  e.Parent := f;
  e.Clear;

  p := TAPanel.Create(f);
  p.Parent := f;
  p.Width := 400;
  p.Color := clYellow;

  e.Parent := p;

  e.AddKeyListener(TTestKeyLi.Create);

  f.ShowModal;
  //AppSystem.GetMsgBox.ShowMessage(IntToStr( f.ShowModal ));

  for i:=0 to f.ControlCount-1 do
    begin
      DefaultMessager.Info(f.Controls[i].Name);
    end;
end;

procedure TTestFrame.Panel4Click(Sender: TObject);
begin
  f.Color := clBlue;
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



end.
