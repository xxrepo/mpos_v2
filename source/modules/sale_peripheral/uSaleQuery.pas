unit uSaleQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_plat, cm_messager,
  cm_AWT, cm_AWTLayoutUtils,
  uNavigator,
  uAForm,
  uSystem;

type

  { TASaleQueryForm }

  TASaleQueryForm = class(TAQueryServiceForm)
  private
    FSNLab: TALabel;
    FCodeLab: TALabel;
    FSNEdt: TAEdit;
    FCodeEdt: TAEdit;
    FGridLayout: TAGridLayout;
  public
    constructor Create(AOwner: TAComponent); override;
    procedure FormShow(e: IFormEvent); override;
    procedure FormClick(e: IControlEvent); override;
    procedure FormKeyPressed(e: IKeyEvent); override;
  private
    procedure DoQuery;
  end;

  { TNavigatorNodeListener }

  TNavigatorNodeListener = class(TNavigatorNodeAdapter, IRunnable)
  public
    procedure Click(e: INavigatorNodeEvent); override;
    procedure Run;
  end;

implementation

uses cm_AWTControlUtils;

{ TASaleQueryForm }

constructor TASaleQueryForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FSNLab := TALabel.Create(FQueryPanel);
  FSNLab.Parent := FQueryPanel;
  FSNLab.Caption := '单据编号：';
  FCodeLab := TALabel.Create(FQueryPanel);
  FCodeLab.Parent := FQueryPanel;
  FCodeLab.Caption := '商品编码：';
  //
  FSNEdt := TAEdit.Create(FQueryPanel);
  FSNEdt.Parent := FQueryPanel;
  FCodeEdt := TAEdit.Create(FQueryPanel);
  FCodeEdt.Parent := FQueryPanel;
  //
  FQueryPanel.Height := 100;
  SetLabelHeightAndToBottom(20, [FSNLab, FCodeLab]);
  FGridLayout := TAGridLayout.Create(nil, FQueryPanel, 2, 2);
  FGridLayout.PutLayoutControls([FSNLab, FSNEdt, FCodeLab, FCodeEdt]);
  //
  AddButton('退出').AddControlListener(Self);
  AddButton('查询').AddControlListener(Self);
end;

procedure TASaleQueryForm.FormShow(e: IFormEvent);
begin
  inherited FormShow(e);
  FGridLayout.ReLayout;
end;

procedure TASaleQueryForm.FormClick(e: IControlEvent);
begin
  inherited FormClick(e);
  if ButtonCount >= 2 then
    begin
      if e.GetAControl = Buttons[0] then
        Close
      else if e.GetAControl = Buttons[1] then
        DoQuery;
    end;
end;

procedure TASaleQueryForm.FormKeyPressed(e: IKeyEvent);
begin
  inherited FormKeyPressed(e);
  if e.GetKeyCode = VK_RETURN then
    begin
      if FSNEdt.Focused then
        begin
          if FCodeEdt.CanFocus then
            FCodeEdt.SetFocus;
        end
      else if FCodeEdt.Focused then
        DoQuery;
    end
  else
    AppSystem.GetMsgBar.Hide;
end;

procedure TASaleQueryForm.DoQuery;
begin
  AppSystem.GetMsgBar.ShowMessage(etWarning, '功能尚未开发！');
end;

{ TNavigatorNodeListener }

var
  f: TASaleQueryForm;

procedure TNavigatorNodeListener.Click(e: INavigatorNodeEvent);
begin
  f := TASaleQueryForm.Create(nil);
  f.SetTitle('交易查询');
  f.BoundsRect := AppSystem.GetServiceRect;
  f.ShowModal;
  f.Free;
end;

procedure TNavigatorNodeListener.Run;
var
  n: INavigator;
begin
  if InterfaceRegister.OutInterface(INavigator, n) then
    begin
      DefaultMessager.Error('INavigator.GetNode(''SaleQuery'')...');
      n.GetNode('SaleQuery').SetListener(Self);
    end;
end;


end.

