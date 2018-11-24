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

  TASaleQueryForm = class(TAQueryServiceForm, IRunnable, INavNodeListener)
  private
    FSNLab: TALabel;
    FCodeLab: TALabel;
    FSNEdt: TAEdit;
    FCodeEdt: TAEdit;
    FGridLayout: TAGridLayout;
  public
    constructor Create(AOwner: TAComponent); override;
    procedure Run;
    procedure Selected(e: INavNodeEvent);
    procedure Opened(e: INavNodeEvent);
  public
    procedure FormShow(e: IFormEvent); override;
    procedure FormClick(e: IControlEvent); override;
    procedure FormKeyPressed(e: IKeyEvent); override;
  private
    procedure DoQuery;
  end;

const
  SaleQueryNodeName: string = 'SaleQuery';

implementation

uses cm_AWTControlUtils;

{ TASaleQueryForm }

constructor TASaleQueryForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  Self.SetTitle('交易查询');
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

procedure TASaleQueryForm.Run;
var
  n: INavigator;
  navNode: INavNode;
begin
  Messager.Error('Run()...');
  if InterfaceRegister.OutInterface(INavigator, n) then
    begin
      navNode := n.FindNode(SaleQueryNodeName);
      if Assigned(navNode) then
        navNode.SetListener(Self);
    end;
end;

procedure TASaleQueryForm.Selected(e: INavNodeEvent);
begin
  Self.BoundsRect := AppSystem.GetServiceRect;
  Self.ShowModal;
end;

procedure TASaleQueryForm.Opened(e: INavNodeEvent);
begin

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


end.

