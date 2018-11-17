unit uSaleQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_plat, cm_messager,
  cm_AWT, cm_AWTEventUtils, cm_AWTLayoutUtils,
  uNavigator,
  uAForm,
  uSystem;

type

  { TASaleQueryForm }

  TASaleQueryForm = class(TAQueryServiceForm)
  private
    FLab: TALabel;
    FEdt1: TAEdit;
    FEdt2: TAEdit;
    FFlowLayout: TAFlowLayout;
  public
    constructor Create(AOwner: TAComponent); override;
  end;

  { TNavigatorNodeListener }

  TNavigatorNodeListener = class(TCMBase, INavigatorNodeListener, IRunnable)
  public
    procedure Click(e: INavigatorNodeEvent);
    procedure DblClick(e: INavigatorNodeEvent);
    procedure Run;
  end;

  { TFormKeyAdapter }

  TFormKeyAdapter = class(TKeyAdapter)
  public
    procedure KeyPressed(e: IKeyEvent); override;
  end;

  { TXWinControlAdapter }

  TXWinControlAdapter = class(TWinControlAdapter)
  public
    procedure ControlDblClick(e: IControlEvent); override;
    procedure ControlEnter(e: IWinControlEvent); override;
  end;

implementation

{ TASaleQueryForm }

constructor TASaleQueryForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FLab := TALabel.Create(FQueryPanel);
  FLab.Parent := FQueryPanel;
  FLab.Top := 10;
  FLab.Left := 20;
  FLab.Caption := '请输入';
  FLab.Layout := tlBottom;
  //
  FEdt1 := TAEdit.Create(FQueryPanel);
  FEdt1.Parent := FQueryPanel;
  FEdt1.Left := 120;
  FEdt1.Top := 10;
  FEdt2 := TAEdit.Create(FQueryPanel);
  FEdt2.Parent := FQueryPanel;
  FEdt2.Left := 220;
  FEdt2.Top := 10;
  //
  FLab.AutoSize := False;
  FLab.Height := FEdt1.Height;

  FEdt1.AddWinControlListener(TXWinControlAdapter.Create);
  FEdt2.AddWinControlListener(TXWinControlAdapter.Create);

  //
  FFlowLayout := TAFlowLayout.Create(nil);
  FFlowLayout.Container := FQueryPanel;
  FFlowLayout.PutLayoutControl(FLab);
  FFlowLayout.PutLayoutControl(FEdt1);
  FFlowLayout.PutLayoutControl(FEdt2);
end;

{ TNavigatorNodeListener }

var
  f: TASaleQueryForm;

procedure TNavigatorNodeListener.Click(e: INavigatorNodeEvent);
begin
  f := TASaleQueryForm.Create(nil);

  f.SetTitle('你好，世界！');
  f.BoundsRect := AppSystem.GetServiceRect;
  f.BorderStyle := TFormBorderStyle.bsNone;

  f.AddButton('测试1');
  f.AddKeyListener(TFormKeyAdapter.Create);


  f.ShowModal;
end;

procedure TNavigatorNodeListener.DblClick(e: INavigatorNodeEvent);
begin

end;

procedure TNavigatorNodeListener.Run;
var
  n: INavigator;
begin
  if InterfaceRegister.OutInterface(INavigator, n) then
    begin
      DefaultMessager.Error('11111111111');
      n.GetNode('SaleQuery').SetListener(Self);
    end;
end;

{ TFormKeyAdapter }

procedure TFormKeyAdapter.KeyPressed(e: IKeyEvent);
begin
  if e.GetKeyCode = 27 then
    f.Close;
end;

{ TXWinControlAdapter }

procedure TXWinControlAdapter.ControlDblClick(e: IControlEvent);
begin
  AppSystem.GetMsgBar.ShowMessage(etError, IntToStr(e.GetAControl.GetHashCode) + #10 + e.GetAControl.Name);

  f.FFlowLayout.ControlOrientation := coRightToLeft;
  f.FFlowLayout.ReLayout;
end;

procedure TXWinControlAdapter.ControlEnter(e: IWinControlEvent);
begin
  AppSystem.GetMsgBar.ShowMessage(etInfo, IntToStr(e.GetAWinControl.GetHashCode) + #10 + e.GetAWinControl.ClassName);
end;

end.

