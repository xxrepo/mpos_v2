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
    FLab: TALabel;
    FEdt1: TAEdit;
    FEdt2: TAEdit;
    FFlowLayout: TAFlowLayout;
  public
    constructor Create(AOwner: TAComponent); override;
    procedure FormShow(e: IFormEvent); override;
    procedure FormKeyPressed(e: IKeyEvent); override;
    procedure FormDblClick(e: IControlEvent); override;
    procedure FormEnter(e: IWinControlEvent); override;
  end;

  { TNavigatorNodeListener }

  TNavigatorNodeListener = class(TNavigatorNodeAdapter, IRunnable)
  public
    procedure Click(e: INavigatorNodeEvent); override;
    procedure Run;
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

  //
  FFlowLayout := TAFlowLayout.Create(nil, FQueryPanel);
  FFlowLayout.PutLayoutControls([FLab, FEdt1, FEdt2]);

  //
  AddButton('测试1');
  OpenDefaultFormEvent := True;
  OpenDefaultKeyEvent := True;
  //

  FEdt1.AddWinControlListener(Self);
  FEdt2.AddWinControlListener(Self);
end;

procedure TASaleQueryForm.FormShow(e: IFormEvent);
begin
  FFlowLayout.ControlOrientation := coRightToLeft;
  FFlowLayout.ReLayout;
end;

procedure TASaleQueryForm.FormKeyPressed(e: IKeyEvent);
begin
  if e.GetKeyCode = 27 then
    Close;
end;

procedure TASaleQueryForm.FormDblClick(e: IControlEvent);
begin
  AppSystem.GetMsgBar.ShowMessage(etError, IntToStr(e.GetAControl.GetHashCode) + #10 + e.GetAControl.Name);
end;

procedure TASaleQueryForm.FormEnter(e: IWinControlEvent);
begin
  AppSystem.GetMsgBar.ShowMessage(etInfo, IntToStr(e.GetAWinControl.GetHashCode) + #10 + e.GetAWinControl.ClassName);
end;

{ TNavigatorNodeListener }

var
  f: TASaleQueryForm;

procedure TNavigatorNodeListener.Click(e: INavigatorNodeEvent);
begin
  f := TASaleQueryForm.Create(nil);

  f.SetTitle('你好，世界！');
  f.BoundsRect := AppSystem.GetServiceRect;

  f.ShowModal;
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


end.

