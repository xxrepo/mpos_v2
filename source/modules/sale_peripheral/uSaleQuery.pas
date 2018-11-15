unit uSaleQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_plat, cm_messager,
  cm_AWT, cm_AWTEventUtils,
  uNavigator,
  uAForm,
  uSystem;

type

  { TASaleQueryForm }

  TASaleQueryForm = class(TAQueryServiceForm)
  private
    FLab: TALabel;
    FEdt: TAEdit;
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
  //FLab.Font.Height := 20;
  FEdt := TAEdit.Create(FQueryPanel);
  FEdt.Parent := FQueryPanel;
  FEdt.Left := 120;
  FEdt.Top := 10;

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

end.

