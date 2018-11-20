unit uTest;

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

  { TNavigatorNodeListener }

  TNavigatorNodeListener = class(TNavigatorNodeAdapter, IRunnable)
  public
    procedure Click(e: INavigatorNodeEvent); override;
    procedure Run;
  end;


  { TTestForm }

  TTestForm = class(TACustomServiceForm)
  private
    FFlowLayout: TAFlowLayout;
    FGridLayout: TAGridLayout;
  protected
    procedure FormShow(e: IFormEvent); override;
    procedure FormKeyPressed(e: IKeyEvent); override;
  public
    constructor Create(AOwner: TAComponent); override;
  end;

implementation

{ TNavigatorNodeListener }

procedure TNavigatorNodeListener.Click(e: INavigatorNodeEvent);
var
  f: TTestForm;
begin
  DefaultMessager.Error('TTestForm----------------------------------');
  f := TTestForm.Create(nil);
  f.SetTitle('你好吗？');

  DefaultMessager.Error('TTestForm bbb');
  f.BoundsRect := AppSystem.GetServiceRect;


  DefaultMessager.Error('TTestForm cc');
  f.ShowModal;
  f.Free;
end;

procedure TNavigatorNodeListener.Run;
var
  n: INavigator;
begin
  if InterfaceRegister.OutInterface(INavigator, n) then
    begin
      DefaultMessager.Error('11111111111');
      n.GetNode('Test').SetListener(Self);
    end;
end;

{ TTestForm }

constructor TTestForm.Create(AOwner: TAComponent);
var
  l1, l2: TALabel;
  e1, e2: TAEdit;
begin
  inherited Create(AOwner);
  //
  Messager.Info('Create()...');
  FFlowLayout := TAFlowLayout.Create(nil, FCenterPanel);
  FGridLayout := TAGridLayout.Create(nil, FCenterPanel);
  FCenterPanel.Color := clYellow;

  l1 := TALabel.Create(Self);
  l1.Parent := FCenterPanel;
  l1.AutoSize := False;
  l1.Width := 60;
  l2 := TALabel.Create(Self);
  l2.Parent := FCenterPanel;
  l2.AutoSize := False;
  l2.Width := 60;
  e1 := TAEdit.Create(Self);
  e1.Parent := FCenterPanel;
  e2 := TAEdit.Create(Self);
  e2.Parent := FCenterPanel;

  Messager.Info('2222');
  l1.Caption := 'AAA';
  l2.Caption := 'BBB';

  //FFlowLayout.PutLayoutControls([l1, e1]);
  //FFlowLayout.PutLayoutControlsToNewLine([l2, e2]);

  FGridLayout.PutLayoutControls([l1, e1, l2, e2]);

  Messager.Info('Create().');
end;

procedure TTestForm.FormShow(e: IFormEvent);
begin
  inherited FormShow(e);
  Messager.Info('FormShow()...');
  //FFlowLayout.ReLayout;

  Self.AddButton('AAA');
end;

procedure TTestForm.FormKeyPressed(e: IKeyEvent);
begin
  inherited FormKeyPressed(e);
  if e.GetKeyCode = VK_F1 then
    //FFlowLayout.ReLayout;
    FGridLayout.ReLayout
  else if e.GetKeyCode = VK_F2 then
    begin
      FGridLayout.DefaultColWidth := 60;
      FGridLayout.ColCount := 2;
    end;
end;

end.




