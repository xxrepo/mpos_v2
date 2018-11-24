unit uTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_plat, cm_messager,
  cm_AWT, cm_AWTLayoutUtils,
  uAForm,
  uSystem,
  uNavigatorFrame,
  uMain, cm_dialogs;

type

  { TTestForm }

  TTestForm = class(TACustomServiceForm, IRunnable)
  private
    FFlowLayout: TAFlowLayout;
    FGridLayout: TAGridLayout;
    nf: TNavigatorFrame;
  protected
    procedure Run;
    procedure FormShow(e: IFormEvent); override;
    procedure FormKeyPressed(e: IKeyEvent); override;
  public
    constructor Create(AOwner: TAComponent); override;
  end;

implementation

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
  FGridLayout := TAGridLayout.Create(nil, FCenterPanel, 2, 4);
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


  nf := TNavigatorFrame.Create(Self);
  nf.Parent := FMainPanel;
  nf.Left := 0;

  //AppSystem.GetMsgBox.ShowMessage(IntToStr(nf.Color));

  Messager.Info('Create().');
end;

procedure TTestForm.Run;
begin
  BoundsRect := AppSystem.GetServiceRect;
  ShowModal;
end;

procedure TTestForm.FormShow(e: IFormEvent);
var
  main: IMain;
begin
  inherited FormShow(e);
  Messager.Info('FormShow()...');
  //FFlowLayout.ReLayout;
  FGridLayout.ReLayout;

  Self.AddButton('AAA');

  //nf.AutoSize := False;
  //nf.Height := Self.he;
  //nf.Width := 200;
  nf.Align := alClient;

  //nf.AddNode('', TCfg.Create('a', 'a v'));
  //nf.AddNode('', TCfg.Create('b', 'b v'));
  //nf.AddNode('a', TCfg.Create('aa', 'aa v'));
  //nf.AddNode('a', TCfg.Create('ab', 'ab v'));
  //nf.AddNode('aa', TCfg.Create('aaa', 'aaa v'));

  nf.RefreshDisplay;

  //AppSystem.GetMsgBox.ShowMessage( AppSystem.GetParameter.Get('navigator.nodes.node$2.nodes.colWidth').AsString );

  if InterfaceRegister.OutInterface(IMain, main) then
    nf.Parent := main.GetNavigation;

  Close;
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




