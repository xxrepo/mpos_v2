unit uFrameNavigator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
  cm_theme, cm_controlutils,
  uFrame,
  uMPOS;

type

  { TNavigatorFrame }

  TNavigatorFrame = class(TPOSFrame, IThemeable)
    PanelClient: TPanel;
    PanelBottom: TPanel;
    PanelTop: TPanel;
    procedure FrameResize(Sender: TObject);
  private
    FHostingMenu: TCMGridLayoutHostingMenu;
    procedure aa(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetTheme(ATheme: ITheme); override;
  end;

implementation

{$R *.frm}

{ TNavigatorFrame }

procedure TNavigatorFrame.FrameResize(Sender: TObject);
begin
  FHostingMenu.ReLayout;
end;

procedure TNavigatorFrame.aa(Sender: TObject);
begin
  TPanel(Sender).Caption :=
  POSSystem.GetParameter.Get('navigation.items.item.caption').AsString;
end;

constructor TNavigatorFrame.Create(AOwner: TComponent);
var
  p: TPanel;
begin
  inherited Create(AOwner);
  FHostingMenu := TCMGridLayoutHostingMenu.Create(PanelClient);
  FHostingMenu.SetColWidth(79);
  FHostingMenu.SetRowHeight(40);
  FHostingMenu.RowCount := 2;
  FHostingMenu.ColCount := 2;
  FHostingMenu.SelectBoxSpacing := 2;
  //FHostingMenu.SelectBoxWidth := 3;

  p := TPanel.Create(Self);
  p.Color := clRed;
  p.Parent := PanelClient;
  p.OnClick := @aa;
  FHostingMenu.AddLayoutControl(p);

  p := TPanel.Create(Self);
  p.Color := clGreen;
  p.Parent := PanelClient;
  FHostingMenu.PutLayoutControl(p);


  FHostingMenu.AlignAtGrid := True;
  FHostingMenu.ReLayout;
end;

procedure TNavigatorFrame.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  PanelTop.Color := ATheme.Parameter.Get('navigator.topColor').AsInteger;
  PanelClient.Color := ATheme.Parameter.Get('panelColor').AsInteger;
  PanelBottom.Color := ATheme.Parameter.Get('navigator.bottomColor').AsInteger;
end;

end.

