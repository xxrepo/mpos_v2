unit uFrameNavigator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
  cm_theme, cm_controlutils, cm_parameter, cm_messager,
  uFrame, uSystem;

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
    procedure LoadConfig;
  public
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
  AppSystem.GetMsgBox.ShowMessage(TPanel(Sender).Hint);
end;

constructor TNavigatorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHostingMenu := TCMGridLayoutHostingMenu.Create(PanelClient);
  FHostingMenu.TopSpacing := 4;
  FHostingMenu.LeftSpacing := 4;
  FHostingMenu.BorderSpacing := 4;
  FHostingMenu.SetColWidth(160);
  FHostingMenu.SetRowHeight(40);
  FHostingMenu.RowCount := 10;
  FHostingMenu.ColCount := 1;
  FHostingMenu.SelectBoxSpacing := 1;
  //FHostingMenu.SelectBoxWidth := 4;
  //FHostingMenu.SelectBoxSize := 10;

  FHostingMenu.AlignAtGrid := True;
end;

procedure TNavigatorFrame.LoadConfig;
var
  i: Integer;
  p: ICMParameter;
  panel: TPanel;
begin
  p := AppSystem.GetParameter.Get('navigator.items');
  if not p.IsNull then
    begin
      for i:=0 to p.ItemCount-1 do
        begin
          panel := TPanel.Create(Self);
          panel.Color := AppSystem.GetParameter.Get('themes.lavender.color3').AsInteger;
          panel.Parent := PanelClient;
          panel.Caption := p.GetItem(i).Get('caption').AsString;
          if not p.GetItem(i).Get('col').IsNull then
            FHostingMenu.PutLayoutControl(panel, p.GetItem(i).Get('col').AsInteger, p.GetItem(i).Get('row').AsInteger)
          else
            FHostingMenu.PutLayoutControl(panel);
          //test
          panel.Hint := p.GetItem(i).Get('call').AsString;
          panel.OnDblClick := @aa;
        end;
      FHostingMenu.ReLayout;
    end;
end;

procedure TNavigatorFrame.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  PanelTop.Color := ATheme.GetParameter.Get('navigator.topColor').AsInteger;
  PanelClient.Color := ATheme.GetParameter.Get('panelColor').AsInteger;
  PanelBottom.Color := ATheme.GetParameter.Get('navigator.bottomColor').AsInteger;
end;

end.

