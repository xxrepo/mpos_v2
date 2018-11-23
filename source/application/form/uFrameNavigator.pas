unit uFrameNavigator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics,  Dialogs,
  cm_theme, cm_controlutils, cm_parameter, cm_messager, cm_plat,
  uFrame, uSystem,
  uNavigator;

type

  { TNavigatorFrame }

  TNavigatorFrame = class(TPOSFrame, IThemeable)
    PanelBottom: TPanel;
    PanelTop: TPanel;
    procedure FrameResize(Sender: TObject);
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

end;

constructor TNavigatorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InterfaceRegister.PutInterface('INavigator', INavigator, Self);
end;

procedure SetStyle(menu: TCMGridLayoutMenu; param: ICMParameter);
begin
  menu.TopSpacing := 4;
  menu.LeftSpacing := 4;
  menu.BorderSpacing := 4;
  menu.SetColWidth(param.Get('ColWidth').AsInteger);
  menu.SetRowHeight(param.Get('RowHeight').AsInteger);
  menu.ColCount := param.Get('ColCount').AsInteger;
  menu.RowCount := param.Get('RowCount').AsInteger;
  menu.SelectBoxSpacing := 1;
  menu.AlignAtGrid := param.Get('AlignAtGrid').AsBoolean;
end;

procedure TNavigatorFrame.LoadConfig;
var
  p: ICMParameter;
begin
  p := AppSystem.GetParameter.Get('Navigator.Items.Style');
  if not p.IsNull then
    //SetStyle(FListPanel.FHostingMenu, p);
  p := AppSystem.GetParameter.Get('Navigator.Items');
  if not p.IsNull then
    begin
      //SetChildren(FListPanel, p);
    end;
end;

procedure TNavigatorFrame.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  PanelTop.Color := ATheme.GetParameter.Get('navigator.topColor').AsInteger;
  //PanelClient.Color := ATheme.GetParameter.Get('panelColor').AsInteger;
  PanelBottom.Color := ATheme.GetParameter.Get('navigator.bottomColor').AsInteger;
end;


end.

