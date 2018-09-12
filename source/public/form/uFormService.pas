unit uFormService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  cm_theme,
  uForm;

type

  { TServiceForm }

  TServiceForm = class(TPOSForm)
    PanelClient: TPanel;
    PanelBottom: TPanel;
    PanelTop: TPanel;
  private
    FTitleLab: TLabel;
  protected
    procedure SetTitle(const ATitle: string); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetTheme(ATheme: ITheme); override;
  end;


implementation

{$R *.frm}

{ TServiceForm }

constructor TServiceForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FTitleLab := TLabel.Create(Self);
  FTitleLab.Parent := PanelTop;
  FTitleLab.Align := alLeft;
  FTitleLab.BorderSpacing.Left := 10;
  FTitleLab.Layout := tlCenter;
end;

procedure TServiceForm.SetTitle(const ATitle: string);
begin
  FTitleLab.Caption := ATitle;
end;

procedure TServiceForm.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  PanelTop.Color := ATheme.Parameter.Get('service.titleColor').AsInteger;
  FTitleLab.Font.Size := ATheme.Parameter.Get('service.titleFont').Get('size').AsInteger;
  FTitleLab.Font.Color := ATheme.Parameter.Get('service.titleFont').Get('color').AsInteger;
  FTitleLab.Font.Name := ATheme.Parameter.Get('service.titleFont').Get('name').AsString;
  PanelClient.Color := ATheme.Parameter.Get('panelColor').AsInteger;
  PanelBottom.Color := ATheme.Parameter.Get('service.buttonBarColor').AsInteger;
end;


end.

