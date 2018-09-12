unit uFormPopupService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  cm_theme,
  uForm;

type

  { TPopupServiceForm }

  TPopupServiceForm = class(TPOSForm)
    PanelBottom: TPanel;
    PanelClient: TPanel;
    PanelTop: TPanel;
  private

  public
    procedure SetTheme(ATheme: ITheme); override;
  end;

var
  PopupServiceForm: TPopupServiceForm;

implementation

{$R *.frm}

{ TPopupServiceForm }

procedure TPopupServiceForm.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  PanelTop.Color := ATheme.Parameter.Get('service.titleColor').AsInteger;
  PanelClient.Color := ATheme.Parameter.Get('panelColor').AsInteger;
  PanelBottom.Color := ATheme.Parameter.Get('service.buttonBarColor').AsInteger;
end;

end.

