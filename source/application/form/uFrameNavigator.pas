unit uFrameNavigator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  cm_theme,
  uFrame;

type

  { TNavigatorFrame }

  TNavigatorFrame = class(TPOSFrame, IThemeable)
    PanelBottom: TPanel;
    PanelTitle: TPanel;
  private

  public
    procedure SetTheme(ATheme: ITheme); override;
  end;

implementation

{$R *.frm}

{ TNavigatorFrame }

procedure TNavigatorFrame.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  PanelTitle.Color := ATheme.Parameter.Get('color1').AsInteger;
  Self.Color := ATheme.Parameter.Get('color2').AsInteger;
end;

end.

