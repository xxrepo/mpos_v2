unit uMainTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls,
  cm_interfaces, cm_AWT, cm_AWTProxyUtils,
  uMain;

type

  { TMainTool }

  TMainTool = class(TCMBase, IMain)
  private
    FANavigation: TAPanel;
    FAStatus: TAPanel;
    FNav, FStatus: TPanel;
  public
    constructor Create(nav, status: TPanel);
  public // IMain
    function GetNavigation: TAPanel;
    function GetStatus: TAPanel;
  end;

implementation

{ TMainTool }

constructor TMainTool.Create(nav, status: TPanel);
begin
  FANavigation := nil;
  FAStatus := nil;
  FNav := nav;
  FStatus := status;
end;

function TMainTool.GetNavigation: TAPanel;
begin
  if not Assigned(FANavigation) then
    FANavigation := TProxyConverter.Create.ConvertPanel(FNav);
  Result := FANavigation;
end;

function TMainTool.GetStatus: TAPanel;
begin
  if not Assigned(FAStatus) then
    FAStatus := TProxyConverter.Create.ConvertPanel(FStatus);
  Result := FAStatus;
end;

end.

