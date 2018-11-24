unit uMainTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls,
  cm_interfaces, cm_AWT, cm_AWTProxyExt,
  uMain;

type

  { TMainTool }

  TMainTool = class(TCMBase, IMainBlock)
  private
    FHeader: TAPanel;
    FLeft: TAPanel;
    FCentral: TAPanel;
    FRight: TAPanel;
    FSlidingOut: TAPanel;
    FFooter: TAPanel;
    lclheader, lclleft, lclcentral, lclright, lclslidingOut, lclfooter: TPanel;
  public
    constructor Create(header, left, central, right, slidingOut, footer: TPanel);
  public // IMainBlock
    function GetHeader: TAPanel;
    function GetLeft: TAPanel;
    function GetCentral: TAPanel;
    function GetRight: TAPanel;
    function GetSlidingOut: TAPanel;
    function GetFooter: TAPanel;
  end;

implementation

{ TMainTool }

constructor TMainTool.Create(header, left, central, right, slidingOut, footer: TPanel);
begin
  FHeader := nil;
  FLeft := nil;
  FCentral := nil;
  FRight := nil;
  FSlidingOut := nil;
  FFooter := nil;
  lclheader := header;
  lclleft := left;
  lclcentral := central;
  lclright := right;
  lclslidingOut := slidingOut;
  lclfooter := footer;
end;

function TMainTool.GetHeader: TAPanel;
begin
  if not Assigned(FHeader) then
    FHeader := TProxyConverter.Create.ConvertPanel(lclheader);
  Result := FHeader;
end;

function TMainTool.GetLeft: TAPanel;
begin
  if not Assigned(FLeft) then
    FLeft := TProxyConverter.Create.ConvertPanel(lclleft);
  Result := FLeft;
end;

function TMainTool.GetCentral: TAPanel;
begin
  if not Assigned(FCentral) then
    FCentral := TProxyConverter.Create.ConvertPanel(lclcentral);
  Result := FCentral;
end;

function TMainTool.GetRight: TAPanel;
begin
  if not Assigned(FRight) then
    FRight := TProxyConverter.Create.ConvertPanel(lclright);
  Result := FRight;
end;

function TMainTool.GetSlidingOut: TAPanel;
begin
  if not Assigned(FSlidingOut) then
    FSlidingOut := TProxyConverter.Create.ConvertPanel(lclslidingOut);
  Result := FSlidingOut;
end;

function TMainTool.GetFooter: TAPanel;
begin
  if not Assigned(FFooter) then
    FFooter := TProxyConverter.Create.ConvertPanel(lclfooter);
  Result := FFooter;
end;


end.

