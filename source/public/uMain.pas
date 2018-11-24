unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_AWT, cm_plat;

type

  IMainBlock = interface(ICMBase)
    ['{4941F424-9B61-4690-8116-21ECC9B79D21}']
    function GetHeader: TAPanel;
    function GetLeft: TAPanel;
    function GetCentral: TAPanel;
    function GetRight: TAPanel;
    function GetSlidingOut: TAPanel;
    function GetFooter: TAPanel;
  end;

function GetMainBlock: IMainBlock;

implementation

function GetMainBlock: IMainBlock;
begin
  Result := nil;
  InterfaceRegister.OutInterface(IMainBlock, Result);
end;

end.

