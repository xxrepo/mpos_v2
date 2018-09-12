unit uSaleSubInvService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, uSaleOrder;

type

  ISaleSubInvService = interface(ICMBase)
    ['{323B5E56-49D3-408D-85C5-92B5C9286332}']
    function Add(): TSaleSubInv;
    function Get(AGUID: string): TSaleSubInv;
    function GetByParentGUID(AGUID: string): TSaleSubInvs;
    function SQL(Instance: TSaleSubInv): string;
    function Save(Instance: TSaleSubInv): boolean;
  end;


implementation

end.

