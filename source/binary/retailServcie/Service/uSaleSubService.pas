unit uSaleSubService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, uSaleOrder;

type

  ISaleSubService = interface(ICMBase)
    ['{323B5E56-49D3-408D-85C5-92B5C9286332}']
    function Add(): TSaleSub;
    function Get(AGUID: string): TSaleSub;
    function GetByParentGUID(AGUID: string): TSaleSubs;
    function SQL(Instance: TSaleSub): string;
    function Save(Instance: TSaleSub): boolean;
  end;


implementation

end.

