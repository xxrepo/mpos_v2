unit uSalePayService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, uSaleOrder;

type

  ISalePayService = interface(ICMBase)
    ['{323B5E56-49D3-408D-85C5-92B5C9286332}']
    function Add(): TSalePay;
    function Get(GUID: string): TSalePay;
    function SQL(ASaleOrder: TSalePay): string;
    function Save(ASaleOrder: TSalePay): boolean;
  end;


implementation

end.

