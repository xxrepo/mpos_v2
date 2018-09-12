unit uSaleOrderService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, cm_interfaces, uSaleOrder;

type


  ISaleOrderService = interface(ICMBase)
    ['{323B5E56-49D3-408D-85C5-92B5C9286332}']
    function Add(): TSaleOrder;
    function Get(GUID: string): TSaleOrder;
    function SQL(ASaleOrder: TSaleOrder): string;
    function Save(ASaleOrder: TSaleOrder): boolean;
  end;



implementation

end.

