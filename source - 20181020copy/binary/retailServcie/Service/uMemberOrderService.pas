unit uMemberOrderService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, uSaleOrder;

type

  IMemberOrderService = interface(ICMBase)
    ['{323B5E56-49D3-408D-85C5-92B5C9286332}']
    function Add(): TMemberOrder;
    function Get(AGUID: string): TMemberOrder;
    function GetByParentGUID(AGUID: string): TMemberOrders;
    function SQL(Instance: TMemberOrder): string;
    function Save(Instance: TMemberOrder): boolean;
  end;


implementation

end.

