unit uSaleMasterService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, uSaleOrder;

type

  ISaleMasterService = interface(ICMBase)
    ['{323B5E56-49D3-408D-85C5-92B5C9286332}']
    function Add(): TSaleMaster;
    function Get(GUID: string): TSaleMaster;
    function SQL(ASaleOrder: TSaleMaster): string;
    function Save(ASaleOrder: TSaleMaster): boolean;
  end;


implementation

end.

