unit uSalePromotionService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, uSaleOrder;

type

  ISalePromotionService = interface(ICMBase)
    ['{323B5E56-49D3-408D-85C5-92B5C9286332}']
    function Add(): TSalePromotion;
    function Get(GUID: string): TSalePromotion;
    function SQL(ASaleOrder: TSalePromotion): string;
    function Save(ASaleOrder: TSalePromotion): boolean;
  end;


implementation

end.

