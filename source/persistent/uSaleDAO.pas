unit uSaleDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uDAO,
  uSalePO, uProductPO;

type

  ISaleDAO = interface(IPOSDAO)
    ['{8BA5E56B-20CB-4C47-9967-E21EA389992C}']
    function GetSaveOrderScript(AOrder: TSaleOrder): string;
    function SaveOrder(AOrder: TSaleOrder): boolean;
    function SaveDetail(ADetail: TSaleDetail): boolean;
    function SaveOrderEx(AOrderEx: TSaleOrderEx): boolean;
    function ExistOrder(const AUUID: string): boolean;
  end;

  IProductDAO = interface(IPOSDAO)
    ['{61F8F49F-7A6D-40E8-8D19-780D8853627E}']
    function GetByGID(AGID: integer): TProduct;
    function GetByGDCode(const AGDCode: string): TProduct;
    function GetByBarCode(const ABarCode: string): TProduct;
    function FindBySortCode(const ASortCode: string): TProductList;
    function FindByInputCode(const AInputCode: string): TProductList;
  end;


implementation

end.

