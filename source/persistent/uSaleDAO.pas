unit uSaleDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  uDAO,
  uSalePO, uProductPO;

type

  { ISaleDAO }

  ISaleDAO = interface(IPOSDAO)
    ['{157C9F84-3098-44FB-809A-84CA3DB6400A}']
    function GetSaveOrderScript(AOrder: TSaleOrder): string;
    function SaveOrder(AOrder: TSaleOrder): boolean;
    function SaveDetail(ADetail: TSaleDetail): boolean;
    function SaveOrderEx(AOrderEx: TSaleOrderEx): boolean;
    function ExistOrder(const AUUID: string): boolean;
    //------
    function GetSaleOrderDataSet(const beginTime, endTime, orderNo, prodCode, barCode: string; amount: currency;
      orderType, payType: integer; out theDataSet: TDataSet): boolean;
    function GetRecoverySaleOrderList(): TSaleOrderList;
    function GetSaleDetailsList(AOrderUUID: string): TSaleDetailList;
    function RemoveRecoverySaleOrder(AOrderUUID: string): boolean;
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
