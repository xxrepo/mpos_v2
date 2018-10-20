unit uStoreSalePromotionDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDAO, uStoreSaleOrder;

type
  //销售单促销信息类DAO
  IStoreSalePromotionDAO = interface(IPOSDAO)
    ['{043908E0-45C7-4D64-BF2A-96B193D4025B}']
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TStoreSalePromotion): string;
    function GetUpdateScript(Instance: TStoreSalePromotion): string;
    function GetDeleteScript(Instance: TStoreSalePromotion): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TStoreSalePromotion): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function Get(AGUID: string): TStoreSalePromotion; overload;
    function GetByParentGUID(AGUID: string): TStoreSalePromotions; overload;
  end;


implementation

end.


