unit uShopProductDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDAO, uStoreSaleOrder;

type

  IShopProductDAO = interface(IPOSDAO)
    ['{E28F9BD5-7038-43F3-8DCF-A150FF05241A}']
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TShopProduct): string;
    function GetUpdateScript(Instance: TShopProduct): string;
    function GetDeleteScript(Instance: TShopProduct): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TShopProduct): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function GetByGID(AGID: integer): TShopProduct; overload;
    function GetByGDCode(AGDCode: string): TShopProduct; overload;
  end;

implementation

end.

