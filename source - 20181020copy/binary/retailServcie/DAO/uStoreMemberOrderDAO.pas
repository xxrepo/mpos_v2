unit uStoreMemberOrderDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDAO, uStoreSaleOrder;

type

  IStoreMemberOrderDAO = interface(IPOSDAO)
    ['{8D1FA224-2EAB-4A82-AB2F-38668EBF6520}']
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TStoreMemberOrder): string;
    function GetUpdateScript(Instance: TStoreMemberOrder): string;
    function GetDeleteScript(Instance: TStoreMemberOrder): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TStoreMemberOrder): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function Get(AGUID: string): TStoreMemberOrder; overload;
    function GetByParentGUID(AGUID: string): TStoreMemberOrders; overload;
  end;


implementation

end.

