unit uStoreSalePayDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDAO, uStoreSaleOrder;

type
  //销售单支付信息类DAO
  IStoreSalePayDAO = interface(IPOSDAO)
    ['{65F70D95-245A-462A-90FB-4A02E0D73C23}']
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TStoreSalePay): string;
    function GetUpdateScript(Instance: TStoreSalePay): string;
    function GetDeleteScript(Instance: TStoreSalePay): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TStoreSalePay): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function Get(AGUID: string): TStoreSalePay; overload;
    function GetByParentGUID(AGUID: string): TStoreSalePays; overload;
  end;

implementation

end.

