unit uStoreSaleSubDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDAO, uStoreSaleOrder;

type
  //销售单明细信息类DAO
  IStoreSaleSubDAO = interface(IPOSDAO)
    ['{96B349C9-E873-4488-A549-14543882A5FB}']
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TStoreSaleSub): string;
    function GetUpdateScript(Instance: TStoreSaleSub): string;
    function GetDeleteScript(Instance: TStoreSaleSub): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TStoreSaleSub): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function Get(AGUID: string): TStoreSaleSub; overload;
    function GetByParentGUID(AGUID: string): TStoreSaleSubs; overload;
  end;

implementation

end.

