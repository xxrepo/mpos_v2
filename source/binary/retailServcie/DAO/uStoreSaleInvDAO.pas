unit uStoreSaleInvDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDAO, uStoreSaleOrder;

type

  IStoreSaleInvDAO = interface(IPOSDAO)
    ['{4F283F95-92CF-415A-87A9-D81FD2C57F6E}']
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TStoreSaleInv): string;
    function GetUpdateScript(Instance: TStoreSaleInv): string;
    function GetDeleteScript(Instance: TStoreSaleInv): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TStoreSaleInv): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
        function Get(AGUID: string): TStoreSaleInv; overload;
    function GetByParentGUID(AGUID: string): TStoreSaleInvs; overload;
  end;


implementation

end.

