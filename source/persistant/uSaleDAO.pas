unit uSaleDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uDAO,
  uSalePO;

type

  ISaleDAO = interface(IPOSDAO)
    ['{8BA5E56B-20CB-4C47-9967-E21EA389992C}']
    function GetSaveOrderScript(AOrder: TSaleOrder): string;
    function SaveOrder(AOrder: TSaleOrder): Boolean;
    function SaveDetail(ADetail: TSaleDetail): Boolean;
    function SaveOrderEx(AOrderEx: TSaleOrderEx): Boolean;
    function ExistOrder(const AUUID: string): Boolean;
  end;

implementation

end.

