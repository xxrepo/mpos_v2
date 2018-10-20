unit uStoreSaleMainDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDAO, uStoreSaleOrder;

type

  //销售单类DAO
  IStoreSaleMainDAO = interface(IPOSDAO)
    ['{A8FF7CCA-BBD3-404F-9F63-AFA2248DF118}']
    ////////////////////////////////////////////////////////
    function GetInsertScript(Instance: TStoreSaleMain): string;
    function GetUpdateScript(Instance: TStoreSaleMain): string;
    function GetDeleteScript(Instance: TStoreSaleMain): string;
    ////////////////////////////////////////////////////////
    function Save(Instance: TStoreSaleMain): boolean;
    function Remove(AValue: string): boolean;
    ////////////////////////////////////////////////////////
    function Get(const AGUID: string): TStoreSaleMain;
  end;

implementation

end.

