unit uCompany;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  Generics.Collections, Generics.Defaults,
  cm_interfaces,
  cm_sysutils,
  cm_DB;

type

  TCompany = class;

  TCompanyList = TObjectList<TCompany>;

  TCompany = class(TPersistent)
  protected
    FCode: string;
    FName: string;
    FAbbrName: string;
    FAreaConfig: string;
  public
    property Code: string read FCode write FCode;
    property Name: string read FName write FName;
    property AbbrName: string read FAbbrName write FAbbrName;
    property AreaConfig: string read FAreaConfig write FAreaConfig;
  end;

  ICompanyDAO = interface(ICMBase)
    ['{5EE95323-BE2B-4C53-932A-3C4BD3F9E41F}']
    function FindByCode(const ACode: string): TCompany;
    function GetList(): TCompanyList;
  end;


implementation



end.




