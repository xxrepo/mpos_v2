unit uRoleDAO;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  uDAO,
  uRole;

type
  IRoleDAO = interface(IPOSDAO)
    ['{763904EC-A4FC-4550-A1F4-BF7A6EAC1700}']
    function GetRole(const ARoleCode: string): TRole;
    function ExistRole(const ARoleCode: string): boolean;
  end;

implementation


end.
