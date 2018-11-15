unit uRoleDAO;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  uDAO,
  uRole;

type
  IRoleDAO = interface(IPOSDAO)
    ['{E437F739-9660-4AA4-854E-00648394A7BE}']
    function GetRole(const ARoleCode: string): TRole;
    function ExistRole(const ARoleCode: string): boolean;
  end;

implementation


end.
