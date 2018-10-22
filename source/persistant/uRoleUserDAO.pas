unit uRoleUserDAO;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  uDAO,
  uRoleUser;

type
  IRoleUserDAO = interface(IPOSDAO)
    ['{CF66456E-A23D-47BB-BA97-18A72B4F31F7}']
    function GetRoleUser(const ARoleCode: string; const AUserCode: string): TRoleUser;
    function GetRoleUserListByUser(const AUserCode: string): TRoleUserList;
    function ExistRoleUser(const ARoleCode: string; const AUserCode: string): boolean;
  end;

implementation


end.
