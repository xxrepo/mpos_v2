unit uRoleAuthorityDAO;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  uDAO,
  uRoleAuthority;

type
  IRoleAuthorityDAO = interface(IPOSDAO)
    ['{CCB71688-55D4-4787-BE96-DE371C15C21B}']
    function GetRoleAuthority(const ARoleCode: string; const aAuthorityCode: string): TRoleAuthority;
    function ExistRoleAuthority(const ARoleCode: string; const aAuthorityCode: string): boolean;
  end;

implementation



end.
