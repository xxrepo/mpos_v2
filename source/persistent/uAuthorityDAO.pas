unit uAuthorityDAO;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  uDAO,
  uAuthority;

type
  IAuthorityDAO = interface(IPOSDAO)
    ['{0F5B05D2-EDC2-44B2-9E38-AD94DE44363D}']
    function GetAuthority(const aAuthorityCode: string): TAuthority;
    function ExistAuthority(const aAuthorityCode: string): boolean;
  end;

implementation



end.
