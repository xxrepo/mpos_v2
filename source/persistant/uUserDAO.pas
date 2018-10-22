unit uUserDAO;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  uDAO,
  uUser;

type
  IUserDAO = interface(IPOSDAO)
    ['{763904EC-A4FC-4550-A1F4-BF7A6EAC1700}']
    function GetUser(const AUserCode: string): TUser;
    function ExistUser(const AUserCode: string): boolean;
  end;

implementation



end.
