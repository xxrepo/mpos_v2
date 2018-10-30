unit uUser;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

type

  { TUser }

  TUser = class(TPersistent)
  private
    FCode: string;
    FName: string;
    FPassword: string;
    FRoleCode: string;
  published
    property Code: string read FCode write FCode;
    property Name: string read FName write FName;
    property Password: string read FPassword write FPassword;
    property RoleCode: string read FRoleCode write FRoleCode;
  end;


  TUserList = class(TObjectList<TUser>);

implementation

end.

