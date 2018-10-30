unit uRoleUser;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

type
  TRoleUser = class(TPersistent)
  private
    FID: integer;
    FRoleCode: string;
    FUserCode: string;
  published
    property ID: integer read FID write FID;
    property RoleCode: string read FRoleCode write FRoleCode;
    property UserCode: string read FUserCode write FUserCode;
  end;

  TRoleUserList = class(TObjectList<TRoleUser>);

implementation

end.
