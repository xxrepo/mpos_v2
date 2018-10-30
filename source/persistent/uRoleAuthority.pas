unit uRoleAuthority;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

type

  TRoleAuthority = class(TPersistent)
  private
    FRoleCode: string;
    FAuthorityCode: string;
  published
    property RoleCode: string read FRoleCode write FRoleCode;
    property AuthorityCode: string read FAuthorityCode write FAuthorityCode;
  end;

  TRoleAuthorityList = class(TObjectList<TRoleAuthority>);


implementation

end.

