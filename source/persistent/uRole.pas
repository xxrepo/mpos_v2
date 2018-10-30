unit uRole;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

type

  TRole = class(TPersistent)
  private
    FCode: string;
    FName: string;
  published
    property Code: string read FCode write FCode;
    property Name: string read FName write FName;
  end;

  TRoleList = class(TObjectList<TRole>);

implementation

end.
