unit uAuthority;


{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

type

  TAuthority = class(TPersistent)
  private
    FCode: string;
    FName: string;
    FDescribe: string;
  published
    property Code: string read FCode write FCode;
    property Name: string read FName write FName;
    property Describe: string read FDescribe write FDescribe;
  end;

  TAuthorityList = class(TObjectList<TAuthority>);

implementation

end.

