unit uAuthenticationService;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces,
  uUser, uAuthority;

type
  IAuthenticationService = interface(ICMBase)
    ['{8E6EA0FA-A82A-4E8C-B997-D619FBBA9192}']
    function Login(AUserCode, APassWord: string): boolean;
    function Check(aAuthority: TAuthority; AUser: TUser): boolean; overload;
    function Check(aAuthority: TAuthority; AUserCode: string): boolean; overload;
  end;

implementation



end.
