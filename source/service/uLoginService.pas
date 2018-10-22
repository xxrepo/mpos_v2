unit uLoginService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  ILoginService = interface
    ['{D484DA6B-3104-419D-935C-CFAC57A0DFA5}']
    function Login(): boolean;
    function LogOut(): boolean;
    function ReLogin(): boolean;
  end;

implementation

end.

