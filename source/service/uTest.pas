unit uTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces;

type

  { ITest
    // 测试业务。
  }

  ITest = interface(ICMBase)
    ['{B9CBE1BF-2E9B-4F39-921B-56A174ADAE3D}']
    procedure Test;
    procedure Test2;
  end;


implementation

end.

