unit uTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager,
  uSystem;

type

  { TTest }

  TTest = class(TCMMessageable)
  public
    procedure test;
  end;

implementation

{ TTest }

procedure TTest.test;
begin
  // 获得“value”
  AppSystem.GetParameter.Get('test.name').AsString;
  // 获得“123”
  AppSystem.GetParameter.Get('test.myName').AsString;
  // 获得能转换的类型值，否则为默认值
  AppSystem.GetParameter.Get('test.myName').AsInteger;
  // 获得“hello world”
  AppSystem.GetParameter.Get('test.name2.desc').AsString;
  // 与上等效
  AppSystem.GetParameter.Get('test.name2').Get('desc').AsString;
  // 获得“”，不存在的节点获得默认值，可以通过调用IsNull判断是否存在，
  //调用DataType获得数据类型
  AppSystem.GetParameter.Get('test.haha').AsString;
end;

end.

