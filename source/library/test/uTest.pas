unit uTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager;

type

  { TTest }

  TTest = class
  private
    FMessager: TCMMessager;
  public
    constructor Create;
    destructor Destroy; override;
    procedure test;
  end;

implementation

{ TTest }

constructor TTest.Create;
begin

end;

destructor TTest.Destroy;
begin
  FMessager.Free;
  inherited Destroy;
end;

procedure TTest.test;
var
  messager: TCMMessager;
begin
  messager := TCMMessageManager.GetInstance.GetMessager('test');
  messager.Info('hello world');
end;

end.

