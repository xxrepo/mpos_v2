unit uDatabaseParameter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  cm_messager, cm_parameter, cm_ParameterUtils,
  cm_Plat,
  uDB;

type

  { TDatabaseParameter }

  TDatabaseParameter = class(TCMMessageable)
  private
    FParameterObj: TCMConstantParameter;
    FParameter: ICMConstantParameter;
  public
    constructor Create;
    destructor Destroy; override;
    function Load: Boolean;
  end;

implementation

{ TDatabaseParameter }

constructor TDatabaseParameter.Create;
begin
  inherited Create;
  FParameterObj := TCMConstantParameter.Create(nil, 'dbroot', 'dbroot');
  FParameter := FParameterObj;
end;

destructor TDatabaseParameter.Destroy;
begin
  inherited Destroy;
end;

function TDatabaseParameter.Load: Boolean;
var
  smt: IPOSStatement;
  ds: TDataSet;
begin
  Result := False;
  Messager.Debug('开始加载数据库参数...');
  if InterfaceRegister.OutInterface(IPOSStatement, smt) then
    begin
      ds := smt.Query('select * from parameter;');
      if Assigned(ds) then
        begin
          if not ds.IsEmpty then
            begin
              FParameterObj.ParameterSet.LoadParameters(FParameter, ds);
            end;
          ds.Free;
        end;
    end;
  Messager.Debug('结束加载数据库参数。');
end;

end.

