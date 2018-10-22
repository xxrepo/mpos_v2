unit uRoleDAOImpl;

{$mode delphi}{$H+}


interface

uses
  Classes, SysUtils, DB,
  cm_sysutils, cm_DBUtils,
  uDAO,
  uRole, uRoleDAO;

type

  { TRoleDAO }

  TRoleDAO = class(TPOSDAO, IRoleDAO)
  private

  public

  public
    function GetRole(const ARoleCode: string): TRole;
    function ExistRole(const ARoleCode: string): boolean;
  end;

resourcestring
  GetUserSQLFmt = 'select * from tbRole where Code = %s;';
  ExistOrderSQLFmt = 'select 1 from tbRole where Code = %s;';

implementation

{ TRoleDAO }
function TRoleDAO.GetRole(const ARoleCode: string): TRole;
var
  ds: TDataSet;
begin
  Result := nil;
  ds := GetDBHelper.Query(GetUserSQLFmt, [QuotedStr(ARoleCode)]);
  if Assigned(ds) then
  begin
    if not ds.IsEmpty then
    begin
      Result := TRole.Create;
      Result.Code := ds.FieldByName('Code').AsString;
      Result.Name := ds.FieldByName('Name').AsString;
      //Result.Password := ds.FieldByName('AccountPwd').AsString;
    end;
    ds.Free;
  end;
end;

function TRoleDAO.ExistRole(const ARoleCode: string): boolean;
var
  ds: TDataSet;
begin
  Result := False;
  ds := GetDBHelper.Query(ExistOrderSQLFmt, [QuotedStr(ARoleCode)]);
  if Assigned(ds) then
  begin
    Result := not ds.IsEmpty;
    ds.Free;
  end;
end;

end.
