unit uRoleAuthorityDAOImpl;

{$mode delphi}{$H+}


interface

uses
  Classes, SysUtils, DB,
  cm_sysutils, cm_DBUtils,
  uDAO,
  uRoleAuthority, uRoleAuthorityDAO;

type

  { TRoleAuthorityDAO }

  TRoleAuthorityDAO = class(TPOSDAO, IRoleAuthorityDAO)
  private

  public

  public
    function GetRoleAuthority(const ARoleCode: string; const aAuthorityCode: string): TRoleAuthority;
    function ExistRoleAuthority(const ARoleCode: string; const aAuthorityCode: string): boolean;
  end;

resourcestring
  GetUserSQLFmt = 'select * from tbRoleAuthority where RoleCode = %s AND AuthorityCode = %s;';
  ExistOrderSQLFmt = 'select 1 from tbRoleAuthority where RoleCode = %s AND AuthorityCode = %s;';

implementation

{ TRoleAuthorityDAO }
function TRoleAuthorityDAO.GetRoleAuthority(const ARoleCode: string; const aAuthorityCode: string): TRoleAuthority;
var
  ds: TDataSet;
begin
  Result := nil;
  ds := GetDBHelper.Query(GetUserSQLFmt, [QuotedStr(ARoleCode)]);
  if Assigned(ds) then
  begin
    if not ds.IsEmpty then
    begin
      Result := TRoleAuthority.Create;
      Result.RoleCode := ds.FieldByName('RoleCode').AsString;
      Result.AuthorityCode := ds.FieldByName('AuthorityCode').AsString;
    end;
    ds.Free;
  end;
end;

function TRoleAuthorityDAO.ExistRoleAuthority(const ARoleCode: string; const aAuthorityCode: string): boolean;
var
  ds: TDataSet;
begin
  Result := False;
  ds := GetDBHelper.Query(ExistOrderSQLFmt, [QuotedStr(ARoleCode), QuotedStr(aAuthorityCode)]);
  if Assigned(ds) then
  begin
    Result := not ds.IsEmpty;
    ds.Free;
  end;
end;

end.
