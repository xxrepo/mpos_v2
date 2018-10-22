unit uRoleUserDAOImpl;

{$mode delphi}{$H+}


interface

uses
  Classes, SysUtils, DB,
  cm_sysutils, cm_DBUtils,
  uDAO,
  uRoleUser, uRoleUserDAO;

type

  { TRoleAuthorityDAO }

  TRoleUserDAO = class(TPOSDAO, IRoleUserDAO)
  private

  public

  public
    function GetRoleUser(const ARoleCode: string; const AUserCode: string): TRoleUser;
    function GetRoleUserListByUser(const AUserCode: string): TRoleUserList;
    function ExistRoleUser(const ARoleCode: string; const AUserCode: string): boolean;
  end;

resourcestring
  GetInstanceSQLFmt = 'select * from tbRoleAuthority where RoleCode = %s AND AuthorityCode = %s;';
  ExistOrderSQLFmt = 'select 1 from tbRoleAuthority where RoleCode = %s AND AuthorityCode = %s;';

implementation

{ TRoleAuthorityDAO }
function TRoleUserDAO.GetRoleUser(const ARoleCode: string; const AUserCode: string): TRoleUser;
var
  ds: TDataSet;
begin
  Result := nil;
  ds := GetDBHelper.Query(GetInstanceSQLFmt, [QuotedStr(ARoleCode)]);
  if Assigned(ds) then
  begin
    if not ds.IsEmpty then
    begin
      Result := TRoleUser.Create;
      Result.RoleCode := ds.FieldByName('RoleCode').AsString;
      Result.UserCode := ds.FieldByName('UserCode').AsString;
    end;
    ds.Free;
  end;
end;

function TRoleUserDAO.GetRoleUserListByUser(const AUserCode: string): TRoleUserList;
var
  ru: TRoleUser;
  ds: TDataSet;
begin
  Result := TRoleUserList.Create();
  ds := GetDBHelper.Query(GetInstanceSQLFmt, [QuotedStr(AUserCode)]);
  if Assigned(ds) then
  begin
    if not ds.IsEmpty then
      while not ds.EOF do
      begin
        ru := TRoleUser.Create;
        ru.RoleCode := ds.FieldByName('RoleCode').AsString;
        ru.UserCode := ds.FieldByName('UserCode').AsString;
        Result.Add(ru);
        ds.Next;
      end;
    ds.Free;
  end;
end;

function TRoleUserDAO.ExistRoleUser(const ARoleCode: string; const AUserCode: string): boolean;
var
  ds: TDataSet;
begin
  Result := False;
  ds := GetDBHelper.Query(ExistOrderSQLFmt, [QuotedStr(ARoleCode), QuotedStr(AUserCode)]);
  if Assigned(ds) then
  begin
    Result := not ds.IsEmpty;
    ds.Free;
  end;
end;

end.
