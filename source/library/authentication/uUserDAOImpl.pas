unit uUserDAOImpl;

{$mode delphi}{$H+}


interface

uses
  Classes, SysUtils, DB,
  cm_sysutils, cm_DBUtils,
  uDAO,
  uUser, uUserDAO;

type

  { TUserDAO }

  TUserDAO = class(TPOSDAO, IUserDAO)
  private

  public

  public
    function GetUser(const AUserCode: string): TUser;
    function ExistUser(const AUserCode: string): boolean;
  end;

resourcestring
  GetUserSQLFmt = 'select * from tbShopEmployee where Code = %s;';
  ExistOrderSQLFmt = 'select 1 from tbShopEmployee where Code=%s;';

implementation

{ TUserDAO }
function TUserDAO.GetUser(const AUserCode: string): TUser;
var
  ds: TDataSet;
begin
  Result := nil;
  ds := GetDBHelper.Query(GetUserSQLFmt, [QuotedStr(AUserCode)]);
  if Assigned(ds) then
  begin
    if not ds.IsEmpty then
    begin
      Result := TUser.Create;
      Result.Code := ds.FieldByName('Code').AsString;
      Result.Name := ds.FieldByName('Name').AsString;
      Result.Password := ds.FieldByName('AccountPwd').AsString;
    end;
    ds.Free;
  end;
end;

function TUserDAO.ExistUser(const AUserCode: string): boolean;
var
  ds: TDataSet;
begin
  Result := False;
  ds := GetDBHelper.Query(ExistOrderSQLFmt, [QuotedStr(AUserCode)]);
  if Assigned(ds) then
  begin
    Result := not ds.IsEmpty;
    ds.Free;
  end;
end;

end.
