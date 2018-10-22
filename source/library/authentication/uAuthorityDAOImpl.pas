unit uAuthorityDAOImpl;

{$mode delphi}{$H+}


interface

uses
  Classes, SysUtils, DB,
  cm_sysutils, cm_DBUtils,
  uDAO,
  uAuthority, uAuthorityDAO;

type

  { TAuthorityDAO }

  TAuthorityDAO = class(TPOSDAO, IAuthorityDAO)
  private

  public

  public
    function GetAuthority(const aAuthorityCode: string): TAuthority;
    function ExistAuthority(const aAuthorityCode: string): boolean;
  end;

resourcestring
  GetInstanceSQLFmt = 'select * from tbAuthority where Code = %s;';
  ExistOrderSQLFmt = 'select 1 from tbAuthority where Code = %s;';

implementation

{ TAuthorityDAO }
function TAuthorityDAO.GetAuthority(const aAuthorityCode: string): TAuthority;
var
  ds: TDataSet;
begin
  Result := nil;
  ds := GetDBHelper.Query(GetInstanceSQLFmt, [QuotedStr(aAuthorityCode)]);
  if Assigned(ds) then
  begin
    if not ds.IsEmpty then
    begin
      Result := TAuthority.Create;
      Result.Code := ds.FieldByName('Code').AsString;
      Result.Name := ds.FieldByName('Name').AsString;
    end;
    ds.Free;
  end;
end;

function TAuthorityDAO.ExistAuthority(const aAuthorityCode: string): boolean;
var
  ds: TDataSet;
begin
  Result := False;
  ds := GetDBHelper.Query(ExistOrderSQLFmt, [QuotedStr(aAuthorityCode)]);
  if Assigned(ds) then
  begin
    Result := not ds.IsEmpty;
    ds.Free;
  end;
end;

end.
