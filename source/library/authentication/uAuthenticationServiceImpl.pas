unit uAuthenticationServiceImpl;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, cm_Messager, cm_interfaces,
  uUser, uUserDAO,
  uRoleUser, uRoleUserDAO,
  uRoleAuthority, uRoleAuthorityDAO,
  uAuthority,
  uAuthenticationService,
  uApp;

type

  { TAuthenticationService }

  TAuthenticationService = class(TCMMessageable, IAuthenticationService)
  public
    function Login(AUserCode, APassWord: string): boolean;
    function Check(aAuthority: TAuthority; AUser: TUser): boolean; overload;
    function Check(aAuthority: TAuthority; AUserCode: string): boolean; overload;
  end;

implementation

{ TAuthenticationService }

function TAuthenticationService.Login(AUserCode, APassWord: string): boolean;
var
  user: TUser = nil;
  userDAO: IUserDAO;
begin
  Messager.Debug('Login: %s.', ['begin']);

  Result := False;
  if InterfaceRegister.OutInterface(IUserDAO, userDAO) then
  begin
    user := userDAO.GetUser(AUserCode);
    if Assigned(user) then
    begin
      if user.Password = APassWord then
      begin
        messager.Debug('Login: %s.', ['权限验证成功']);
        Result := True;
      end;
      user.Free;
    end;
  end;
  Messager.Debug('Login: %s.', ['end']);

end;

function TAuthenticationService.Check(aAuthority: TAuthority; AUser: TUser): boolean;
begin
  Result := Self.Check(aAuthority, AUser.Code);
end;

function TAuthenticationService.Check(aAuthority: TAuthority; AUserCode: string): boolean;
var
  i: integer;
  user: TUser = nil;
  userDAO: IUserDAO;
  ruDAO: IRoleUserDAO;
  raDAO: IRoleAuthorityDAO;
  ruList: TRoleUserList = nil;
begin
  Messager.Debug('Check: %s.', ['begin']);
  Result := False;
  if InterfaceRegister.OutInterface(IRoleUserDAO, ruDAO) then
    if InterfaceRegister.OutInterface(IRoleAuthorityDAO, raDAO) then
    begin
      ruList := ruDAO.GetRoleUserListByUser(AUserCode);
      if ruList.Count > 0 then
        for i := 0 to rulist.Count - 1 do
          if (raDAO.ExistRoleAuthority(TRoleUser(rulist[i]).RoleCode, aAuthority.Code)) then
          begin
            messager.Debug('Check: %s.', ['权限验证成功']);
            Result := True;
            Exit;
          end;
      ruList.Free;
    end;
  Messager.Debug('Check: %s.', ['end']);
end;


end.
