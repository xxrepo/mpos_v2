unit uExports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager, cm_InterfaceRegister,
  uDAO,
  uUser, uUserDAO, uUserDAOImpl,
  uRole, uRoleDAO, uRoleDAOImpl,
  uRoleUser, uRoleUserDAO, uRoleUserDAOImpl,
  uAuthority, uAuthorityDAO, uAuthorityDAOImpl,
  uRoleAuthority, uRoleAuthorityDAO, uRoleAuthorityDAOImpl,
  uAuthenticationService, uAuthenticationServiceImpl,
  uRegisterService, uRegisterServiceImpl,
  uTableStructureService, uTableStructureServiceImpl,
  uLoginService, uLoginServiceImpl,
  uCompany, uCompanyDAOImpl,
  uPOS, uSupport;

type

  { TExports }

  TExports = class(TCMMessageable)
  public
    procedure LoadExport(ARegister: ICMInterfaceRegister);
  end;

implementation

{ TExports }

procedure TExports.LoadExport(ARegister: ICMInterfaceRegister);
var
  userDao: IUserDAO;
  roleDao: IRoleDAO;
  ruDao: IRoleUserDAO;
  auDao: IAuthorityDAO;
  raDao: IRoleAuthorityDAO;
begin
  Messager.Debug('开始加载销售业务...');


  ARegister.PutInterface('IUserDAO', ISupport, TSupport.Create);

  if TPOSDAOFactory.GetInstance.OutDAO(TUserDAO, IUserDAO, userDao) then
    ARegister.PutInterface('IUserDAO', IUserDAO, userDao)
  else
    Messager.Error('UserDAO实例化失败！');

  if TPOSDAOFactory.GetInstance.OutDAO(TRoleDAO, IRoleDAO, roleDao) then
    ARegister.PutInterface('IRoleDAO', IRoleDAO, roleDao)
  else
    Messager.Error('IRoleDAO实例化失败！');

  if TPOSDAOFactory.GetInstance.OutDAO(TRoleUserDAO, IRoleUserDAO, ruDao) then
    ARegister.PutInterface('IRoleUserDAO', IRoleUserDAO, ruDao)
  else
    Messager.Error('IRoleUserDAO实例化失败！');

  if TPOSDAOFactory.GetInstance.OutDAO(TAuthorityDAO, IAuthorityDAO, auDao) then
    ARegister.PutInterface('IAuthorityDAO', IAuthorityDAO, auDao)
  else
    Messager.Error('IAuthorityDAO实例化失败！');

  if TPOSDAOFactory.GetInstance.OutDAO(TRoleAuthorityDAO, IRoleAuthorityDAO, raDao) then
    ARegister.PutInterface('IRoleAuthorityDAO', IRoleAuthorityDAO, raDao)
  else
    Messager.Error('IRoleAuthorityDAO实例化失败！');

  if (ARegister.PutInterface('', IAuthenticationService, TAuthenticationService.Create) < 0) then
    Messager.Error('IAuthenticationService实例化失败！');

  if (ARegister.PutInterface('', ITableStructureService, TTableStructureService.Create) < 0) then
    Messager.Error('ITableStructureService实例化失败！');

  if (ARegister.PutInterface('', IRegisterService, TRegisterService.Create()) < 0) then
    Messager.Error('IRegisterService实例化失败！');

  if (ARegister.PutInterface('', ILoginService, TLoginService.Create()) < 0) then
    Messager.Error('ILoginService实例化失败！');

  if (ARegister.PutInterface('', ICompanyDAO, TCompanyFileDAO.Create()) < 0) then
    Messager.Error('ICompanyDAO实例化失败！');

end;

end.









