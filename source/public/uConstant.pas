unit uConstant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  //日志应是最先初始化的，以常量出现
  LogPath: string = 'log' + PathDelim;
  LogFileNamePrefix: string = 'mpos_';
  //config file name
  DefaultConfigFileName: string = 'config' + PathDelim + 'mpos.xml';
  LibrariesConfigFileName: string = 'config' + PathDelim + 'libraries.xml';
  //parameter name
  MessageLevelParameterName: string = 'mpos.messageLevel';
  IcoParameterName: string = 'mpos.resources.ico';
  LogoParameterName: string = 'mpos.resources.logo';
  XMLConfigParameterName: string = 'mpos.configFiles';
  //
  ModulesPath: string = 'mods' + PathDelim;
  //数据库 MessageHandler 的代号
  DBMessageHandlerCode: string = 'DB';

  //数据库参数接口代号
  DBParameterInterfaceCode: string = 'DBParameter';

implementation

end.

