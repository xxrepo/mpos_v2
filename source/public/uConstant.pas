unit uConstant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  //日志应是最先初始化的，以常量出现
  LogPath: string = 'log/';
  LogFileNamePrefix: string = 'mpos_';
  DefaultConfigFileName: string = 'config/mpos.xml';
  ThemeConfigFileName: string = 'config/themes.xml';
  XMLConfigParameterName: string = 'mpos.configFiles';
  //数据库 MessageHandler 的代号
  DBMessageHandlerCode: string = 'DB';

implementation

end.

