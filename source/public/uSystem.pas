unit uSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_parameter, cm_dialogs;


const
  //日志应是最先初始化的，以常量出现
  LogPath: string = 'log/';
  LogFileNamePrefix: string = 'mpos_';
  DefaultConfigFileName: string = 'config/mpos.xml';
  ThemeConfigFileName: string = 'config/themes.xml';
  XMLConfigParameterName: string = 'mpos.configFiles';

type

  IAppSystem = interface(ICMBase)
    ['{81D73F7E-8FBE-4D85-9E58-C787DD18192E}']
    function GetVersion: string;
    function IsTestMode: Boolean;
    function GetStartTime: TDateTime;
    function GetLoginTime: TDateTime;
    function GetMsgBox: TCMMsgBox;
    function GetParameter: ICMParameter;
    function GetLog: ICMLog;
    function GetWorkRect: TRect;
  end;


implementation

end.

