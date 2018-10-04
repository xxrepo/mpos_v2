unit uSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_parameter, cm_dialogs;


const
  //日志是最先的，以常量出现
  LogPath: string = 'log/';
  LogFileNamePrefix: string = 'mpos_';
  DefaultConfigFileName: string = 'config/mpos.xml';
  ThemeConfigFileName: string = 'config/themes.xml';
  XMLConfigParameterName: string = 'mpos.configFiles';

type

  IPOSSystem = interface(ICMBase)
    ['{9FC11795-80A7-4CEC-8510-58F1D11161F0}']
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

