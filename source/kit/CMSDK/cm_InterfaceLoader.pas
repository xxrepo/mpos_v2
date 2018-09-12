unit cm_InterfaceLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_InterfaceRegister;

type

  ICMLibInfo = interface
    ['{31BE4AED-DF3C-427B-B541-6C768A12DC81}']
    function GetFileName: string; //加载的文件名
    function GetProperties: TStrings; //配置的属性
    function GetLoadHandle: THandle;
    function GetLoadExportAddress: Pointer; //引出方法的地址
  end;

  { ICMInterfaceLoader }

  ICMInterfaceLoader = interface
    ['{E506B99A-8B0A-451D-9415-FB87D43C8DA3}']
    function LoadFile(const AFileName: string; AProperties: TStrings=nil): Boolean;
    procedure LoadByConfig(const AConfigFileName: string); //依配置文件加载
    procedure LoadDirAll(const ADirPath: string); //加载指定目录下的所有文件
  end;

  TLoadLibraryExportProc = function(ARegister: ICMInterfaceRegister; const AInfo: ICMLibInfo): Boolean; stdcall;
  TUnloadLibraryExportProc = function(AStatus: Integer): Boolean; stdcall;

const
  DefaultLoadLibraryExportName: string = 'LoadExport';
  DefaultUnloadLibraryExportName: string = 'UnloadExport';

implementation


end.

