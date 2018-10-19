unit cm_InterfaceLoaderImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils,
  cm_interfaces,
  cm_messager,
  cm_sysutils,
  cm_fileutils,
  cm_DOM,
  cm_XML,
  cm_InterfaceRegister,
  cm_InterfaceLoader;

type

  { TCMLibInfo }

  TCMLibInfo = class(TCMBase, ICMLibInfo)
  private
    FFileName: string;
    FProperties: TStrings;
    FLoadHandle: THandle;
    FLoadExportAddress: Pointer;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    property Properties: TStrings read FProperties write FProperties;
    property LoadHandle: THandle read FLoadHandle write FLoadHandle;
    property LoadExportAddress: Pointer read FLoadExportAddress write FLoadExportAddress;
  public
    function GetFileName: string;
    function GetProperties: TStrings;
    function GetLoadHandle: THandle;
    function GetLoadExportAddress: Pointer;
  end;

  { TCMInterfaceLoader }

  TCMInterfaceLoader = class(TCMMessageableComponent, ICMInterfaceLoader)
  private
    FInterfaceRegister: ICMInterfaceRegister;
    FLoadLibraryExportName: string;
    FUnloadLibraryExportName: string;
  protected
    FInfoList: TInterfaceList;
  public
    constructor Create(AOwner: TComponent; ARegister: ICMInterfaceRegister); reintroduce;
    destructor Destroy; override;
    property InterfaceRegister: ICMInterfaceRegister read FInterfaceRegister write FInterfaceRegister;
    property LoadLibraryExportName: string read FLoadLibraryExportName write FLoadLibraryExportName;
    property UnloadLibraryExportName: string read FUnloadLibraryExportName write FUnloadLibraryExportName;
  public
    function LoadFile(const AFileName: string; AProperties: TStrings=nil): Boolean;
    procedure LoadByConfig(const AConfigFileName: string);
    procedure LoadDirAll(const ADirPath: string);
  end;

implementation

{TCMLibInfo}

constructor TCMLibInfo.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FProperties := TStringList.Create;
end;

destructor TCMLibInfo.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

function TCMLibInfo.GetFileName: string;
begin
  Result := FFileName;
end;

function TCMLibInfo.GetProperties: TStrings;
begin
  Result := FProperties;
end;

function TCMLibInfo.GetLoadHandle: THandle;
begin
  Result := FLoadHandle;
end;

function TCMLibInfo.GetLoadExportAddress: Pointer;
begin
  Result := FLoadExportAddress;
end;

{TCMInterfaceLoader}

constructor TCMInterfaceLoader.Create(AOwner: TComponent; ARegister: ICMInterfaceRegister);
begin
  inherited Create(AOwner);
  FInfoList := TInterfaceList.Create;
  FInterfaceRegister := ARegister;
  FLoadLibraryExportName := DefaultLoadLibraryExportName;
  FUnloadLibraryExportName := DefaultUnloadLibraryExportName;
end;

destructor TCMInterfaceLoader.Destroy;
begin
  FInfoList.Free;
  inherited Destroy;
end;

function TCMInterfaceLoader.LoadFile(const AFileName: string; AProperties: TStrings=nil): Boolean;
var
  loadExportProc: TLoadLibraryExportProc;
  libHandle: TLibHandle;
  procAddr: Pointer;
  aLibInfo: TCMLibInfo;
begin
  Result := False;
  if not Assigned(FInterfaceRegister) then
    begin
      Messager.Error('接口寄存器不存在.', [AFileName]);
      Exit;
    end;
  if not FileExistsUTF8(AFileName) then
    begin
      Messager.Error('文件:%s 不存在.', [AFileName]);
      Exit;
    end;
  //
  libHandle := LoadLibrary(UnicodeString(AFileName));
  if libHandle = 0 then
    begin
      Messager.Error('加载库文件:%s 失败。', [AFileName]);
      Exit;
    end;
  //
  procAddr := GetProcAddress(libHandle, LoadLibraryExportName);
  if procAddr = nil then
    begin
      Messager.Error('获取库文件:%s 的引出方法:%s地址失败。', [AFileName, LoadLibraryExportName]);
      Exit;
    end;
  Pointer(loadExportProc) := procAddr;
  //
  aLibInfo := TCMLibInfo.Create(AFileName);
  aLibInfo.LoadHandle := libHandle;
  if Assigned(AProperties) then
    aLibInfo.Properties.AddStrings(AProperties);
  aLibInfo.LoadExportAddress := procAddr;
  //
  Messager.Debug('行将执行库:%s 引出方法:%s()...', [AFileName, LoadLibraryExportName]);
  Result := loadExportProc(FInterfaceRegister, aLibInfo);
  if Result then
    FInfoList.Add(aLibInfo)
  else
    Messager.Warning('库文件:%s 引出方法:%s() 返回False.', [AFileName, LoadLibraryExportName]);
  Messager.Debug('库文件:%s 加载完成.', [AFileName]);
end;

procedure TCMInterfaceLoader.LoadByConfig(const AConfigFileName: string);
var
  xml: TCMDOMNodeStreamer;
  node, libNode, propertyNode: TCMDOMNode;
  libFileName: string;
  properties: TStrings;
begin
  if not FileExistsUTF8(AConfigFileName) then
    begin
      Messager.Error('配置文件:%s 不存在.', [AConfigFileName]);
      Exit;
    end;
  //
  xml := TCMDOMNodeStreamer.Create(nil);
  try
    try
      if not xml.ReadXML(node, AConfigFileName) then
        begin
          Messager.Error('读取配置文件:%s 失败.', [AConfigFileName]);
          Exit;
        end;
    except
      on e: Exception do
        begin
          Messager.Error('读取配置文件:%s 出错. %s %s', [AConfigFileName, e.ClassName, e.Message]);
          Exit;
        end;
    end;
    //
    Messager.Debug('开始加载:%s 配置的库...', [AConfigFileName]);
    properties := TStringList.Create;
    properties.NameValueSeparator := '=';
    try
      libNode := node.FirstChild;
      while Assigned(libNode) do
        begin
          libFileName := libNode.GetAttribute('filename');
          if libFileName = '' then
            begin
              Messager.Error('配置中lib节点必须要有filename属性.');
            end
          else
            begin
              libFileName := RepairLibraryFileExt(libFileName, True);
              if FileExistsUTF8(libFileName) then
                begin
                  properties.Clear;
                  propertyNode := libNode.FirstChild;
                  while Assigned(propertyNode) do
                    begin
                      properties.Add(propertyNode.Name + properties.NameValueSeparator + propertyNode.Text);
                      propertyNode := propertyNode.NextSibling;
                    end;
                  Self.LoadFile(libFileName, properties);
                end
              else
                begin
                  Messager.Error('配置中的filename:%s 不存在.', [libFileName]);
                end
            end;
          libNode := libNode.NextSibling;
        end;
     finally
       properties.Free;
     end;
     node.Free;
  finally
    xml.Free;
  end;
end;

procedure TCMInterfaceLoader.LoadDirAll(const ADirPath: string);
var
  files: TStrings;
  i: Integer;
  dirPath, libFileName: string;
begin
  if not DirPathExists(ADirPath) then
    begin
      Messager.Error('目录路径:%s 不存在.', [ADirPath]);
      Exit;
    end;
  dirPath := AppendPathDelim(ADirPath);
  try
    files := GetFileNameList(dirPath, {$IFDEF UNIX}'.so'{$ELSE}'.dll'{$ENDIF});
    try
      if files.Count <= 0 then
        begin
          Messager.Error('目录路径:%s 没有需要加载的文件...', [dirPath]);
          Exit;
        end;
      Messager.Debug('开始加载路径:%s 下的所有库文件...', [dirPath]);
      for i:=0 to files.Count-1 do
        begin
          libFileName := dirPath + files[i];
          Self.LoadFile(libFileName);
        end;
    finally
      files.Free
    end;
  except
    on e: Exception do
      begin
        Messager.Error('加载路径:%s 下的库文件时出错. %s %s', [ADirPath], e);
        Exit;
      end;
  end;
end;

end.

