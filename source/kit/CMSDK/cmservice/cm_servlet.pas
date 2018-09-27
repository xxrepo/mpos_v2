{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_servlet

    This is not a complete unit, for testing

    //

    一个 servlet 可以映射多个路径，但一个路径应只能找到k唯一的 servlet。

 **********************************************************************}

unit cm_servlet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_messager, cm_parameter;

type

  EServletException = class(Exception);

  IRequestDispatcher = interface; //声明

  { IServletContext
    //Servlet 的环境对象，通过这个对象，Servlet引擎向Servlet提供环境信息。
    //是 Servlet 与 Servlet容器 之间进行通信的接口
    //Servlet 容器在启动一个应用时，会为它创建一个 ServletContext，
      同一个应用的所有 Servlet 共享一个 ServletContext; Servlet 对象们通过它们的 ServletContext 访问容器中的各种资源
  }
  IServletContext = interface(ICMBase)
    ['{401A412B-500E-40BF-8BA8-77786D9095D1}']
    function GetAttributes: ICMParameterDataList; //用于在应用范围共享数据
    function GetContext(const APath: string): IServletContext; //返回映射到另一 path 的 servlet 上下文。
    function GetContextPath: string; //当前应用的 URL 入口
    function GetInitParameters: ICMConstantParameterDataList; //返回指定上下文范围的初始化参数值。此方法与 ServletConfig 方法不一样，此方法应用于上下文中所有的参数。
    function GetServletContextName: string; //当前应用的名称，文件中的<display-name>元素值
    function GetNamedDispatcher(const AName: string): IRequestDispatcher; //返回具有指定名字或路径的 servlet 的 RequestDispatcher。
    function GetRequestDispatcher(const APath: string): IRequestDispatcher;
    function GetServerInfo: string; //Servlet引擎的名字和版本号。
  end;

  { IServletConfig
    用于保存 Servlet 初始化信息
    每一个ServletConfig对象对应着一个唯一的Servlet。
  }
  IServletConfig = interface(ICMBase)
    ['{40D96154-8019-4CB7-B477-69D801BEB1FA}']
    function GetServletName: string; //Returns the name of this servlet instance.
    function GetInitParameters: ICMConstantParameterDataList; //只应用于已编码的指定servlet。
    function GetServletContext: IServletContext;
  end;

  { IServletRequest
    封装了客户端请求的细节
    生命周期只存于容器内部
  }
  IServletRequest = interface(ICMBase)
    ['{F69A91CB-95F3-4720-9A8B-E339559A30FD}']
    function GetAttributes: ICMParameterDataList; //只存在于容器内部
    function GetParameters: ICMConstantParameterDataList;
    function GetRequestURL: string;
    function GetProtocol: string;
    function GetHost: string;
    function GetPort: Word;
    function GetContextPath: string;
    function GetServletPath: string;
    function GetRequestDispatcher(const APath: string): IRequestDispatcher;
  end;

  { IServletResponse
    一个servlet生成的结果。
  }
  IServletResponse = interface(ICMBase)
    ['{20E17C78-451E-454E-B313-2C8454174A3F}']
    function IsCommitted: Boolean;  //TODO 考虑是否需要
    procedure SetContentType(const AType: string);
    function GetContentType: string;
    function GetContent: ICMParameterDataList;
  end;

  { IRequestDispatcher
    //从客户端接收请求，然后将它发给服务器的可用资源。
    //在整个Servlet模型中用于转发和包含
  }
  IRequestDispatcher = interface(ICMBase)
    ['{79A632E9-7A8D-4522-8C4F-8AE9C90D5E4A}']
    {转发请求
      1、在目标资源中调用 forward 方法时，必须保证此响应没有提交。
      2、在 forward 语句的前后，都不应该有响应输出的语句，应该会被忽略。
    }
    procedure Forward(ARequest: IServletRequest; AResponse: IServletResponse);
    {包含响应中的内容
      被包含者不能设置共有的响应信息（否则并不会产生效果）。。
    }
    procedure Include(ARequest: IServletRequest; AResponse: IServletResponse);
  end;

  { IServlet
    //它规定了必须由 Servlet 类实现由 servlet 引擎识别和管理的方法集。
  }
  IServlet = interface(ICMBase)
    ['{7A57B067-B001-4ACD-9A75-773E0B7681D9}']
    procedure Init(AConfig: IServletConfig); //在servlet被载入后和实施服务前由servlet引擎进行1次性调用。
    function GetServletConfig: IServletConfig; //返回传递到servlet的init()方法的ServletConfig对象
    procedure Service(ARequest: IServletRequest; AResponse: IServletResponse); //处理request对象中描述的请求，使用response对象返回请求结果
    procedure Over;
    function GetServletInfo: string;
  end;

  IFilterChain = interface(ICMBase)
    ['{D15BEB93-9DCF-4B1B-8340-7D6B9D021244}']
    procedure DoFilter(ARequest: IServletRequest; AResponse: IServletResponse);
  end;

  IFilterConfig = interface(ICMBase)
    ['{37B7E54E-D0DD-41C2-B3F5-C7D0F10359D3}']
    function GetFilterName: string;
    function GetInitParameters: ICMConstantParameterDataList;
    function GetServletContext: IServletContext;
  end;

  IFilter = interface(ICMBase)
    ['{BDB777BB-82BC-4709-8ABE-F6D16B38CA71}']
    procedure Init(AConfig: IServletConfig);
    procedure DoFilter(ARequest: IServletRequest; AResponse: IServletResponse; AChain: IFilterChain);
    procedure Over;
  end;

  IListener = interface(ICMListener)
    ['{7AE370E9-8E70-4BCD-A76B-F7E5185A1709}']
  end;

  { IServletContainer
        这是一个 servlet 容器接口
    用于动态配置 servlet 应用时先行放入容器，后继可以依据配置的 code 找到相应的 servlet。
    由此，也意味着对于一个 servlet 应具有唯一的 code，这个 code 仅仅用于辨别无其他意义。
  }
  IServletCollection = interface(ICMBase)
    ['{04970409-8397-4A99-9BE6-EB46638B66BB}']
    function AddServlet(const ACode: string; AServlet: IServlet): Boolean;
    function AddFilter(const ACode: string; AFilter: IFilter): Boolean;
    function AddListener(const ACode: string; AListener: IListener): Boolean;
  end;

  { TGenericServlet
    //一种与协议无关的servlet
  }
  TGenericServlet = class(TCMMessageable, IServlet)
  private
    FConfig: IServletConfig;
  protected
    FCode: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Init; virtual; abstract; overload;
    function GetInitParameter(const AName: string): string;
    function GetInitParameterNames: TStrings;
    function GetServletContext: IServletContext;
    function GetServletName: string;
  public //IServlet
    procedure Init(AConfig: IServletConfig); overload;
    function GetServletConfig: IServletConfig;
    procedure Service(ARequest: IServletRequest; AResponse: IServletResponse); virtual;
    procedure Over; virtual;
    function GetServletInfo: string;
  end;

var
  ServletCollection: IServletCollection = nil;

implementation

{ TGenericServlet }

constructor TGenericServlet.Create;
begin
  inherited Create;
  FConfig := nil;
  FCode := Self.GetImplementorName;
end;

destructor TGenericServlet.Destroy;
begin
  Over;
  FConfig := nil;
  inherited Destroy;
end;

procedure TGenericServlet.AfterConstruction;
begin
  inherited AfterConstruction;
  if Assigned(ServletCollection) then
    ServletCollection.AddServlet(Self.FCode, Self);
end;

function TGenericServlet.GetInitParameter(const AName: string): string;
var
  sc: IServletConfig;
begin
  Result := '';
  sc := GetServletConfig;
  if Assigned(sc) then
    Result := sc.GetInitParameters.Get(AName).AsString;
end;

function TGenericServlet.GetInitParameterNames: TStrings;
var
  sc: IServletConfig;
begin
  Result := nil;
  sc := GetServletConfig;
  if Assigned(sc) then
    Result := sc.GetInitParameters.GetNames;
end;

function TGenericServlet.GetServletContext: IServletContext;
begin
  Result := GetServletConfig.GetServletContext;
end;

function TGenericServlet.GetServletName: string;
begin
  Result := GetServletConfig.GetServletName;
end;

procedure TGenericServlet.Init(AConfig: IServletConfig);
begin
  FConfig := AConfig;
  Self.Init;
end;

function TGenericServlet.GetServletConfig: IServletConfig;
begin
  Result := FConfig;
end;

procedure TGenericServlet.Service(ARequest: IServletRequest; AResponse: IServletResponse);
begin
  //
end;

procedure TGenericServlet.Over;
begin
  //
end;

function TGenericServlet.GetServletInfo: string;
begin
  Result := Format('%s.%s', [Self.UnitName, Self.ClassName]);
end;

end.

