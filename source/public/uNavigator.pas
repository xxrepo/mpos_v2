unit uNavigator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces;

type

  // 导航栏的简单解决方案

  INavigatorNodeListener = interface;

  { INavigatorNodeConfig
    // 导航节点的配置
  }

  INavigationNodeConfig = interface
    ['{2AC6C25B-C10E-4384-AF28-236F96112ECF}']
    function GetName: string;  //名字应唯一
    function GetCaption: string;
    function GetCol: Integer;  //未配置时返回实现分配
    function GetRow: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetCall: string;  //保留
    function GetChildrenColWidth: Integer;
    function GetChildrenRowHeight: Integer;
    function GetChildrenColCount: Integer;
    function GetChildrenRowCount: Integer;
    function GetChildrenAlignAtGrid: Boolean;
  end;

  { INavigationNode
    // 导航节点，能够获得配置和节点信息
  }

  INavigationNode = interface
    ['{4DE9E4BC-6568-4325-875E-B848027CF0E8}']
    function GetConfig: INavigationNodeConfig;
    function GetLevel: Integer;
    function GetParentName: string;
    function GetChildrenNames: TStrings;
    procedure SetDblOpenChildren(b: Boolean); //设置双击打开子项
    function IsDblOpenChildren: Boolean;
    procedure Show;
    procedure SetListener(l: INavigatorNodeListener);
  end;

  INavigator = interface;

  { INavigatorNodeEvent }

  INavigatorNodeEvent = interface(ICMEvent)
    ['{4DE9E4BC-6568-4325-875E-B848027CF0E8}']
    function GetNavigator: INavigator;
    function GetNode: INavigationNode;
    // TODO
  end;

  { INavigatorNodeListener }

  INavigatorNodeListener = interface(ICMListener)
    ['{34312F7F-F777-4906-AA08-4D0783ACBFF4}']
    procedure Click(e: INavigatorNodeEvent);
    procedure DblClick(e: INavigatorNodeEvent);
  end;

  INavigator = interface(ICMBase)
    ['{422F5AEA-5FC0-44ED-A28E-5CD4FB2D526F}']
    function AddNode(AParentNode: INavigationNode; ANewNodeConfig: INavigationNodeConfig): Boolean;
    function RemoveNode(ANode: INavigationNode): Boolean;
    function GetNode(const ANodeName: string): INavigationNode;
    function GetRootNames: TStrings;
  end;

implementation

end.

