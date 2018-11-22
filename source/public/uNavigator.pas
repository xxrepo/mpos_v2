unit uNavigator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces;

type

  INavsCfg = interface
    ['{035B3A37-B02B-4336-8805-DEFD56B04BFF}']
    function GetColCount: Integer;
    function GetRowCount: Integer;
    function GetColWidth: Integer;
    function GetRowHeight: Integer;
    function GetAlign: Boolean;
  end;

  INavNodeCfg = interface
    ['{CAFC4D36-F850-402C-B9BC-043DC96A952F}']
    function GetName: string;  //名字应唯一
    function GetCaption: string;
    function GetCol: Integer;  //未配置时返回实现分配
    function GetRow: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
  end;

  INavNodeListener = interface;
  INavigator = interface;

  INavNode = interface
    ['{65BE3B79-9C92-4667-927B-2FB4DC70E26E}']
    function GetName: string;   //名字应唯一
    function GetLevel: Integer;
    function GetParentName: string;
    function GetChildrenNames: TStrings;
    procedure SetListener(l: INavNodeListener);
    function GetCfg: INavNodeCfg;
    //procedure Show;
  end;

  INavNodeEvent = interface(ICMEvent)
    ['{93409BB9-432E-4452-B9AD-6CB57218FACD}']
    function GetNavigator: INavigator;
    function GetNode: INavNode;
  end;

  INavNodeListener = interface(ICMListener)
    ['{FAF46D80-DD92-4620-9ABD-2E70DF640547}']
    procedure Opened(e: INavNodeEvent);
    //
  end;

  INavigator = interface(ICMBase)
    ['{422F5AEA-5FC0-44ED-A28E-5CD4FB2D526F}']
    function AddNode(const AParentNodeName: string; INavNodeCfg: INavNodeCfg): Boolean;
    function RemoveNode(const ANodeName: string): Boolean;
    function FindNode(const ANodeName: string): INavNode;
    function GetRootNames: TStrings;
  end;

implementation


end.

