unit cm_tree;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  cm_generics;

type

  TCMTreeNode<T> = class
  private

  public
    constructor Create(AObj: T; AParentNode: TCMDOMNode=nil);
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;
    property
    //
    property Level: Integer read FLevel;
    //property Data: Pointer read FData write FData;
    //
    property ParentNode: TCMDOMNode read FParentNode;
    function FindNode(const ANodeName: string): TCMDOMNode;
    function FindChildNode(const ANodeName: string): TCMDOMNode;
    function HasChildNodes: Boolean;
    property ChildCount: Integer read GetChildCount;
    property FirstChild: TCMDOMNode read GetFirstChild;
    property LastChild: TCMDOMNode read GetLastChild;
    property ChildNodes: TCMDOMNodeList read FChildNodeList;
    function AppendChild(NewChild: TCMDOMNode): Integer;
    function RemoveChild(OldChild: TCMDOMNode): Integer; overload;
    function RemoveChild(const OldChildName: string): Integer; overload;
    //
    property PreviousSibling: TCMDOMNode read GetPreviousSibling;
    property NextSibling: TCMDOMNode read GetNextSibling;
  end;

implementation

end.

