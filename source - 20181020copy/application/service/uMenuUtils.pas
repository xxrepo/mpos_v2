unit uMenuUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces,
  uMenus;

type


  { TPOSMenuItem }

  TPOSMenuItem = class(TCMBase, IPOSMenuItem)
  private
    FCaption: string;
    FCommand: string;
    FParent: IPOSMenuItem;
    FChildren: IInterfaceList;
  public
    constructor Create;
    function GetCaption: string;
    function GetCommand: string;
    function GetParent: IPOSMenuItem;
    function GetIndex: Integer;
    function Add(const ACaption, ACommand: string): IPOSMenuItem;
    function Insert(AIndex: Integer; const ACaption, ACommand: string): IPOSMenuItem;
    procedure Remove(AItem: IPOSMenuItem);
    procedure Clear;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IPOSMenuItem;
  end;

implementation

{ TPOSMenuItem }

constructor TPOSMenuItem.Create;
begin
  FChildren := TInterfaceList.Create;
end;

function TPOSMenuItem.GetCaption: string;
begin
  Result := FCaption;
end;

function TPOSMenuItem.GetCommand: string;
begin
  Result := FCommand;
end;

function TPOSMenuItem.GetParent: IPOSMenuItem;
begin
  Result := FParent;
end;

function TPOSMenuItem.GetIndex: Integer;
begin
  Result := FChildren.IndexOf(Self);
end;

function TPOSMenuItem.Add(const ACaption, ACommand: string): IPOSMenuItem;
var
  item: TPOSMenuItem;
begin
  item := TPOSMenuItem.Create;
  item.FCaption := ACaption;
  item.FCommand := ACommand;
  item.FParent := Self;
  Result := item;
  FChildren.Add(Result);
end;

function TPOSMenuItem.Insert(AIndex: Integer; const ACaption, ACommand: string): IPOSMenuItem;
var
  item: TPOSMenuItem;
begin
  item := TPOSMenuItem.Create;
  item.FCaption := ACaption;
  item.FCommand := ACommand;
  item.FParent := Self;
  Result := item;
  FChildren.Insert(AIndex, Result);
end;

procedure TPOSMenuItem.Remove(AItem: IPOSMenuItem);
begin
  FChildren.Remove(AItem);
end;

procedure TPOSMenuItem.Clear;
begin
  FChildren.Clear;
end;

function TPOSMenuItem.GetCount: Integer;
begin
  Result := FChildren.Count;
end;

function TPOSMenuItem.GetItem(AIndex: Integer): IPOSMenuItem;
begin
  Result := IPOSMenuItem(FChildren.Items[AIndex]);
end;

end.

