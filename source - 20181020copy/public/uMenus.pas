unit uMenus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces;

type

  IPOSMenuItem = interface(ICMBase)
    ['{5963F370-19FB-461B-95E2-E7198A244741}']
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

end.

