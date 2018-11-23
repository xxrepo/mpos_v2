unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_AWT;

type

  IMain = interface(ICMBase)
    ['{2FB3BEDE-1CBE-4084-A670-EB253A36882B}']
    function GetNavigation: TAPanel;
    function GetStatus: TAPanel;
  end;

implementation

end.

