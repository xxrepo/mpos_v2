unit uDBInitialize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces;

type
  IDBInitialize = interface(ICMBase)
    ['{C5A0BB9F-F271-4033-B295-3BE23FA657BB}']
    function DBInitialize: boolean;
  end;

implementation

end.

