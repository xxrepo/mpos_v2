{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_lzutils

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_lazutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8;

  function RepairUTF8Path(const AUTF8Path: string): string;
  function RepairFileUTF8Path(const AUFT8FileName: string): string;

implementation

function RepairUTF8Path(const AUTF8Path: string): string;
var
  path: string;
begin
  Result := AUTF8Path;
  path := AUTF8Path;
  ForcePathDelims(path);
  {$IFDEF UNIX}
  if (LeftStr(path, 1) <> PathDelim) and (LeftStr(path, 2) <> '..') then
    path := ExtractFilePath(SysToUTF8(paramStr(0))) + path;
  {$ELSE}
  if (Copy(path, 2, 1) <> ':') and (LeftStr(path, 2) <> '..') then
    begin
      if LeftStr(path, 1) = PathDelim then
        path := ExtractFileDir(SysToUTF8(paramStr(0))) + path
      else
        path := ExtractFilePath(SysToUTF8(paramStr(0))) + path;
    end;
  {$ENDIF}
  if RightStr(path, 1) <> PathDelim then
    path := path + PathDelim;
  Result := path;
end;

function RepairFileUTF8Path(const AUFT8FileName: string): string;
begin
  Result := RepairUTF8Path(ExtractFilePath(AUFT8FileName)) + ExtractFileName(AUFT8FileName);
end;


end.

