unit cm_fileutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, strutils, LazUTF8, LazFileUtils,
  md5;


  function FileHexMd5(const AFileName: string): string;
  function CloneFile(const ASrcFileName, ADestFileName: string): Boolean;
  procedure WriteStringFile(const AStr, AFileName: string);
  function GetFileNameList(const APath, AExt: string): TStrings;

implementation


function FileHexMd5(const AFileName: string): string;
begin
  Result := MD5Print(MD5File(UTF8ToSys(AFileName)));
end;

function CloneFile(const ASrcFileName, ADestFileName: string): Boolean;
begin
  Result := False;
  if FileExistsUTF8(ASrcFileName) then
    begin
      if not FileExistsUTF8(ADestFileName) then
        begin
          Result := CopyFile(ASrcFileName, ADestFileName, [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime], True);
        end
      else if not SameText(FileHexMd5(ASrcFileName), FileHexMd5(ADestFileName)) then
        begin
          if DeleteFile(ADestFileName) then
            Result := CopyFile(ASrcFileName, ADestFileName, [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime], True);
        end
      else
        begin
          Result := True;
        end;
    end;
end;

procedure WriteStringFile(const AStr, AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    fs.WriteBuffer(Pointer(AStr)^, Length(AStr));
  finally
    fs.Free;
  end;
end;

function GetFileNameList(const APath, AExt: string): TStrings;
var
  ext: string;
  SearchRec: TSearchRec;
  found: Integer;
begin
  Result := TStringList.Create;
  if not DirectoryExistsUTF8(APath) then
    Exit;
  if Copy(AExt, 1, 1) <> '.' then
    ext := '.' + AExt
  else
    ext := AExt;
  //
  found := FindFirst(APath + '*' + ext, faAnyFile, SearchRec);
  try
    while found = 0 do
      begin
        if (not AnsiStartsText('.', SearchRec.Name))
           {$IFDEF UNIX}
           and (not (SearchRec.Attr in [$30, $32]))
           {$ELSE}
           and (SearchRec.Attr <> faDirectory)
           {$ENDIF}
        then
          begin
            Result.Add(SearchRec.Name);
          end;
        found := FindNext(SearchRec);
      end;
  finally
    FindClose(SearchRec);
  end;
end;

end.

