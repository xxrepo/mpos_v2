unit cm_net;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_parameter;

type

  { TURLConnection }

  TURLConnection = class
  private
    FURL: string;
    FConnectTimeout: Integer;
    FReadTimeout: Integer;
  protected
    FRequestParameters: ICMParameterDataList;
    FResponseContent: ICMConstantParameterDataList;
  public
    constructor Create(const AURL: string); virtual;
    destructor Destroy; override;
    property URL: string read FURL;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
    property RequestParameters: ICMParameterDataList read FRequestParameters;
    procedure Connect; virtual; abstract;
    property ResponseContent: ICMConstantParameterDataList read FResponseContent;
  end;

implementation

{ TURLConnection }

constructor TURLConnection.Create(const AURL: string);
begin
  FURL := AURL;
  FConnectTimeout := 0;
  FReadTimeout := 0;
  FRequestParameters := nil;
  FResponseContent := nil;
end;

destructor TURLConnection.Destroy;
begin
  FRequestParameters := nil;
  FResponseContent := nil;
  inherited Destroy;
end;


end.

