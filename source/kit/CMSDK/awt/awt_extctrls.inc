
type

  { TADateTimePicker }

  TADateTimePicker = class(TACustomControl)
  private
    function GetDate: TDate;
    function GetDateTime: TDateTime;
    function GetMaxDate: TDate;
    function GetMinDate: TDate;
    function GetTime: TTime;
    procedure SetDate(AValue: TDate);
    procedure SetDateTime(AValue: TDateTime);
    procedure SetMaxDate(AValue: TDate);
    procedure SetMinDate(AValue: TDate);
    procedure SetTime(AValue: TTime);
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IADateTimePickerPeer;
  public
    property MaxDate: TDate read GetMaxDate write SetMaxDate;
    property MinDate: TDate read GetMinDate write SetMinDate;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property Time: TTime read GetTime write SetTime;
    property Date: TDate read GetDate write SetDate;
  end;



