
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


  { TACustomGrid }

  TACustomGrid = class abstract(TACustomControl)
  private
    function GetBorderColor: TColor;
    function GetCol: Integer;
    function GetColCount: Integer;
    function GetColWidths(ACol: Integer): Integer;
    function GetDefColWidth: Integer;
    function GetDefRowHeight: Integer;
    function GetFixedColor: TColor;
    function GetFixedCols: Integer;
    function GetFixedRows: Integer;
    function GetGridBorderStyle: TBorderStyle;
    function GetOptions: TGridOptions;
    function GetRow: Integer;
    function GetRowCount: Integer;
    function GetRowHeights(ARow: Integer): Integer;
    function GetScrollBars: TScrollStyle;
    function GetTitleFont: TAFont;
    procedure SetBorderColor(AValue: TColor);
    procedure SetCol(AValue: Integer);
    procedure SetColCount(AValue: Integer);
    procedure SetColWidths(ACol: Integer; AValue: Integer);
    procedure SetDefColWidth(AValue: Integer);
    procedure SetDefRowHeight(AValue: Integer);
    procedure SetFixedcolor(AValue: TColor);
    procedure SetFixedCols(AValue: Integer);
    procedure SetFixedRows(AValue: Integer);
    procedure SetGridBorderStyle(AValue: TBorderStyle);
    procedure SetOptions(AValue: TGridOptions);
    procedure SetRow(AValue: Integer);
    procedure SetRowCount(AValue: Integer);
    procedure SetRowHeights(ARow: Integer; AValue: Integer);
    procedure SetScrollBars(AValue: TScrollStyle);
    procedure SetTitleFont(AValue: TAFont);
  protected
    property BorderStyle: TBorderStyle read GetGridBorderStyle write SetGridBorderStyle;
    property BorderColor: TColor read GetBorderColor write SetBorderColor;
    property Col: Integer read GetCol write SetCol;
    property ColCount: Integer read GetColCount write SetColCount;
    property ColWidths[ACol: Integer]: Integer read GetColWidths write SetColWidths;
    property DefaultColWidth: Integer read GetDefColWidth write SetDefColWidth;
    property DefaultRowHeight: Integer read GetDefRowHeight write SetDefRowHeight;
    property FixedCols: Integer read GetFixedCols write SetFixedCols;
    property FixedRows: Integer read GetFixedRows write SetFixedRows;
    property FixedColor: TColor read GetFixedColor write SetFixedcolor;
    property Options: TGridOptions read GetOptions write SetOptions;
    property Row: Integer read GetRow write SetRow;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property RowHeights[ARow: Integer]: Integer read GetRowHeights write SetRowHeights;
    property ScrollBars: TScrollStyle read GetScrollBars write SetScrollBars;
    property TitleFont: TAFont read GetTitleFont write SetTitleFont;
  public
    function GetPeer: IACustomGridPeer;
  public
    procedure BeginUpdate;
    function  CellRect(ACol, ARow: Integer): TRect;
    procedure Clear;
    procedure EndUpdate(ARefresh: Boolean=True);
    procedure InvalidateCell(ACol, ARow: Integer);
    procedure InvalidateCol(ACol: Integer);
    procedure InvalidateRow(ARow: Integer);
  end;

  { TACustomDrawGrid }

  TACustomDrawGrid = class abstract(TACustomGrid)
  public
    function GetPeer: IACustomDrawGridPeer;
  published
    property BorderStyle;
    property BorderColor;
    property Col;
    property ColCount;
    property ColWidths;
    property DefaultColWidth;
    property DefaultRowHeight;
    property FixedCols;
    property FixedRows;
    property FixedColor;
    property Options;
    property Row;
    property RowCount;
    property RowHeights;
    property ScrollBars;
  public
    procedure AddDrawGridListener(l: IDrawGridListener);
    procedure RemoveDrawGridListener(l: IDrawGridListener);
    function GetDrawGridListeners: TDrawGridListenerList;
  end;

  { TAStringGrid }

  TAStringGrid = class(TACustomDrawGrid)
  private
    function GetCells(ACol, ARow: Integer): string;
    function GetCols(Index: Integer): TStrings;
    function GetRows(Index: Integer): TStrings;
    procedure SetCells(ACol, ARow: Integer; AValue: string);
    procedure SetCols(Index: Integer; AValue: TStrings);
    procedure SetRows(Index: Integer; AValue: TStrings);
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IAStringGridPeer;
  published
    property TitleFont;
  public
    procedure AutoSizeColumn(ACol: Integer);
    procedure AutoSizeColumns;
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property Cols[Index: Integer]: TStrings read GetCols write SetCols;
    property Rows[Index: Integer]: TStrings read GetRows write SetRows;
  end;


