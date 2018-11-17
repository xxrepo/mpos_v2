
  { 这些定义是参照 LCL 的。}

type

  // for Controls
  TCaption = type String;
  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom);
  TFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin);
  TBorderStyle = bsNone..bsSingle;

  // for Graphics
  TColor = -$7FFFFFFF-1..$7FFFFFFF;
  TTextLayout = (tlTop, tlCenter, tlBottom);

  // for GraphType
  TGraphicsBevelCut = (bvNone, bvLowered, bvRaised, bvSpace);
  // for ExtCtrls
  TPanelBevel = TGraphicsBevelCut;
  TBevelWidth = 1..Maxint;

  TTabOrder = -1..32767;


const
  // standard colors
  clBlack   = TColor($000000);
  clMaroon  = TColor($000080);
  clGreen   = TColor($008000);
  clOlive   = TColor($008080);
  clNavy    = TColor($800000);
  clPurple  = TColor($800080);
  clTeal    = TColor($808000);
  clGray    = TColor($808080);
  clSilver  = TColor($C0C0C0);
  clRed     = TColor($0000FF);
  clLime    = TColor($00FF00);
  clYellow  = TColor($00FFFF);
  clBlue    = TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua    = TColor($FFFF00);
  clLtGray  = TColor($C0C0C0); // clSilver alias
  clDkGray  = TColor($808080); // clGray alias
  clWhite   = TColor($FFFFFF);
  StandardColorsCount = 16;

  // extended colors
  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue    = TColor($F0CAA6);
  clCream      = TColor($F0FBFF);
  clMedGray    = TColor($A4A0A0);
  ExtendedColorCount = 4;

  // special colors
  clNone    = TColor($1FFFFFFF);
  clDefault = TColor($20000000);


