
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

  TScrollStyle = (ssNone, ssHorizontal, ssVertical, ssBoth, ssAutoHorizontal, ssAutoVertical, ssAutoBoth);

  TMouseButton = (mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2, mbUnknown);


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


const

  VK_BACK = 8; //	08	8	Backspace键
  VK_TAB = 9; //	09	9	Tab键
  VK_RETURN = 13; //	0D	13	Enter键
  VK_SHIFT = 16; //	10	16	Shift键
  VK_CONTROL = 17; //	11	17	Ctrl键
  VK_MENU = 18; //	12	18	Alt键
  VK_PAUSE = 19; //	13	19	Pause键
  VK_CAPITAL = 20; //	14	20	Caps Lock键
  VK_ESCAPE = 27; //	1B	27	Esc键
  VK_SPACE = 32; //	20	32	Spacebar键
  VK_PRIOR = 33; //	21	33	Page Up键
  VK_NEXT = 34; //	22	34	Page Domw键
  VK_END = 35; //	23	35	End键
  VK_HOME = 36; //	24	36	Home键
  VK_LEFT = 37; //	25	37	LEFT ARROW键(←)
  VK_UP = 38; //	26	38	UP ARROW键(↑)
  VK_RIGHT = 39; //	27	39	RIGHT ARROW键(→)
  VK_DOWN = 40; //	28	40	DOWN ARROW键(↓)
  //
  VK_0 = 48; //	30	48	0键
  VK_1 = 49; //	31	49	1键
  VK_2 = 50; //	32	50	2键
  VK_3 = 51; //	33	51	3键
  VK_4 = 52; //	34	52	4键
  VK_5 = 53; //	35	53	5键
  VK_6 = 54; //	36	54	6键
  VK_7 = 55; //	37	55	7键
  VK_8 = 56; //	38	56	8键
  VK_9 = 57; //	39	57	9键
  VK_A = 65; //	41	65	A键
  VK_B = 66; //	42	66	B键
  VK_C = 67; //	43	67	C键
  VK_D = 68; //	44	68	D键
  VK_E = 69; //	45	69	E键
  VK_F = 70; //	46	70	F键
  VK_G = 71; //	47	71	G键
  VK_H = 72; //	48	72	H键
  VK_I = 73; //	49	73	I键
  VK_J = 74; //	4A	74	J键
  VK_K = 75; //	4B	75	K键
  VK_L = 76; //	4C	76	L键
  VK_M = 77; //	4D	77	M键
  VK_N = 78; //	4E	78	N键
  VK_O = 79; //	4F	79	O键
  VK_P = 80; //	50	80	P键
  VK_Q = 81; //	51	81	Q键
  VK_R = 82; //	52	82	R键
  VK_S = 83; //	53	83	S键
  VK_T = 84; //	54	84	T键
  VK_U = 85; //	55	85	U键
  VK_V = 86; //	56	86	V键
  VK_W = 87; //	57	87	W键
  VK_X = 88; //	58	88	X键
  VK_Y = 89; //	59	89	Y键
  VK_Z = 90; //	5A	90	Z键
  //
  VK_NUMPAD0 = 96; //	60	96	数字键0键
  VK_NUMPAD1 = 97; //	61	97	数字键1键
  VK_NUMPAD2 = 98; //	62	98	数字键2键
  VK_NUMPAD3 = 99; //	62	99	数字键3键
  VK_NUMPAD4 = 100; //	64	100	数字键4键
  VK_NUMPAD5 = 101; //	65	101	数字键5键
  VK_NUMPAD6 = 102; //	66	102	数字键6键
  VK_NUMPAD7 = 103; //	67	103	数字键7键
  VK_NUMPAD8 = 104; //	68	104	数字键8键
  VK_NUMPAD9 = 105; //	69	105	数字键9键
  //
  VK_F1 = 112; //	70	112	F1键
  VK_F2 = 113; //	71	113	F2键
  VK_F3 = 114; //	72	114	F3键
  VK_F4 = 115; //	73	115	F4键
  VK_F5 = 116; //	74	116	F5键
  VK_F6 = 117; //	75	117	F6键
  VK_F7 = 118; //	76	118	F7键
  VK_F8 = 119; //	77	119	F8键
  VK_F9 = 120; //	78	120	F9键
  VK_F10 = 121; //	79	121	F10键
  VK_F11 = 122; //	7A	122	F11键
  VK_F12 = 123; //	7B	123	F12键


