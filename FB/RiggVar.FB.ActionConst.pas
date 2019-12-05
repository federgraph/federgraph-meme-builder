unit RiggVar.FB.ActionConst;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

type
  TFederAction = Integer;

const

  //Pages
  faActionPageM = 103;
  faActionPageP = 104;

  //ViewFlags
  faToggleAllText = 323;
  faToggleTouchFrame = 324;

  //ColorScheme
  faCycleColorSchemeM = 653;
  faCycleColorSchemeP = 654;

  //TouchLayout
  faTouchTablet = 662;
  faTouchPhone = 663;
  faTouchDesk = 664;

  //EmptyAction
  faNoop = 871;

  faButtonFrameReport = 1070;

  //MemeBuilder
  faMemeNoop = 1071;
  faMemeToggleEdits = 1072;
  faMemeReset = 1073;
  faMemeSwapText = 1074;
  faMemeClearImage = 1075;
  faMemeInitChecker = 1076;
  faMemeSelectTop = 1077;
  faMemeSelectBottom = 1078;
  faMemeParamTopGlow = 1079;
  faMemeParamBottomGlow = 1080;
  faMemeParamTopMargin = 1081;
  faMemeParamBottomMargin = 1082;
  faMemeParamTopSize = 1083;
  faMemeParamBottomSize = 1084;
  faMemeToggleDropTarget = 1085;
  faMemeToggleHelp = 1086;
  faMemeToggleReport = 1087;
  faMemeToggleTiling = 1088;
  faMemeToggleFontColor = 1089;
  faMemeToggleTextColor = 1090;
  faMemeFontOffice = 1091;
  faMemeFontNormal = 1092;
  faMemeCycleFontP = 1093;
  faMemeCycleFontM = 1094;
  faMemeCycleDarkColorP = 1095;
  faMemeCycleDarkColorM = 1096;
  faMemeCycleLightColorP = 1097;
  faMemeCycleLightColorM = 1098;
  faMemeAdaptFormSize = 1099;
  faMemeGotoLandscape = 1100;
  faMemeGotoSquare = 1101;
  faMemeGotoPortrait = 1102;
  faMemeFormat0 = 1103;
  faMemeFormat1 = 1104;
  faMemeFormat2 = 1105;
  faMemeFormat3 = 1106;
  faMemeFormat4 = 1107;
  faMemeFormat5 = 1108;
  faMemeFormat6 = 1109;
  faMemeFormat7 = 1110;
  faMemeFormat8 = 1111;
  faMemeFormat9 = 1112;
  faMemeSampleT = 1113;
  faMemeSampleP = 1114;
  faMemeSampleM = 1115;
  faMemeSample00 = 1116;
  faMemeSample01 = 1117;
  faMemeSample02 = 1118;
  faMemePickFont = 1119;
  faMemePickColor = 1120;
  faMemeSaveBitmap = 1121;
  faMemeCopyBitmap = 1122;
  faMemePasteBitmap = 1123;
  faMemeShowColorPicker = 1124;
  faMemeShowFontPicker = 1125;

implementation

end.
