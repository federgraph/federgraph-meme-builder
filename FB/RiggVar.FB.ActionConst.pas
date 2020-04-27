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

// --- generated code snippet ---
// Note that some of the defined actions
//   may not be implemented in this version of the app.

const

  { EmptyAction }
  faNoop = 0;

  { Pages }
  faActionPageM = 1;
  faActionPageP = 2;

  { Forms }
  faShowActi = 3;
  faShowMemo = 4;

  { TouchLayout }
  faTouchTablet = 5;
  faTouchPhone = 6;
  faTouchDesk = 7;

  { Format }
  faMemeGotoLandscape = 8;
  faMemeGotoSquare = 9;
  faMemeGotoPortrait = 10;
  faMemeFormat0 = 11;
  faMemeFormat1 = 12;
  faMemeFormat2 = 13;
  faMemeFormat3 = 14;
  faMemeFormat4 = 15;
  faMemeFormat5 = 16;
  faMemeFormat6 = 17;
  faMemeFormat7 = 18;
  faMemeFormat8 = 19;
  faMemeFormat9 = 20;

  { ColorScheme }
  faCycleColorSchemeM = 21;
  faCycleColorSchemeP = 22;

  { ViewFlags }
  faToggleAllText = 23;
  faToggleTouchFrame = 24;

  { MemeParams }
  faMemeParamTopGlow = 25;
  faMemeParamBottomGlow = 26;
  faMemeParamTopMargin = 27;
  faMemeParamBottomMargin = 28;
  faMemeParamTopSize = 29;
  faMemeParamBottomSize = 30;

  { MemeSelect }
  faMemeSelectTop = 31;
  faMemeSelectBottom = 32;

  { MemeOption }
  faMemeToggleFontColor = 33;
  faMemeToggleTextColor = 34;
  faMemeFontOffice = 35;
  faMemeFontNormal = 36;
  faMemeCycleFontP = 37;
  faMemeCycleFontM = 38;

  { MemeColor }
  faMemeCycleDarkColorP = 39;
  faMemeCycleDarkColorM = 40;
  faMemeCycleLightColorP = 41;
  faMemeCycleLightColorM = 42;

  { MemeSapmples }
  faMemeSampleT = 43;
  faMemeSampleP = 44;
  faMemeSampleM = 45;
  faMemeSample00 = 46;
  faMemeSample01 = 47;
  faMemeSample02 = 48;

  { MemePicker }
  faMemePickFont = 49;
  faMemePickColor = 50;
  faMemeShowColorPicker = 51;
  faMemeShowFontPicker = 52;

  { MemeIO }
  faMemeSaveBitmap = 53;
  faMemeCopyBitmap = 54;
  faMemePasteBitmap = 55;

  { MemeReport }
  faMemeToggleHelp = 56;
  faMemeToggleReport = 57;
  faButtonFrameReport = 58;

  { MemeImage }
  faMemeClearImage = 59;
  faMemeInitChecker = 60;
  faMemeToggleTiling = 61;

  { Sonstiges }
  faMemeReset = 62;
  faMemeAdaptFormSize = 63;
  faMemeSwapText = 64;
  faMemeToggleDropTarget = 65;
  faMemeToggleEdits = 66;

  faMax = 67;

implementation

end.
