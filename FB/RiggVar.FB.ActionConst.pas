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

  // ---

  faButtonFrameReport = 1990;

  // --- Meme ---

  faMemeNoop = 2000;

  faMemeToggleEdits = 2001;
  faMemeReset = 2002;
  faMemeSwapText = 2003;

  faMemeClearImage = 2004;
  faMemeInitChecker = 2005;

  faMemeSelectTop = 2006;
  faMemeSelectBottom = 2007;

  faMemeParamTopGlow = 2008;
  faMemeParamBottomGlow = 2009;
  faMemeParamTopMargin = 2010;
  faMemeParamBottomMargin = 2011;
  faMemeParamTopSize = 2012;
  faMemeParamBottomSize = 2013;

  faMemeToggleDropTarget = 2014;
  faMemeToggleHelp = 2015;
  faMemeToggleReport = 2016;
  faMemeToggleTiling = 2017;
  faMemeToggleFontColor = 2018;
  faMemeToggleTextColor = 2019;

  faMemeFontOffice = 2020;
  faMemeFontNormal = 2021;

  faMemeCycleFontP = 2022;
  faMemeCycleFontM = 2023;

  faMemeCycleDarkColorP = 2024;
  faMemeCycleDarkColorM = 2025;

  faMemeCycleLightColorP = 2026;
  faMemeCycleLightColorM = 2027;

  faMemeAdaptFormSize = 2028;

  faMemeGotoLandscape = 2029;
  faMemeGotoSquare = 2030;
  faMemeGotoPortrait = 2031;

  faMemeFormat0 = 2032;
  faMemeFormat1 = 2033;
  faMemeFormat2 = 2034;
  faMemeFormat3 = 2035;
  faMemeFormat4 = 2036;
  faMemeFormat5 = 2037;
  faMemeFormat6 = 2038;
  faMemeFormat7 = 2039;
  faMemeFormat8 = 2040;
  faMemeFormat9 = 2041;

  faMemeSampleT = 2042;
  faMemeSampleP = 2043;
  faMemeSampleM = 2044;

  faMemeSample00 = 2045;
  faMemeSample01 = 2046;
  faMemeSample02 = 2047;

  faMemePickFont = 2048;
  faMemePickColor = 2049;

  faMemeSaveBitmap = 2050;
  faMemeCopyBitmap = 2051;
  faMemePasteBitmap = 2052;

implementation

end.
