unit RiggVar.FB.ActionShort;

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

uses
  System.SysUtils,
  RiggVar.FB.ActionConst;

function GetFederActionShort(fa: TFederAction): string;

implementation

function GetFederActionShort(fa: TFederAction): string;
begin
  result := '??';
  case fa of
    faNoop: result := '';

    faActionPageM: result := 'P-';
    faActionPageP: result := 'P+';

    faToggleAllText: result := 'tat';
    faToggleTouchFrame: result := 'fra';

    faCycleColorSchemeM: result := 'c-';
    faCycleColorSchemeP: result := 'c+';

    faTouchTablet: result := 'tab';
    faTouchPhone: result := 'pho';
    faTouchDesk: result := 'dsk';

    faButtonFrameReport: result := 'bfr';

    // --- Meme ---

    faMemeToggleEdits: result := 'Edt';
    faMemeReset: result := 'R';
    faMemeSwapText: result := 'swp';

    faMemeClearImage: result := 'c';
    faMemeInitChecker: result := 'C';

    faMemeSelectTop: result := 't';
    faMemeSelectBottom: result := 'b';

    faMemeParamTopGlow: result := 'g';
    faMemeParamBottomGlow: result := 'G';
    faMemeParamTopMargin: result := 'm';
    faMemeParamBottomMargin: result := 'M';
    faMemeParamTopSize: result := 's';
    faMemeParamBottomSize: result := 'S';

    faMemeToggleDropTarget: result := 'dt';
    faMemeToggleHelp: result := 'h';
    faMemeToggleReport: result := 'r';
    faMemeToggleTiling: result := 'til';

    faMemeToggleFontColor: result := 'tfc';
    faMemeToggleTextColor: result := 'ttc';

    faMemeFontOffice: result := 'of';
    faMemeFontNormal: result := 'nf';

    faMemeCycleFontP: result := 'f+';
    faMemeCycleFontM: result := 'F-';

    faMemeCycleDarkColorP: result := 'dk+';
    faMemeCycleDarkColorM: result := 'dk-';

    faMemeCycleLightColorP: result := 'lc+';
    faMemeCycleLightColorM: result := 'lc-';

    faMemeAdaptFormSize: result := 'afs';

    faMemeGotoLandscape: result := '[L]';
    faMemeGotoSquare: result := '[S]';
    faMemeGotoPortrait: result := '[P]';

    faMemeFormat0: result := '[0]';
    faMemeFormat1: result := '[1]';
    faMemeFormat2: result := '[2]';
    faMemeFormat3: result := '[3]';
    faMemeFormat4: result := '[4]';
    faMemeFormat5: result := '[5]';
    faMemeFormat6: result := '[6]';
    faMemeFormat7: result := '[7]';
    faMemeFormat8: result := '[8]';
    faMemeFormat9: result := '[9]';

    faMemeSampleT: result := 'S~';
    faMemeSampleP: result := 'S+';
    faMemeSampleM: result := 'S-';

    faMemeSample00: result := '00';
    faMemeSample01: result := '01';
    faMemeSample02: result := '02';

    faMemeShowFontPicker: result := 'sF';
    faMemeShowColorPicker: result := 'sC';

    faMemePickFont: result := 'pF';
    faMemePickColor: result := 'pC';

    faMemeSaveBitmap: result := 'Sv';
    faMemeCopyBitmap: result := '^c';
    faMemePasteBitmap: result := '^v';

    faShowActi: result := 'FA';
    faShowMemo: result := 'FM';

  end;
end;

end.
