unit RiggVar.FB.ActionLong;

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

function GetFederActionLong(fa: TFederAction): string;

implementation

function GetFederActionLong(fa: TFederAction): string;
begin
  result := '??';
  case fa of
    faNoop: result := 'Noop';

    faActionPageM: result := 'Action Page -';
    faActionPageP: result := 'Action Page +';

    faToggleAllText: result := 'Toggle all text';
    faToggleTouchFrame: result := 'Touch frame';

    faCycleColorSchemeM: result := 'cycle 3D color scheme +';
    faCycleColorSchemeP: result := 'cycle 3D color scheme +';

    faTouchTablet: result := 'Touch Tablet';
    faTouchPhone: result := 'Touch Phone';
    faTouchDesk: result := 'Touch Desk';

    // --- Meme ---

    faMemeNoop: result := 'Meme Noop';

    faMemeToggleEdits: result := 'Toggle Edits';
    faMemeReset: result := 'Reset';
    faMemeSwapText: result := 'Swap Text';

    faMemeClearImage: result := 'Clear Image';
    faMemeInitChecker: result := 'Init Checker';

    faMemeSelectTop: result := 'Select Top Text';
    faMemeSelectBottom: result := 'Select Bottom Text';

    faMemeParamTopGlow: result := 'Top Glow Softness';
    faMemeParamBottomGlow: result := 'Bottom Glow Softness';
    faMemeParamTopMargin: result := 'Top Text Margin';
    faMemeParamBottomMargin: result := 'Bottom Text Margin';
    faMemeParamTopSize: result := 'Top Font Size';
    faMemeParamBottomSize: result := 'Bottom Font Size';

    faMemeToggleDropTarget: result := 'Toggle Drop Target';
    faMemeToggleHelp: result := 'Toggle Help Text';
    faMemeToggleReport: result := 'Toggle Report';
    faMemeToggleTiling: result := 'Toggle Tiling Mode';
    faMemeToggleFontColor: result := 'Toggle Font Color Black/White';
    faMemeToggleTextColor: result := 'Toggle Text Color Black/White';

    faMemeFontOffice: result := 'Try Use Office Fonts';
    faMemeFontNormal: result := 'Use Normal Fonts';

    faMemeCycleFontP: result := 'Cycle Font Family Plus';
    faMemeCycleFontM: result := 'Cycle Font Family Minus';

    faMemeCycleDarkColorP: result := 'Cycle Dark Color Plus';
    faMemeCycleDarkColorM: result := 'Cycle Dark Color Minus';

    faMemeCycleLightColorP: result := 'Cycle Light Color Plus';
    faMemeCycleLightColorM: result := 'Cycle Light Color Minus';

    faMemeAdaptFormSize: result := 'Adapt Form Size';

    faMemeGotoLandscape: result := 'Goto Landscape';
    faMemeGotoSquare: result := 'Goto Square';
    faMemeGotoPortrait: result := 'Goto Portrait';

    faMemeFormat0: result := 'Format 0';
    faMemeFormat1: result := 'Format 1';
    faMemeFormat2: result := 'Format 2';
    faMemeFormat3: result := 'Format 3';
    faMemeFormat4: result := 'Format 4';
    faMemeFormat5: result := 'Format 5';
    faMemeFormat6: result := 'Format 6';
    faMemeFormat7: result := 'Format 7';
    faMemeFormat8: result := 'Format 8';
    faMemeFormat9: result := 'Format 9';

    faMemeSampleT: result := 'Sample Item Toggle (Index 0/1)';
    faMemeSampleP: result := 'Cycle Sample Items Plus';
    faMemeSampleM: result := 'Cycle Sample Items Minus';

    faMemeSample00: result := 'Select Sample Bundle 00';
    faMemeSample01: result := 'Select Sample Bundle 01';
    faMemeSample02: result := 'Select Sample Bundle 02';

    faMemePickFont: result := 'Pick Font via Dialog';
    faMemePickColor: result := 'Pick Font Color via Dialog';

    faMemeSaveBitmap: result := 'Save TBitmap';
    faMemeCopyBitmap: result := 'Copy TBitmap';
    faMemePasteBitmap: result := 'Paste TBitmap';

  end;
end;

end.
