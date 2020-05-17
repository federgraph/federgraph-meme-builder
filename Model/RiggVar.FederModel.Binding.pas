unit RiggVar.FederModel.Binding;

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
  Classes;

type
  TFederBinding = class
  public
    procedure InitInfoText(SL: TStrings);
    procedure InitHelpText(SL: TStrings);
    procedure InitShortcutKeys(ML: TStrings);
  end;

implementation

procedure TFederBinding.InitInfoText(SL: TStrings);
begin
  SL.Add('For info about MB (Meme Builder)');
  SL.Add('');
  SL.Add('  visit Website or code at GitHub');
  SL.Add('');
  SL.Add('-');
  SL.Add('-     F');
  SL.Add('-    * * *');
  SL.Add('-   *   *   G');
  SL.Add('-  *     * *   *');
  SL.Add('- E - - - H - - - I');
  SL.Add('-  *     * *         *');
  SL.Add('-   *   *   *           *');
  SL.Add('-    * *     *             *');
  SL.Add('-     D-------A---------------B');
  SL.Add('-              *');
  SL.Add('-              (C) federgraph.de');
  SL.Add('-');
  SL.Add('  github.com/federgraph/federgraph-meme-builder')
end;

procedure TFederBinding.InitHelpText(SL: TStrings);
begin
  SL.Add('Help Text');
  SL.Add('=========');
  SL.Add('');
  SL.Add('--- manually written key binding report ---');
  SL.Add('');
  InitShortcutKeys(SL);
end;

procedure TFederBinding.InitShortcutKeys(ML: TStrings);
begin
  ML.Add('    = : fa := faMemeSample00;');
  ML.Add('    ? : fa := faMemeSample01;');
  ML.Add('    ! : fa := faMemeSample02;');
  ML.Add('');
  ML.Add('    a : fa := faMemeAdaptFormSize;');
  ML.Add('');
  ML.Add('    b : fa := faMemeSelectBottom;');
  ML.Add('    B : fa := faButtonFrameReport;');
  ML.Add('');
  ML.Add('    c : fa := faMemeClearImage;');
  ML.Add('    C : fa := faMemeInitChecker;');
  ML.Add('');
  ML.Add('    d : fa := faMemeToggleDropTarget;');
  ML.Add('');
  ML.Add('    f : fa := faMemeCycleFontP;');
  ML.Add('    F : fa := faMemeCycleFontM;');
  ML.Add('');
  ML.Add('    g : fa := faMemeParamTopGlow;');
  ML.Add('    G : fa := faMemeParamBottomGlow;');
  ML.Add('');
  ML.Add('    h : fa := faMemeToggleHelp;');
  ML.Add('');
  ML.Add('    i : fa := faMemeCycleLightColorP;');
  ML.Add('    I : fa := faMemeCycleLightColorM;');
  ML.Add('');
  ML.Add('    j : fa := faMemeCycleDarkColorP;');
  ML.Add('    J : fa := faMemeCycleDarkColorM;');
  ML.Add('');
  ML.Add('    k : fa := faCycleColorSchemeP;');
  ML.Add('    K : fa := faCycleColorSchemeM;');
  ML.Add('');
  ML.Add('    l : fa := faMemeGotoLandscape;');
  ML.Add('');
  ML.Add('    m : fa := faMemeParamTopMargin;');
  ML.Add('    M : fa := faMemeParamBottomMargin;');
  ML.Add('');
  ML.Add('    o : fa := faMemeFontOffice;');
  ML.Add('    O : fa := faMemeFontNormal;');
  ML.Add('');
  ML.Add('    p : fa := faMemeGotoPortrait;');
  ML.Add('');
  ML.Add('    q : fa := faMemeGotoSquare;');
  ML.Add('');
  ML.Add('    r : fa := faMemeToggleReport;');
  ML.Add('    R : fa := faMemeReset;');
  ML.Add('');
  ML.Add('    s : fa := faMemeParamTopSize;');
  ML.Add('    S : fa := faMemeParamBottomSize;');
  ML.Add('');
  ML.Add('    t : fa := faMemeSelectTop;');
  ML.Add('');
  ML.Add('    u : fa := faMemeToggleTiling;');
  ML.Add('');
  ML.Add('    v : fa := faMemeToggleFontColor;');
  ML.Add('    V : fa := faMemeToggleTextColor;');
  ML.Add('');
  ML.Add('    w: fa := faToggleTouchFrame;');
  ML.Add('');
  ML.Add('    x : fa := faMemeSampleT;');
  ML.Add('');
  ML.Add('    y : fa := faMemeSampleP;');
  ML.Add('    Y : fa := faMemeSampleM;');
  ML.Add('');
  ML.Add('    Z : fa := faMemeSwapText;');
  ML.Add('');
  ML.Add('    0 : fa := faMemeFormat0;');
  ML.Add('    1 : fa := faMemeFormat1;');
  ML.Add('    2 : fa := faMemeFormat2;');
  ML.Add('    3 : fa := faMemeFormat3;');
  ML.Add('    4 : fa := faMemeFormat4;');
  ML.Add('    5 : fa := faMemeFormat5;');
  ML.Add('    6 : fa := faMemeFormat6;');
  ML.Add('    7 : fa := faMemeFormat7;');
  ML.Add('    8 : fa := faMemeFormat8;');
  ML.Add('    9 : fa := faMemeFormat9;');
  ML.Add('');
  ML.Add('    ä : fa := faMemePickFont;');
  ML.Add('    Ä : fa := faMemePickColor;');
  ML.Add('');
  ML.Add('    ö : fa := faMemeShowColorPicker;');
  ML.Add('    Ö : fa := faMemeShowFontPicker;');
  ML.Add('');
  ML.Add('    + : fa := faActionPageP;');
  ML.Add('    * : fa := faActionPageM;');
  ML.Add('');
  ML.Add(' vkEscape : fa := faMemeToggleEdits;');
  ML.Add(' vkF12    : fa := faMemeSaveBitmap;');
  ML.Add(' vkC      : fa := faMemeCopyBitmap;');
  ML.Add(' vkV      : fa := faMemePasteBitmap;');
end;

(*
--- generated key binding report ---


061 = faActionPageE

*)

end.
