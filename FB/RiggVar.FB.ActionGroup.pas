unit RiggVar.FB.ActionGroup;

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
  RiggVar.FB.ActionConst;

type
  TActionGroup = array of Integer;

const

ActionGroupEmptyAction: TActionGroup = [
faNoop];

ActionGroupTouchLayout: TActionGroup = [
faTouchTablet,
faTouchPhone,
faTouchDesk];

ActionGroupPages: TActionGroup = [
  faActionPageM,
  faActionPageP];

ActionGroupColorScheme: TActionGroup = [
  faCycleColorSchemeM,
  faCycleColorSchemeP];

ActionGroupForms: TActionGroup = [
faShowActi,
faShowMemo];

ActionGroupViewFlags: TActionGroup = [
  faToggleAllText,
  faToggleTouchFrame];

ActionGroupMemeFormat: TActionGroup = [
  faMemeGotoLandscape,
  faMemeGotoSquare,
  faMemeGotoPortrait,
  faMemeFormat0,
  faMemeFormat1,
  faMemeFormat2,
  faMemeFormat3,
  faMemeFormat4,
  faMemeFormat5,
  faMemeFormat6,
  faMemeFormat7,
  faMemeFormat8,
  faMemeFormat9];

ActionGroupMemeParams: TActionGroup = [
  faMemeParamTopGlow,
  faMemeParamBottomGlow,
  faMemeParamTopMargin,
  faMemeParamBottomMargin,
  faMemeParamTopSize,
  faMemeParamBottomSize];

ActionGroupMemeSelect: TActionGroup = [
  faMemeSelectTop,
  faMemeSelectBottom];

ActionGroupMemeOptions: TActionGroup = [
  faMemeToggleFontColor,
  faMemeToggleTextColor,
  faMemeFontOffice,
  faMemeFontNormal,
  faMemeCycleFontP,
  faMemeCycleFontM];

ActionGroupMemeColor: TActionGroup = [
  faMemeCycleDarkColorP,
  faMemeCycleDarkColorM,
  faMemeCycleLightColorP,
  faMemeCycleLightColorM];

ActionGroupMemeSamples: TActionGroup = [
  faMemeSampleT,
  faMemeSampleP,
  faMemeSampleM,
  faMemeSample00,
  faMemeSample01,
  faMemeSample02];

ActionGroupMemePicker: TActionGroup = [
  faMemePickFont,
  faMemePickColor,
  faMemeShowColorPicker,
  faMemeShowFontPicker];

ActionGroupMemeIO: TActionGroup = [
  faMemeSaveBitmap,
  faMemeCopyBitmap,
  faMemePasteBitmap];

ActionGroupMemoReport: TActionGroup = [
  faMemeToggleHelp,
  faMemeToggleReport,
  faButtonFrameReport];

ActionGroupSonstiges: TActionGroup = [
  faMemeReset,
  faMemeAdaptFormSize,
  faMemeSwapText,
  faMemeToggleDropTarget,
  faMemeToggleEdits];

ActionGroupMemoImage: TActionGroup = [
  faMemeClearImage,
  faMemeInitChecker,
  faMemeToggleTiling];

implementation

end.
