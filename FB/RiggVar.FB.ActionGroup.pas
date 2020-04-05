unit RiggVar.FB.ActionGroup;

(*
-
-     F            info: http://wwww.riggvar.de
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) RiggVar Software UG (haftungsbeschr�nkt)
-
*)

interface

uses
  RiggVar.FB.ActionConst;

type
  TActionGroup = array of Integer;

const

ActionGroupTouchLayout: TActionGroup = [
faTouchTablet,
faTouchPhone,
faTouchDesk];

ActionGroupPages: TActionGroup = [
faActionPageM,
faActionPageP
//faActionPageE,
//faActionPageS,
//faActionPageX,
//faActionPage1,
//faActionPage2,
//faActionPage3,
//faActionPage4,
//faActionPage5,
//faActionPage6
];

//ActionGroupInput: TActionGroup = [
//faToggleDropTarget];

ActionGroupColorScheme: TActionGroup = [
faCycleColorSchemeM,
faCycleColorSchemeP
//faBlackText,
//faGrayText,
//faWhiteText
];

//ActionGroupBitmapCycle: TActionGroup = [
//faCycleBitmapM,
//faCycleBitmapP,
//faRandom,
//faRandomWhite,
//faRandomBlack,
//faBitmapEscape,
//faToggleContour];

//--- Wheel
//ActionGroupWheel: TActionGroup = [
//faPlusOne,
//faPlusTen,

//faWheelLeft,
//faWheelRight,
//faWheelDown,
//faWheelUp,

//faParamValuePlus1, //alias faWheelUp, faPlusOne
//faParamValueMinus1, //alias faWheelDown, faPlusTen
//faParamValuePlus10, //alias faWheelRight
//faParamValueMinus10 //alias faWheelLeft
//];

//--- Forms
ActionGroupForms: TActionGroup = [
faShowActi,
faShowMemo];

//ActionGroupHelp: TActionGroup = [
//faCycleHelpM,
//faCycleHelpP,
//faHelpCycle,
//faHelpList,
//faHelpHome,
//faToggleLanguage];

//ActionGroupCopyPaste: TActionGroup = [
//faSave,
//faLoad,
//faOpen,
//faCopy,
//faPaste,
//faShare];

//ActionGroupCopyImage: TActionGroup = [
//faCopyScreenshot,
//faCopyBitmap,
//faCopyBitmap3D];

//--- Import Texture
//ActionGroupTextureImport: TActionGroup = [
//faToggleDropTarget];

//ActionGroupViewParams: TActionGroup = [
//
//faPan
//
//faParamORX,
//faParamORY,
//faParamORZ,
//
//faParamRX,
//faParamRY,
//faParamRZ,
//faParamCZ
//];

//--- Behaviour
//ActionGroupViewOptions: TActionGroup = [
//faToggleMoveMode,
//faLinearMove,
//faExpoMove];

ActionGroupViewFlags: TActionGroup = [
//faToggleBMap,
//faToggleZoom,
//faToggleMapK,
//faMapKOn,
//faMapKOff,
//
faToggleAllText,
faToggleTouchFrame
//faToggleTouchMenu,
//faToggleEquationText,
//faTogglePrimeText,
//faToggleSecondText,
//faToggleLabelText,
//
//faLabelBatchM,
//faLabelBatchP,
//faLabelTextP,
//faLabelTextM
];

//ActionGroupFormat: TActionGroup = [
//faFormatLandscape,
//faFormatPortrait,
//faFormatIPhoneLandscape,
//faFormatIPhonePortrait];

//ActionGroupIconSize: TActionGroup = [
//faIconSize016,
//faIconSize032,
//faIconSize048,
//faIconSize064,
//faIconSize096,
//faIconSize128,
//faIconSize256,
//faIconSize512,
//faIconSize640,
//faIconSize960,
//faIconSize01K];

//--- Reset
//ActionGroupReset: TActionGroup = [
//faReset,
//faResetPosition,
//faResetRotation,
//faResetZoom];

//ActionGroupCopyOptions: TActionGroup = [
//faToggleHardCopy,
//faHardCopyOn,
//faHardCopyOff,
//
//faTogglePngCopy,
//faPngCopyOn,
//faPngCopyOff,
//
//faToggleNoCopy,
//faNoCopyOn,
//faNoCopyOff];

ActionGroupEmptyAction: TActionGroup = [
  faNoop
  ];

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
