unit RiggVar.FB.ActionName;

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

function GetFederActionName(fa: TFederAction): string;

implementation

function GetFederActionName(fa: TFederAction): string;
begin
  case fa of
// --- generated code snippet ---
// Not all defined actions are implemented
// in this version of the app.

{ TouchLayout }
faTouchTablet: result := 'faTouchTablet';
faTouchPhone: result := 'faTouchPhone';
faTouchDesk: result := 'faTouchDesk';

{ Pages }
faActionPageM: result := 'faActionPageM';
faActionPageP: result := 'faActionPageP';
//faActionPageE: result := 'faActionPageE';
//faActionPageS: result := 'faActionPageS';
//faActionPageX: result := 'faActionPageX';
//faActionPage1: result := 'faActionPage1';
//faActionPage2: result := 'faActionPage2';
//faActionPage3: result := 'faActionPage3';
//faActionPage4: result := 'faActionPage4';
//faActionPage5: result := 'faActionPage5';
//faActionPage6: result := 'faActionPage6';

//Forms
faShowMemo: result := 'faShowMemo';
faShowActi: result := 'faShowActi';

{ Format }
//faFormatLandscape: result := 'faFormatLandscape';
//faFormatPortrait: result := 'faFormatPortrait';
//faFormatIPhoneLandscape: result := 'faFormatIPhoneLandscape';
//faFormatIPhonePortrait: result := 'faFormatIPhonePortrait';

{ IconSize }
//faIconSize016: result := 'faIconSize016';
//faIconSize032: result := 'faIconSize032';
//faIconSize048: result := 'faIconSize048';
//faIconSize064: result := 'faIconSize064';
//faIconSize096: result := 'faIconSize096';
//faIconSize128: result := 'faIconSize128';
//faIconSize256: result := 'faIconSize256';
//faIconSize512: result := 'faIconSize512';
//faIconSize640: result := 'faIconSize640';
//faIconSize960: result := 'faIconSize960';
//faIconSize01K: result := 'faIconSize01K';

{ Wheel }
//faPlusOne: result := 'faPlusOne';
//faPlusTen: result := 'faPlusTen';
//faWheelLeft: result := 'faWheelLeft';
//faWheelRight: result := 'faWheelRight';
//faWheelDown: result := 'faWheelDown';
//faWheelUp: result := 'faWheelUp';
//faParamValuePlus1: result := 'faParamValuePlus1';
//faParamValueMinus1: result := 'faParamValueMinus1';
//faParamValuePlus10: result := 'faParamValuePlus10';
//faParamValueMinus10: result := 'faParamValueMinus10';

{ ColorScheme }
faCycleColorSchemeM: result := 'faCycleColorSchemeM';
faCycleColorSchemeP: result := 'faCycleColorSchemeP';
//faBlackText: result := 'faBlackText';
//faGrayText: result := 'faGrayText';
//faWhiteText: result := 'faWhiteText';

{ LastLine }
//faELLOn: result := 'faELLOn';
//faELLOff: result := 'faELLOff';

{ EmptyAction }
faNoop: result := 'faNoop';

{ Help }
//faCycleHelpM: result := 'faCycleHelpM';
//faCycleHelpP: result := 'faCycleHelpP';
//faHelpCycle: result := 'faHelpCycle';
//faHelpList: result := 'faHelpList';
//faHelpHome: result := 'faHelpHome';
//faToggleLanguage: result := 'faToggleLanguage';

{ ViewParams }
//faPan: result := 'faPan';
//faParamORX: result := 'faParamORX';
//faParamORY: result := 'faParamORY';
//faParamORZ: result := 'faParamORZ';
//faParamRX: result := 'faParamRX';
//faParamRY: result := 'faParamRY';
//faParamRZ: result := 'faParamRZ';
//faParamCZ: result := 'faParamCZ';

{ ViewOptions }
//faToggleMoveMode: result := 'faToggleMoveMode';
//faLinearMove: result := 'faLinearMove';
//faExpoMove: result := 'faExpoMove';

{ ViewFlags }
//faToggleBMap: result := 'faToggleBMap';
//faToggleZoom: result := 'faToggleZoom';
//faToggleMapK: result := 'faToggleMapK';
//faMapKOn: result := 'faMapKOn';
//faMapKOff: result := 'faMapKOff';
faToggleAllText: result := 'faToggleAllText';
faToggleTouchFrame: result := 'faToggleTouchFrame';
//faToggleTouchMenu: result := 'faToggleTouchMenu';
//faToggleEquationText: result := 'faToggleEquationText';
//faTogglePrimeText: result := 'faTogglePrimeText';
//faToggleSecondText: result := 'faToggleSecondText';
//faToggleLabelText: result := 'faToggleLabelText';
//faLabelBatchM: result := 'faLabelBatchM';
//faLabelBatchP: result := 'faLabelBatchP';
//faLabelTextP: result := 'faLabelTextP';
//faLabelTextM: result := 'faLabelTextM';

{ ViewType }
//faToggleViewType: result := 'faToggleViewType';
//faViewTypeOrtho: result := 'faViewTypeOrtho';
//faViewTypePerspective: result := 'faViewTypePerspective';

{ Reset }
//faReset: result := 'faReset';
//faResetPosition: result := 'faResetPosition';
//faResetRotation: result := 'faResetRotation';
//faResetZoom: result := 'faResetZoom';

{ ParamT }
//faParamT1: result := 'faParamT1';
//faParamT2: result := 'faParamT2';
//faParamT3: result := 'faParamT3';
//faParamT4: result := 'faParamT4';

{ BitmapCycle }
//faCycleBitmapM: result := 'faCycleBitmapM';
//faCycleBitmapP: result := 'faCycleBitmapP';
//faRandom: result := 'faRandom';
//faRandomWhite: result := 'faRandomWhite';
//faRandomBlack: result := 'faRandomBlack';
//faBitmapEscape: result := 'faBitmapEscape';
//faToggleContour: result := 'faToggleContour';

{ CopyPaste }
//faSave: result := 'faSave';
//faLoad: result := 'faLoad';
//faOpen: result := 'faOpen';
//faCopy: result := 'faCopy';
//faPaste: result := 'faPaste';
//faShare: result := 'faShare';

{ CopyImage }
//faCopyScreenshot: result := 'faCopyScreenshot';
//faCopyBitmap: result := 'faCopyBitmap';
//faCopyBitmap3D: result := 'faCopyBitmap3D';

{ CopyOptions }
//faToggleHardCopy: result := 'faToggleHardCopy';
//faHardCopyOn: result := 'faHardCopyOn';
//faHardCopyOff: result := 'faHardCopyOff';
//faTogglePngCopy: result := 'faTogglePngCopy';
//faPngCopyOn: result := 'faPngCopyOn';
//faPngCopyOff: result := 'faPngCopyOff';
//faToggleNoCopy: result := 'faToggleNoCopy';
//faNoCopyOn: result := 'faNoCopyOn';
//faNoCopyOff: result := 'faNoCopyOff';

{ Input }
//faToggleDropTarget: result := 'faToggleDropTarget';

{ RggControls }
//faController: result := 'faController';
//faWinkel: result := 'faWinkel';
//faVorstag: result := 'faVorstag';
//faWante: result := 'faWante';
//faWoben: result := 'faWoben';
//faSalingH: result := 'faSalingH';
//faSalingA: result := 'faSalingA';
//faSalingL: result := 'faSalingL';
//faSalingW: result := 'faSalingW';
//faMastfallF0C: result := 'faMastfallF0C';
//faMastfallF0F: result := 'faMastfallF0F';
//faMastfallVorlauf: result := 'faMastfallVorlauf';
//faBiegung: result := 'faBiegung';
//faMastfussD0X: result := 'faMastfussD0X';

{ RggFixPoints }
//faFixpointA0: result := 'faFixpointA0';
//faFixpointA: result := 'faFixpointA';
//faFixpointB0: result := 'faFixpointB0';
//faFixpointB: result := 'faFixpointB';
//faFixpointC0: result := 'faFixpointC0';
//faFixpointC: result := 'faFixpointC';
//faFixpointD0: result := 'faFixpointD0';
//faFixpointD: result := 'faFixpointD';
//faFixpointE0: result := 'faFixpointE0';
//faFixpointE: result := 'faFixpointE';
//faFixpointF0: result := 'faFixpointF0';
//faFixpointF: result := 'faFixpointF';

{ RggTrimms }
//faTrimm0: result := 'faTrimm0';
//faTrimm1: result := 'faTrimm1';
//faTrimm2: result := 'faTrimm2';
//faTrimm3: result := 'faTrimm3';
//faTrimm4: result := 'faTrimm4';
//faTrimm5: result := 'faTrimm5';
//faTrimm6: result := 'faTrimm6';
//fa420: result := 'fa420';
//faLogo: result := 'faLogo';

{ RggSalingType }
//faSalingTypOhne: result := 'faSalingTypOhne';
//faSalingTypDrehbar: result := 'faSalingTypDrehbar';
//faSalingTypFest: result := 'faSalingTypFest';

{ RggAppMode }
//faDemo: result := 'faDemo';
//faHull: result := 'faHull';

{ RggTrimmFile }
//faCopyTrimmItem: result := 'faCopyTrimmItem';
//faPasteTrimmItem: result := 'faPasteTrimmItem';
//faCopyAndPaste: result := 'faCopyAndPaste';
//faUpdateTrimm0: result := 'faUpdateTrimm0';
//faReadTrimmFile: result := 'faReadTrimmFile';
//faSaveTrimmFile: result := 'faSaveTrimmFile';
//faCopyTrimmFile: result := 'faCopyTrimmFile';

{ RggTrimmText }
//faToggleTrimmText: result := 'faToggleTrimmText';
//faToggleDiffText: result := 'faToggleDiffText';
//faToggleDataText: result := 'faToggleDataText';
//faToggleDebugText: result := 'faToggleDebugText';
//faUpdateReportText: result := 'faUpdateReportText';

{ RggViewpoint }
//faViewpointS: result := 'faViewpointS';
//faViewpointA: result := 'faViewpointA';
//faViewpointT: result := 'faViewpointT';
//faViewpoint3: result := 'faViewpoint3';

{ RggRenderOptions }
//faWantRenderH: result := 'faWantRenderH';
//faWantRenderP: result := 'faWantRenderP';
//faWantRenderF: result := 'faWantRenderF';
//faWantRenderE: result := 'faWantRenderE';
//faWantRenderS: result := 'faWantRenderS';

{ RggHullMesh }
//faHullMesh: result := 'faHullMesh';
//faHullMeshOn: result := 'faHullMeshOn';
//faHullMeshOff: result := 'faHullMeshOff';

faButtonFrameReport: result := 'faButtonFrameReport';

{ Meme }

faMemeToggleHelp: result := 'faMemeToggleHelp';
faMemeToggleReport: result := 'faMemeToggleReport';

faMemeGotoLandscape: result := 'faMemeGotoLandscape';
faMemeGotoSquare: result := 'faMemeGotoSquare';
faMemeGotoPortrait: result := 'faMemeGotoPortrait';

faMemeFormat0: result := 'faMemeFormat0';
faMemeFormat1: result := 'faMemeFormat1';
faMemeFormat2: result := 'faMemeFormat2';
faMemeFormat3: result := 'faMemeFormat3';
faMemeFormat4: result := 'faMemeFormat4';
faMemeFormat5: result := 'faMemeFormat5';
faMemeFormat6: result := 'faMemeFormat6';
faMemeFormat7: result := 'faMemeFormat7';
faMemeFormat8: result := 'faMemeFormat8';
faMemeFormat9: result := 'faMemeFormat9';

  faMemeToggleEdits: result := 'faMemeToggleEdits';
  faMemeReset: result := 'faMemeReset';
  faMemeSwapText: result := 'faMemeSwapText';
  faMemeClearImage: result := 'faMemeClearImage';
  faMemeInitChecker: result := 'faMemeInitChecker';
  faMemeSelectTop: result := 'faMemeSelectTop';
  faMemeSelectBottom: result := 'faMemeSelectBottom';
  faMemeParamTopGlow: result := 'faMemeParamTopGlow';
  faMemeParamBottomGlow: result := 'faMemeParamBottomGlow';
  faMemeParamTopMargin: result := 'faMemeParamTopMargin';
  faMemeParamBottomMargin: result := 'faMemeParamBottomMargin';
  faMemeParamTopSize: result := 'faMemeParamTopSize';
  faMemeParamBottomSize: result := 'faMemeParamBottomSize';
  faMemeToggleDropTarget: result := 'faMemeToggleDropTarget';
  faMemeToggleTiling: result := 'faMemeToggleTiling';
  faMemeToggleFontColor: result := 'faMemeToggleFontColor';
  faMemeToggleTextColor: result := 'faMemeToggleTextColor';
  faMemeFontOffice: result := 'faMemeFontOffice';
  faMemeFontNormal: result := 'faMemeFontNormal';
  faMemeCycleFontP: result := 'faMemeCycleFontP';
  faMemeCycleFontM: result := 'faMemeCycleFontM';
  faMemeCycleDarkColorP: result := 'faMemeCycleDarkColorP';
  faMemeCycleDarkColorM: result := 'faMemeCycleDarkColorM';
  faMemeCycleLightColorP: result := 'faMemeCycleLightColorP';
  faMemeCycleLightColorM: result := 'faMemeCycleLightColorM';
  faMemeAdaptFormSize: result := 'faMemeAdaptFormSize';
  faMemeSampleT: result := 'faMemeSampleT';
  faMemeSampleP: result := 'faMemeSampleP';
  faMemeSampleM: result := 'faMemeSampleM';
  faMemeSample00: result := 'faMemeSample00';
  faMemeSample01: result := 'faMemeSample01';
  faMemeSample02: result := 'faMemeSample02';
  faMemePickFont: result := 'faMemePickFont';
  faMemePickColor: result := 'faMemePickColor';
  faMemeSaveBitmap: result := 'faMemeSaveBitmap';
  faMemeCopyBitmap: result := 'faMemeCopyBitmap';
  faMemePasteBitmap: result := 'faMemePasteBitmap';
  faMemeShowColorPicker: result := 'faMemeShowColorPicker';
  faMemeShowFontPicker: result := 'faMemeShowFontPicker';

    else
      result := '??';
  end;
end;

end.
