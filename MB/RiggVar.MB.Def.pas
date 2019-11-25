unit RiggVar.MB.Def;

interface

uses
  System.Classes,
  System.UIConsts,
  System.UITypes,
  FMX.Graphics;

type
  TSelectedText = (
    stTop,
    stBottom
  );

  TMemeParam = (
    fpTopMargin,
    fpBottomMargin,
    fpTopSize,
    fpBottomSize,
    fpTopGlow,
    fpBottomGlow
  );

  TSampleTextProps = record
    Text: string;
    FontName: string;
    FontSize: single;
  end;

  TSampleTextItem = record
    Caption: string;
    Top: TSampleTextProps;
    Bottom: TSampleTextProps;
  end;

  { will change without notice, I am playing ... }
  ISampleTextManager = interface
  ['{B8BEB01B-AC6F-4DB2-9F5F-E0B747705F62}']
    function GetCount: Integer;
    function GetCurrentIndex: Integer;
    function GetSampleItem: TSampleTextItem;
    procedure SetUseOfficeFonts(const Value: Boolean);

    procedure Next;
    procedure Previous;
    procedure Toggle;
  end;

  IPicker = interface
['{736BA227-0249-475F-AE02-AB9F3C5EB3FB}']
    function SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
    function SelectFontFamilyName(AFontName: string): string;
    procedure CollectFontFamilyNames(ML: TStrings);
    function IsShiftKeyPressed: Boolean;
 end;

  IScreenshotSaver = interface
  ['{6A887F21-1AF1-4AA4-9B83-BC9D5136E8FF}']
    function CanSave: Boolean;
    function SaveScreenshot(ABitmap: TBitmap): Boolean;
  end;

  TScreenshotSaver = class(TInterfacedObject, IScreenshotSaver)
  public
    function CanSave: Boolean;
    function SaveScreenshot(ABitmap: TBitmap): Boolean;
  end;

const
  slb = sLineBreak;
  dlb = sLineBreak + sLineBreak;

  DarkColors: array of TAlphaColor = [
    claBlack,
    claRed,
    claCrimson,
    claBlue,
    claNavy
  ];

  LightColors: array of TAlphaColor = [
    claWhite,
    claYellow,
    claFuchsia,
    claLime,
    claCornflowerBlue,
    claPlum
  ];

  { Actions }

  faMemeNoop = 0;

  faMemeToggleEdits = 2000;
  faMemeReset = 2001;
  faMemeSwapText = 2002;

  faMemeClearImage = 2003;
  faMemeInitChecker = 2004;

  faMemeSelectTop = 2005;
  faMemeSelectBottom = 2006;

  faMemeParamTopGlow = 2007;
  faMemeParamBottomGlow = 2008;
  faMemeParamTopMargin = 2009;
  faMemeParamBottomMargin = 2010;
  faMemeParamTopSize = 2011;
  faMemeParamBottomSize = 2012;

  faMemeToggleDropTarget = 2013;
  faMemeToggleHelp = 2014;
  faMemeToggleReport = 2015;
  faMemeToggleTiling = 2016;
  faMemeToggleFontColor = 2017;
  faMemeToggleTextColor = 2018;

  faMemeFontOffice = 2019;
  faMemeFontNormal = 2020;

  faMemeCycleFontP = 2021;
  faMemeCycleFontM = 2022;

  faMemeCycleDarkColorP = 2023;
  faMemeCycleDarkColorM = 2024;

  faMemeCycleLightColorP = 2025;
  faMemeCycleLightColorM = 2026;

  faMemeAdaptFormSize = 2027;

  faMemeGotoLandscape = 2028;
  faMemeGotoSquare = 2029;
  faMemeGotoPortrait = 2030;

  faMemeFormat0 = 2031;
  faMemeFormat1 = 2032;
  faMemeFormat2 = 2033;
  faMemeFormat3 = 2034;
  faMemeFormat4 = 2035;
  faMemeFormat5 = 2036;
  faMemeFormat6 = 2037;
  faMemeFormat7 = 2038;
  faMemeFormat8 = 2039;
  faMemeFormat9 = 2040;

  faMemeSampleToggle = 2041;
  faMemeSampleNext = 2042;
  faMemeSamplePrevious = 2043;

  faMemeSample00 = 2044;
  faMemeSample01 = 2045;
  faMemeSample02 = 2046;

  faMemePickFont = 2047;
  faMemePickColor = 2048;

  faMemeSaveBitmap = 2049;
  faMemeCopyBitmap = 2050;
  faMemePasteBitmap = 2051;

implementation

{ TScreenshotSaver }

function TScreenshotSaver.CanSave: Boolean;
begin
  result := false;
end;

function TScreenshotSaver.SaveScreenshot(ABitmap: TBitmap): Boolean;
begin
  result := false;
end;

end.
