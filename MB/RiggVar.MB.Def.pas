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
    procedure ShowColorPicker;
    procedure ShowFontPicker;
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
