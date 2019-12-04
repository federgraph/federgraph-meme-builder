unit RiggVar.MB.Picker.Mac;

interface

{$ifdef MACOS}

uses
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.UITypes,
  FMX.Forms,
  RiggVar.MB.Def;

type
  TPickerMac = class(TInterfacedObject, IPicker)
  public
    function SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
    function SelectFontFamilyName(AFontName: string): string;
    procedure CollectFontFamilyNames(ML: TStrings);
    function IsShiftKeyPressed: Boolean;
 end;

{$endif}

implementation

{$ifdef MACOS}

{ RSP-16220 code navigation is blocked inside MACOS blocks.
  improvement in 10.3.3 - now sometimes it works and sometimes not. }

uses
  FMX.Graphics,
  MacApi.Appkit,
  Macapi.CoreFoundation,
  Macapi.Foundation;

{ TPickerMac }

function TPickerMac.SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
var
  cp: NSColorPanel;
//  nsc: NSColor;
//  acf: TAlphaColorF;
begin
  result := AColor;
  try
    cp := TNsColorPanel.Wrap(TNsColorPanel.OCClass.sharedColorPanel);
    cp.orderFront(nil);

//    nsc := cp.color;
//    acf.R := nsc.redComponent;
//    acf.G := nsc.greenComponent;
//    acf.B := nsc.blueComponent;
//    acf.A := nsc.alphaComponent;
//    result := acf.ToAlphaColor;
  except
  end;
end;

function TPickerMac.SelectFontFamilyName(AFontName: string): string;
var
  fm: NsFontManager;
  fp: NSFontPanel;
//  fn: NSString;
begin
  result := AFontName;
  try
    fm := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
//    fm.setDelegate(PickerDelegate);

    { How do I set a delegate with the changeFont method that works ? }

    fp := TNsFontPanel.Wrap(TNsFontPanel.OCClass.sharedFontPanel);
    fp.orderFront(nil);

    { This should be in the delegate method ? }
//    fn:= fm.selectedFont.familyName;
//    result := string(fn.UTF8String);

//    fm.setDelegate(nil);
  except
    result := AFontName;
  end;
end;

procedure TPickerMac.CollectFontFamilyNames(ML: TStrings);
var
  fm: NsFontManager;
  list:NSArray;
  lItem:NSString;
  i: Integer;
begin
// stackoverflow: how-to-get-the-list-of-fonts-available-delphi-xe3-firemonkey-2

  fm := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
  list := fm.availableFontFamilies;
  if (List <> nil) and (List.count > 0) then
  begin
    for i := 0 to List.Count-1 do
    begin
      lItem := TNSString.Wrap(List.objectAtIndex(i));
      ML.Add(string(lItem.UTF8String))
    end;
  end;
end;

function TPickerMac.IsShiftKeyPressed: Boolean;
begin
{ not yet tested on MACOS at all }
  result := NSShiftKeyMask and TNSEvent.OCClass.modifierFlags = NSShiftKeyMask;
end;

{$endif}

end.
