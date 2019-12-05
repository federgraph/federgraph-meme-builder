unit RiggVar.MB.Picker.Mac;

interface

{$ifdef MACOS}

uses
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.UITypes,
  FMX.Forms,
  MacApi.Appkit,
  Macapi.Foundation,
  RiggVar.MB.Def;

type
  TPickerMac = class(TInterfacedObject, IPicker)
  private
    FF: NSFont;
    ColorPickerShown: Boolean;
    FontPickerShown: Boolean;
    TestCounter: Integer;
    procedure InitFont;
    procedure InitFontManager;
  public
    procedure ShowColorPicker;
    procedure ShowFontPicker;
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
  Macapi.Helpers,
  FMX.Platform.Mac,
  FMX.Graphics;

{ TPickerMac }

procedure TPickerMac.ShowColorPicker;
var
  cp: NSColorPanel;
begin
  cp := TNsColorPanel.Wrap(TNsColorPanel.OCClass.sharedColorPanel);
  cp.makeKeyAndOrderFront(self);
  ColorPickerShown := True;
end;

function TPickerMac.SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
var
  cp: NSColorPanel;
  nsc: NSColor;
  acf: TAlphaColorF;
begin
  result := AColor;

  if not ColorPickerShown then
    Exit;

  cp := TNsColorPanel.Wrap(TNsColorPanel.OCClass.sharedColorPanel);

  if cp = nil then
    Exit;

  nsc := cp.color;

  acf.R := nsc.redComponent;
  acf.G := nsc.greenComponent;
  acf.B := nsc.blueComponent;
  acf.A := nsc.alphaComponent;

  result := acf.ToAlphaColor;
end;

procedure TPickerMac.ShowFontPicker;
var
//  fm: NsFontManager;
  fp: NSFontPanel;
begin
  InitFontManager; // optional ?

  { open Panel via FontManager }
//  fm := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
//  fm.orderFrontFontPanel(nil);

  { or open FontPanel directly }
  fp := TNsFontPanel.Wrap(TNsFontPanel.OCClass.sharedFontPanel);
  fp.makeKeyAndOrderFront(nil); //(self);

  FontPickerShown := True;
end;

function TPickerMac.SelectFontFamilyName(AFontName: string): string;
var
  fm: NsFontManager;
  fn: NSString;
begin
  result := AFontName;

  if not FontPickerShown then
    Exit;

  if FF = nil then
    InitFont;

  fm := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);

// https://developer.apple.com/documentation/appkit/nsfontmanager

  if True then //if fm.sendAction then
  begin
    { convertFont return same font }
    FF := fm.convertFont(FF);
    fn:= FF.familyName;
    result := NSStrToStr(fn);
    { result := '.AppleSystemUIFont'; }
  end
  else
  begin
    { changeFont action was not called }
    Inc(TestCounter);
  end;
end;

procedure TPickerMac.CollectFontFamilyNames(ML: TStrings);
var
  fm: NsFontManager;
  list:NSArray;
  lItem:NSString;
  i: Integer;
begin
  fm := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
  list := fm.availableFontFamilies;
  if (List <> nil) and (List.count > 0) then
  begin
    for i := 0 to List.Count-1 do
    begin
      lItem := TNSString.Wrap(List.objectAtIndex(i));
      ML.Add(NSStrToStr(lItem));
    end;
  end;
end;

function TPickerMac.IsShiftKeyPressed: Boolean;
begin
  result := NSShiftKeyMask and TNSEvent.OCClass.modifierFlags = NSShiftKeyMask;
end;

procedure TPickerMac.InitFont;
begin
  FF := TNSFont.Wrap(TNSFont.OCClass.systemFontOfSize(13));
end;

procedure TPickerMac.InitFontManager;
var
  fm: NsFontManager;
begin
  fm := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
  if FF = nil then
  begin
    InitFont;
    fm.setSelectedFont(FF, false);
  end;
end;

{$endif}

end.
