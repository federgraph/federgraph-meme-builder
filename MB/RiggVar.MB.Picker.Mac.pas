unit RiggVar.MB.Picker.Mac;

interface

{$ifdef MACOS}

uses
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.UITypes,
  System.TypInfo,
  FMX.Forms,
  FMX.Graphics,
  FMX.Platform.Mac,
  MacApi.Appkit,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  Macapi.Foundation,
  Macapi.Helpers,
  RiggVar.MB.Def;

type
  TNSFontPickerEventHandler = class(TOCLocal)
  private
    fm: NSFontManager;
    FFontFamilyName: string;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure fontChanged(Sender: Pointer); cdecl;
    property Name: string read FFontFamilyName;
  end;

  INSFontPickerEventHandler = interface(NSObject)
  ['{C0B63873-2DF0-4046-A1C8-7A5F083597FB}']
    procedure fontChanged(Sender: Pointer); cdecl;
  end;

  TNSColorPickerEventHandler = class(TOCLocal)
  private
    cp: NSColorPanel;
    FColor: TAlphaColor;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure colorChanged(Sender: Pointer); cdecl;
    property Color: TAlphaColor read FColor;
  end;

  INSColorPickerEventHandler = interface(NSObject)
  ['{12C3A1F6-410F-4303-B6FA-4FFB69D98FD3}']
    procedure colorChanged(Sender: Pointer); cdecl;
  end;

  TPickerMac = class(TInterfacedObject, IPicker)
  private
    ColorPickerShown: Boolean;
    FontPickerShown: Boolean;
    TestCounter: Integer;

    FFontPanel: NSFontPanel;
    FFontEvent: TNSFontPickerEventHandler;
    FColorPanel: NSColorPanel;
    FColorEvent: TNSColorPickerEventHandler;
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

{ TPickerMac }

procedure TPickerMac.ShowColorPicker;
begin
  if not ColorPickerShown then
  begin
    FColorPanel := TNSColorPanel.Wrap(TNSColorPanel.OCClass.sharedColorPanel);
    FColorEvent := TNSColorPickerEventHandler.Create;
    FColorEvent.cp := FColorPanel;

    FColorPanel.setTarget(FColorEvent.GetObjectID);
    FColorPanel.setAction(sel_getUid('colorChanged:'));
    ColorPickerShown := True;
  end;
  FColorPanel.orderFront(nil);
end;

function TPickerMac.SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
var
  nsc: NSColor;
  acf: TAlphaColorF;
begin
  result := AColor;

  if not ColorPickerShown then
    Exit;

  nsc := FColorPanel.color;

  acf.R := nsc.redComponent;
  acf.G := nsc.greenComponent;
  acf.B := nsc.blueComponent;
  acf.A := nsc.alphaComponent;

  result := acf.ToAlphaColor;
end;

procedure TPickerMac.ShowFontPicker;
var
  fm: NsFontManager;
  ff: NSFont;
begin
  if not FontPickerShown then
  begin
    fm := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);

    FFontEvent := TNSFontPickerEventHandler.Create;
    FFontEvent.fm := fm;

    ff := TNSFont.Wrap(TNSFont.OCClass.systemFontOfSize(13));
    fm.setSelectedFont(ff, false);
    fm.setTarget(FFontEvent.GetObjectID);
    fm.setAction(sel_getUid('fontChanged:'));

    FontPickerShown := True;
  end;

  FFontPanel := TNsFontPanel.Wrap(TNsFontPanel.OCClass.sharedFontPanel);
  FFontPanel.makeKeyAndOrderFront(nil);
end;

function TPickerMac.SelectFontFamilyName(AFontName: string): string;
var
  fm: NsFontManager;
  fn: NSString;
begin
  result := AFontName;

  if not FontPickerShown then
    Exit;

  fm := FFontEvent.fm;

  { https://developer.apple.com/documentation/appkit/nsfontmanager }
  if fm.sendAction then
  begin
    if fm.selectedFont <> nil then
    begin
      fn:= fm.selectedFont.familyName;
      result := NSStrToStr(fn);
    end;
  end
  else
  begin
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

{ TNSColorPickerEventHandler }

procedure TNSColorPickerEventHandler.colorChanged(Sender: Pointer);
var
  nsc: NSColor;
  acf: TAlphaColorF;
begin
  nsc := cp.color;

  acf.R := nsc.redComponent;
  acf.G := nsc.greenComponent;
  acf.B := nsc.blueComponent;
  acf.A := nsc.alphaComponent;

  FColor := acf.ToAlphaColor;
end;

function TNSColorPickerEventHandler.GetObjectiveCClass: PTypeInfo;
begin
  result := TypeInfo(INSColorPickerEventHandler);
end;

{ TNSFontPickerEventHandler }

procedure TNSFontPickerEventHandler.fontChanged(Sender: Pointer);
var
  nss: NSSTring;
  fn: string;
begin
  if fm.selectedFont <> nil then
  begin
    nss := fm.selectedFont.familyName;
    fn := NSStrToStr(nss);
    FFontFamilyName := fn;
  end;
end;

function TNSFontPickerEventHandler.GetObjectiveCClass: PTypeInfo;
begin
  result := TypeInfo(INSFontPickerEventHandler);
end;

{$endif}

end.
