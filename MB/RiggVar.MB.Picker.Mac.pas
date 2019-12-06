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
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure fontChanged(Sender: Pointer); cdecl;
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

    FFontManager: NSFontManager;
    FFontPanel: NSFontPanel;
    FFontEvent: TNSFontPickerEventHandler;

    FColorPanel: NSColorPanel;
    FColorEvent: TNSColorPickerEventHandler;
  public
    destructor Destroy; override;
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

function NSColorToAlphaColor(nsc: NSColor): TAlphaColor;
var
  acf: TAlphaColorF;
begin
  acf.R := nsc.redComponent;
  acf.G := nsc.greenComponent;
  acf.B := nsc.blueComponent;
  acf.A := nsc.alphaComponent;
  result := acf.ToAlphaColor;
end;

destructor TPickerMac.Destroy;
begin
  FColorEvent.Free;
  FFontEvent.Free;
  inherited;
end;

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
begin
  result := AColor;
  if ColorPickerShown then
    result := NSColorToAlphaColor(FColorPanel.color);
end;

procedure TPickerMac.ShowFontPicker;
var
  fm: NsFontManager;
  ff: NSFont;
begin
  if not FontPickerShown then
  begin
    fm := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
    FFontManager := fm;

    FFontEvent := TNSFontPickerEventHandler.Create;

    ff := TNSFont.Wrap(TNSFont.OCClass.systemFontOfSize(13));
    fm.setSelectedFont(ff, false);
    fm.setTarget(FFontEvent.GetObjectID);
    fm.setAction(sel_getUid('fontChanged:'));

    FFontPanel := TNsFontPanel.Wrap(TNsFontPanel.OCClass.sharedFontPanel);

    FontPickerShown := True;
  end;

  FFontPanel.makeKeyAndOrderFront(nil);
end;

function TPickerMac.SelectFontFamilyName(AFontName: string): string;
var
  fn: NSString;
begin
  result := AFontName;

  if not FontPickerShown then
    Exit;

  { https://developer.apple.com/documentation/appkit/nsfontmanager }
  if FFontManager.sendAction then
  begin
    if FFontManager.selectedFont <> nil then
    begin
      fn:= FFontManager.selectedFont.familyName;
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
begin
  FColor := NSColorToAlphaColor(cp.color);
end;

function TNSColorPickerEventHandler.GetObjectiveCClass: PTypeInfo;
begin
  result := TypeInfo(INSColorPickerEventHandler);
end;

{ TNSFontPickerEventHandler }

procedure TNSFontPickerEventHandler.fontChanged(Sender: Pointer);
begin
end;

function TNSFontPickerEventHandler.GetObjectiveCClass: PTypeInfo;
begin
  result := TypeInfo(INSFontPickerEventHandler);
end;

{$endif}

end.
