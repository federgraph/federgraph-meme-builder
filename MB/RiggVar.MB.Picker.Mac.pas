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
    FColor: TAlphaColor;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure colorChanged(Sender: Pointer); cdecl;
    property Color: TAlphaColor read FColor write FColor;
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

    FontEventHandler: TNSFontPickerEventHandler;
    ColorEventHandler: TNSColorPickerEventHandler;
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

{ I believe that you should never store a retrieved interface reference
  to the shared objects, because it can change, and then there may be problems.

  For example, when the user changes the panel tab in the dialog,
    which is perfectly ok,
      the FontPanel seems to be recreated ... ?
        this is just a conspiracy theory,
          but I got exceptions, including a stacktrace, go figure ...
            and it makes me believe that I should try not to cache the ref.

  Maybe, when the panel type changes, the system will send other messages,
    which I do not implement, and it crashes because of that ?

  Needs testing, I have tested the NORMAL case, and it works.
  ( Using latest Software versions only, 07.12.2019. )
}

function GetNSColorPanel: NSColorPanel;
begin
  result := TNSColorPanel.Wrap(TNSColorPanel.OCClass.sharedColorPanel);
end;

function GetNSFontManager: NSFontManager;
begin
  result := TNSFontManager.Wrap(TNSFontManager.OCClass.sharedFontManager);
end;

function GetNSFontPanel: NSFontPanel;
begin
  result := TNSFontPanel.Wrap(TNSFontPanel.OCClass.sharedFontPanel);
end;

function GetNSFontDefault: NSFont;
begin
  result := TNSFont.Wrap(TNSFont.OCClass.systemFontOfSize(13));
end;

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

{ TPickerMac }

destructor TPickerMac.Destroy;
begin
  ColorEventHandler.Free;
  FontEventHandler.Free;
  inherited;
end;

procedure TPickerMac.ShowColorPicker;
var
  cp: NSColorPanel;
begin
  if not ColorPickerShown then
  begin
    cp := GetNSColorPanel;
    ColorEventHandler := TNSColorPickerEventHandler.Create;
    ColorEventHandler.FColor := claWhite;

    cp.setTarget(ColorEventHandler.GetObjectID);
    cp.setAction(sel_getUid('colorChanged:'));
    ColorPickerShown := True;
  end;
  cp.orderFront(nil);
end;

function TPickerMac.SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
begin
  result := AColor;
  if ColorPickerShown then
    result := NSColorToAlphaColor(GetNSColorPanel.color);
end;

procedure TPickerMac.ShowFontPicker;
var
  ff: NSFont;
  fm: NsFontManager;
  fp: NSFontPanel;
begin
  if not FontPickerShown then
  begin
    FontEventHandler := TNSFontPickerEventHandler.Create;
    ff := GetNSFontDefault;
    fm := GetNsFontManager;
    fm.setSelectedFont(ff, false);
    fm.setTarget(FontEventHandler.GetObjectID);
    fm.setAction(sel_getUid('fontChanged:'));
    FontPickerShown := True;
  end;
  fp := GetNSFontPanel;
  fp.makeKeyAndOrderFront(nil);
end;

function TPickerMac.SelectFontFamilyName(AFontName: string): string;
var
  fn: NSString;
  fm: NSFontManager;
begin
  result := AFontName;

  if not FontPickerShown then
    Exit;

  { https://developer.apple.com/documentation/appkit/nsfontmanager }
  fm := GetNSFontManager;
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
  cl:NSArray;
  li:NSString;
  c: LongWord; //NSUInteger
  i: Integer;
begin
  fm := GetNsFontManager;
  cl := fm.availableFontFamilies;
  if cl <> nil then
  begin
    c := cl.count;
    if c > 0 then
    begin
      for i := 0 to cl.count-1 do
      begin
        li := TNSString.Wrap(cl.objectAtIndex(i));
        ML.Add(NSStrToStr(li));
      end;
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
  cp: NSColorPanel;
begin
  cp := GetNSColorPanel;
  FColor := NSColorToAlphaColor(cp.color);
end;

function TNSColorPickerEventHandler.GetObjectiveCClass: PTypeInfo;
begin
  result := TypeInfo(INSColorPickerEventHandler);
end;

{ TNSFontPickerEventHandler }

procedure TNSFontPickerEventHandler.fontChanged(Sender: Pointer);
begin
  { intentionally left empty }

  { believed to be necessary, empty one will do }

  { there seems to be a problem here:
      FontManger.selectedFont,
        retrieved at this time,
          will be the previously slected one // ?
            according to my tests.
  }

  { Use SelectFontFamilyName() method to retrieve the selected font name. }
end;

function TNSFontPickerEventHandler.GetObjectiveCClass: PTypeInfo;
begin
  result := TypeInfo(INSFontPickerEventHandler);
end;

{$endif}

end.
