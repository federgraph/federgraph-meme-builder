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

{ TPickerMac }

function TPickerMac.SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
begin
  { not implemented }
  result := AColor;
end;

function TPickerMac.SelectFontFamilyName(AFontName: string): string;
begin
  { not implemented }
  result := AFontName;
end;

procedure TPickerMac.CollectFontFamilyNames(ML: TStrings);
var
  fManager: NsFontManager;
  list:NSArray;
  lItem:NSString;
  i: Integer;
begin
// stackoverflow: how-to-get-the-list-of-fonts-available-delphi-xe3-firemonkey-2

{ not yet tested on MACOS at all }

  fManager := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
  list := fManager.availableFontFamilies;
  if (List <> nil) and (List.count > 0) then
  begin
    for i := 0 to List.Count-1 do
    begin
      lItem := TNSString.Wrap(List.objectAtIndex(i));
      FontList.Add(String(lItem.UTF8String))
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
