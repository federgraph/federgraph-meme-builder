unit RiggVar.FB.ActionGroups;

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

{$ifdef FPC}
{$mode delphi}
{$endif}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  RiggVar.FB.ActionGroup;

type
  TActionGroupList = class (TList<TActionGroup>)
  public
    GroupNames: TStrings;
    constructor Create;
    destructor Destroy; override;
    procedure AddSpecial(const Value: TActionGroup; AName: string);
    function ActionCount: Integer;
    function GetUsage: string;
    function GetGroup(fa: Integer): Integer;
    function GetGroupName(i: Integer): string;
  end;

implementation

uses
  RiggVar.FB.ActionConst;

{ TActionGroupList }

destructor TActionGroupList.Destroy;
begin
  GroupNames.Free;
  inherited;
end;

function TActionGroupList.ActionCount: Integer;
var
  i: Integer;
  j: Integer;
  cr: TActionGroup;
begin
  j := 0;
  for i := 0 to Count-1 do
  begin
    cr := self[i];
    j := j + Length(cr);
  end;
  result := j;
end;

procedure TActionGroupList.AddSpecial(const Value: TActionGroup; AName: string);
var
  AG: TActionGroup;
begin
  AG := Value;
  GroupNames.Add(AName);
  Add(AG);
end;

constructor TActionGroupList.Create;
begin
  inherited;

  GroupNames := TStringList.Create;

  { App }
  AddSpecial(ActionGroupEmptyAction, 'EmptyAction');
  AddSpecial(ActionGroupPages, 'Pages');
  AddSpecial(ActionGroupForms, 'Forms');
  AddSpecial(ActionGroupTouchLayout, 'TouchLayout');
//  AddSpecial(ActionGroupEmptyLastLine, 'LastLine');
//  AddSpecial(ActionGroupHelp, 'Help');

  { Format }
//  AddSpecial(ActionGroupFormat, 'Format');
//  AddSpecial(ActionGroupIconSize, 'IconSize');
  AddSpecial(ActionGroupMemeFormat, 'Format');

  { UI }
//  AddSpecial(ActionGroupWheel, 'Wheel');
  AddSpecial(ActionGroupColorScheme, 'ColorScheme');

  { View }
//  AddSpecial(ActionGroupViewParams, 'ViewParams');
//  AddSpecial(ActionGroupViewOptions, 'ViewOptions');
  AddSpecial(ActionGroupViewFlags, 'ViewFlags');
//  AddSpecial(ActionGroupViewType, 'ViewType');
//  AddSpecial(ActionGroupReset, 'Reset');

  { Texture }
//  AddSpecial(ActionGroupParamT, 'Texture Param');
//  AddSpecial(ActionGroupBitmapCycle, 'BitmapCycle');

  { Copy }
//  AddSpecial(ActionGroupCopyPaste, 'CopyPaste');
//  AddSpecial(ActionGroupCopyImage, 'CopyImage');
//  AddSpecial(ActionGroupCopyOptions, 'CopyOptions');

  { Input }
//  AddSpecial(ActionGroupInput, 'Input');

  { MB }
  AddSpecial(ActionGroupMemeParams, 'MemeParams');
  AddSpecial(ActionGroupMemeSelect, 'MemeSelect');
  AddSpecial(ActionGroupMemeOptions, 'MemeOption');
  AddSpecial(ActionGroupMemeColor, 'MemeColor');
  AddSpecial(ActionGroupMemeSamples, 'MemeSapmples');
  AddSpecial(ActionGroupMemePicker, 'MemePicker');
  AddSpecial(ActionGroupMemeIO, 'MemeIO');
  AddSpecial(ActionGroupMemoReport, 'MemeReport');
  AddSpecial(ActionGroupMemoImage, 'MemeImage');
  AddSpecial(ActionGroupSonstiges, 'Sonstiges');

end;

function TActionGroupList.GetGroup(fa: Integer): Integer;
var
  i: Integer;
  j: Integer;
  l: Integer;
  cr: TActionGroup;
begin
  result := -1;
  for i := 0 to Count-1 do
  begin
    cr := Self.Items[i];
    l := Length(cr);
    for j := 0 to l-1 do
    begin
      if cr[j] = fa then
      begin
        result := i;
        Exit;
      end;
    end;
  end;
end;

function TActionGroupList.GetGroupName(i: Integer): string;
begin
  if (i >= 0) and (i < GroupNames.Count) and (i < Count) then
    result := GroupNames[i]
  else
    result := '';
end;

function TActionGroupList.GetUsage: string;
var
  i: Integer;
  j: Integer;
  l: Integer;
  cr: TActionGroup;
  SL: TStringList;
  s1: string;
begin
  SL := TStringList.Create;
  for i := 0 to faMax-1 do
    SL.Add(Format('%d=0', [i]));

  s1 := '1';
  for i := 0 to Count-1 do
  begin
    cr := Self.Items[i];
    l := Length(cr);
    for j := 0 to l-1 do
    begin
      SL.Values[IntToStr(cr[j])] := s1;
    end;
  end;

  for i := SL.Count-1 downto 0 do
    if (SL.Values[IntToStr(i)] = '1') then
      SL.Delete(i);

  result := SL.Text;
  SL.Free;
end;

end.
