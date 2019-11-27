unit RiggVar.FB.Action;

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
  System.Classes,
  RiggVar.FB.ActionConst;

type
  IFederActionHandler = interface
  ['{32729E21-57AF-417E-815E-54DB830A602B}']
    procedure CheckForDuplicates(ML: TStringList);
    procedure Execute(fa: TFederAction);

    function GetCaption(fa: TFederAction): string;
    function GetShortCaption(fa: TFederAction): string;
    function GetChecked(fa: TFederAction): Boolean;
  end;

  TFederActionHandlerBase = class(TInterfacedObject, IFederActionHandler)
  public
    procedure CheckForDuplicates(ML: TStringList); virtual;
    procedure Execute(fa: TFederAction); virtual;

    function GetCaption(fa: TFederAction): string; virtual;
    function GetShortCaption(fa: TFederAction): string; virtual;
    function GetChecked(fa: TFederAction): Boolean; virtual;
  end;

implementation

uses
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong;

procedure TFederActionHandlerBase.CheckForDuplicates(ML: TStringList);
begin
  { not implemented here }
end;

function TFederActionHandlerBase.GetShortCaption(fa: TFederAction): string;
begin
  result := GetFederActionShort(fa);
end;

function TFederActionHandlerBase.GetCaption(fa: TFederAction): string;
begin
  result := GetFederActionLong(fa);
end;

function TFederActionHandlerBase.GetChecked(fa: TFederAction): Boolean;
begin
  result := false;
end;

procedure TFederActionHandlerBase.Execute(fa: TFederAction);
begin
end;

end.
