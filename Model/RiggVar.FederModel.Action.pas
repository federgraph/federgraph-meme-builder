unit RiggVar.FederModel.Action;

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
  System.SysUtils,
  System.Classes,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Action;

type
  TFederActionHandler = class(TFederActionHandlerBase)
  public
    procedure Execute(fa: TFederAction); override;
    function GetChecked(fa: TFederAction): Boolean; override;
  end;

implementation

uses
  RiggVar.App.Main;

procedure TFederActionHandler.Execute(fa: TFederAction);
begin
  if Main <> nil then
    Main.ExecuteAction(fa);
end;

function TFederActionHandler.GetChecked(fa: TFederAction): Boolean;
begin
  result := false;
  if Main <> nil then
    result := Main.GetChecked(fa);
end;

end.
