unit RiggVar.FederModel.ActionMapPhone;

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
  System.UITypes,
  System.UIConsts,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionMap;

type
  TActionMapPhone = class(TActionMap)
  private
    cla: TAlphaColor;
  protected
    procedure InitActionsMB(Layout: Integer);
  public
    constructor Create;
    procedure InitActions(Layout: Integer); override;
  end;

implementation

uses
  RiggVar.App.Main;

constructor TActionMapPhone.Create;
begin
  inherited;
  FPageCount := 2;
  TestName := 'Phone Page';
end;

procedure TActionMapPhone.InitActions(Layout: Integer);
begin
  InitActionsMB(Layout);
end;

procedure TActionMapPhone.InitActionsMB(Layout: Integer);
begin
{
[9]----[10]
[1]--------
[2]--------
[3]--------
-----------
--------[4]
--------[5]
--------[6]
[7]--- -[8]
}

  cla := claWhite;

  case Layout of
    1:
    begin
      InitAction(1, faMemeSwapText);
      InitAction(2, faMemeReset);
      InitAction(3, faMemeCopyBitmap);

      InitAction(4, faMemeFormat1);
      InitAction(5, faMemeSampleP);
      InitAction(6, faMemeSampleM);

      InitAction(7, faMemeToggleEdits);
      InitAction(8, faNoop);
    end;

    2:
    begin
      InitAction(1, faMemeParamTopSize);
      InitAction(2, faMemeParamBottomSize);
      InitAction(3, faNoop);

      InitAction(4, faNoop);
      InitAction(5, faMemeParamTopMargin);
      InitAction(6, faMemeParamBottomMargin);

      InitAction(7, faNoop);
      InitAction(8, faNoop);
    end;

  end;
end;

end.

