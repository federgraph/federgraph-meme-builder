unit RiggVar.FederModel.ActionMapTablet;

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
  TActionMapTablet = class(TActionMap)
  private
    procedure InitDefault;
    procedure InitAC(cl: TCornerLocation; bi, fa: Integer; cla: TAlphaColor);
  protected
    procedure InitActionsMB(Layout: Integer);
  public
    constructor Create;
    procedure InitActions(Layout: Integer); override;
  end;

{
RG08:
[PM][SH][SA][SL]----[04][03][02][01][PP]
[SW]--------------------------------[05]
------------------------------------[06]
----------------------------------------
[11]------------------------------------
[10]--------------------------------[PE]
[08][09]----------------[VO][WI][WA][HU]

FC70:
[01][03][04][05][06]----[14][13][12][11][07]
02]-------------------------------------[08]
----------------------------------------[09]
----------------------------------------[10]
--------------------------------------------
[22]----------------------------------------
[21]----------------------------------------
[20]------------------------------------[24]
[15][16][17][18][19]----[28][27][26][25][23]

RG10:
[1][2][3][4]----[1][2][3][4][5]
[5]-------------------------[6]
----------------------------[7]
-------------------------------
[3]----------------------------
[4]-------------------------[5]
[1][2]-------------[1][2][3][4]

}

implementation

constructor TActionMapTablet.Create;
begin
  inherited;
  FPageCount := 3;
  FEscapeIndex := 4;
  TestName := 'Tablet Page';
end;

procedure TActionMapTablet.InitActions(Layout: Integer);
begin
  InitDefault;
  InitActionsMB(Layout);
end;

procedure TActionMapTablet.InitActionsMB(Layout: Integer);
var
  cl: TCornerLocation;
  cla: TAlphaColor;
begin
  cla := ClaWhite;
  case Layout of
    1:
    begin
      cl := TopLeft;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faMemeToggleEdits, claPlum);
      InitAC(cl, 3, faMemeReset, claPlum);
      InitAC(cl, 4, faMemeSwapText, claPlum);
      InitAC(cl, 5, faNoop, claPlum);
      InitAC(cl, 6, faMemeFormat1, cla);

      cl := TopRight;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faNoop, cla);
      InitAC(cl, 3, faMemeSelectTop, cla);
      InitAC(cl, 4, faMemeSelectBottom, cla);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faMemePickFont, cla);
      InitAC(cl, 7, faMemePickColor, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomLeft;
      InitAC(cl, 1, faMemeParamTopGlow, cla);
      InitAC(cl, 2, faMemeParamBottomGlow, cla);
      InitAC(cl, 3, faMemeParamTopMargin, cla);
      InitAC(cl, 4, faMemeParamBottomMargin, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faMemeParamBottomSize, cla);
      InitAC(cl, 7, faMemeParamTopSize, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomRight;
      InitAC(cl, 1, faMemeSample00, cla);
      InitAC(cl, 2, faMemeSampleM, cla);
      InitAC(cl, 3, faMemeSampleP, cla);
      InitAC(cl, 4, faMemeSampleT, cla);
      InitAC(cl, 5, faMemeSample01, cla);
      InitAC(cl, 6, faMemeSample02, cla);
    end;

    2:
    begin
      cl := TopLeft;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faMemeGotoLandscape, claPlum);
      InitAC(cl, 3, faMemeGotoSquare, claPlum);
      InitAC(cl, 4, faMemeGotoPortrait, claPlum);
      InitAC(cl, 5, faNoop, claPlum);
      InitAC(cl, 6, faMemeAdaptFormSize, cla);

      cl := TopRight;
      InitAC(cl, 1, faMemeFormat0, cla);
      InitAC(cl, 2, faMemeFormat1, claWhite);
      InitAC(cl, 3, faMemeFormat2, cla);
      InitAC(cl, 4, faMemeFormat3, cla);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faMemeFormat4, cla);
      InitAC(cl, 7, faMemeFormat5, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomLeft;
      InitAC(cl, 1, faMemeFormat6, cla);
      InitAC(cl, 2, faMemeFormat7, cla);
      InitAC(cl, 3, faMemeFormat8, cla);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faMemeFormat9, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomRight;
      InitAC(cl, 1, faMemeClearImage, cla);
      InitAC(cl, 2, faMemeInitChecker, cla);
      InitAC(cl, 3, faCycleColorSchemeM, cla);
      InitAC(cl, 4, faCycleColorSchemeP, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faNoop, cla);
    end;

    3:
    begin
      cl := TopLeft;
      //InitAC(cl, 1, faActionPageM, claYellow);
      InitAC(cl, 2, faMemeCycleLightColorM, cla);
      InitAC(cl, 3, faMemeCycleLightColorP, cla);
      InitAC(cl, 4, faMemeCycleDarkColorM, cla);
      InitAC(cl, 5, faMemeCycleDarkColorP, cla);
      InitAC(cl, 6, faNoop, cla);

      cl := BottomLeft;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faMemeToggleDropTarget, cla);
      InitAC(cl, 3, faMemeToggleHelp, cla);
      InitAC(cl, 4, faMemeToggleReport, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faNoop, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := TopRight;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faMemeSample00, cla);
      InitAC(cl, 3, faMemeSample01, cla);
      InitAC(cl, 4, faMemeSample02, cla);
      //InitAC(cl, 5, faActionPageP, claYellow);
      InitAC(cl, 6, faNoop, cla);
      InitAC(cl, 7, faNoop, cla);
      InitAC(cl, 8, faNoop, cla);

      cl := BottomRight;
      InitAC(cl, 1, faNoop, cla);
      InitAC(cl, 2, faNoop, cla);
      InitAC(cl, 3, faNoop, cla);
      InitAC(cl, 4, faNoop, cla);
      InitAC(cl, 5, faNoop, cla);
      InitAC(cl, 6, faNoop, cla);
    end;

  end;
end;

procedure TActionMapTablet.InitDefault;
begin
  IAC(1, faActionPageM, claYellow);
  IAC(7, faActionPageP, claYellow);
end;

procedure TActionMapTablet.InitAC(cl: TCornerLocation; bi,
  fa: Integer; cla: TAlphaColor);
var
  j: Integer; // FrameLocation
begin
  { First, translate from CornerLocation to FrameLocation }
  j := 0;
  case cl of
    TopLeft:
    begin
      case bi of
        1: j := 1;
        2: j := 3;
        3: j := 4;
        4: j := 5;
        5: j := 6;
        6: j := 2;
      end;
    end;

    TopRight:
    begin
      case bi of
        1: j := 14;
        2: j := 13;
        3: j := 12;
        4: j := 11;
        5: j := 7;
        6: j := 8;
        7: j := 9;
        8: j := 10;
      end;
    end;

    BottomRight:
    begin
      case bi of
        1: j := 28;
        2: j := 27;
        3: j := 26;
        4: j := 25;
        5: j := 23;
        6: j := 24;
      end;
    end;

    BottomLeft:
    begin
      case bi of
        1: j := 15;
        2: j := 16;
        3: j := 17;
        4: j := 18;
        5: j := 19;
        6: j := 20;
        7: j := 21;
        8: j := 22;
      end;
    end;
  end;
  { Init button with Action and Color }
  IAC(j, fa, cla);
end;

end.
