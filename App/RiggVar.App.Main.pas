unit RiggVar.App.Main;

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
  RiggVar.FB.Scheme,
  RiggVar.FB.SpeedColor,
  RiggVar.App.Main0;

type
  TMain = TMain0;

var
  Main: TMain;

type
  MainConst = class
  const
    DefaultBtnFontSize = 24;
  end;

  MainVar = class
  public
  class var
    ColorScheme: TColorScheme;
    SpeedColorScheme: TSpeedColorScheme;
    Raster: Integer;
    ClientWidth: Integer;
    ClientHeight: Integer;
    class constructor Create;
  end;

const
  ColorSchemeCount = 7;

implementation

{ MainVars }

class constructor MainVar.Create;
begin
  ColorScheme := TColorScheme.Create(5);
  Raster := 70;
end;

end.

