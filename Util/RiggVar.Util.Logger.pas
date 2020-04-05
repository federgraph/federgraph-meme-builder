unit RiggVar.Util.Logger;

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
  System.Classes;

type
  TTraceDestination = (
    tdNone,
    tdFile,
    tdMemo,
    tdDelphiEventLog,
    tdAll
    );

  TTraceHandler = procedure (Sender: TObject; s: string) of object;

  TLogger = class
  private
    FOnTrace: TTraceHandler;
    procedure SetOnTrace(const Value: TTraceHandler);
  protected
    procedure Write(s: string); virtual;
  public
    TL: TStringList;
    Verbose: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Error(s: string); virtual;
    procedure Info(s: string); virtual;
    procedure InfoVerbose(s: string); virtual;
    property OnTrace: TTraceHandler read FOnTrace write SetOnTrace;
  end;

implementation

const
  MaxLogLines = 24;

procedure TLogger.Write(s: string);
begin
//  log.d(s);
  TL.Add(s);
  if TL.Count > MaxLogLines then
  begin
    while TL.Count > MaxLogLines do
      TL.Delete(0);
  end;
end;

constructor TLogger.Create;
begin
  TL := TStringList.Create;
end;

destructor TLogger.Destroy;
begin
  TL.Free;
  inherited;
end;

procedure TLogger.Error(s: string);
begin
  Write(s);
end;

procedure TLogger.Info(s: string);
begin
  Write(s);
end;

procedure TLogger.InfoVerbose(s: string);
begin
  if Verbose then
    Write(s);
end;

procedure TLogger.SetOnTrace(const Value: TTraceHandler);
begin
  FOnTrace := Value;
end;

end.
