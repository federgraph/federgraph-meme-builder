program FC96;

uses
  FMX.Forms,
  FrmMain in 'App\FrmMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'FC96';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
