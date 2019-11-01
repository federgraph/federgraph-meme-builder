program FC96;

uses
  FMX.Forms,
  FrmMeme in 'App\FrmMeme.pas' {FormMeme};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'FC96';
  Application.CreateForm(TFormMeme, FormMeme);
  Application.Run;
end.
