program Dump;

uses
  Forms,
  fMain in 'fMain.pas' {frmMain},
  tabfile in 'tabfile.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
