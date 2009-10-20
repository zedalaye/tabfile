(*
 * $Id: Test.dpr,v 1.2 2004/09/16 12:18:33 Jean Exp $
 *)

program Test;

uses
  Forms,
  frmTest in 'frmTest.pas' {frmMain},
  tabfile in 'tabfile.pas',
  bmsearch in 'bmsearch.pas',
  frmTestVisu in 'frmTestVisu.pas' {frmVisu};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
