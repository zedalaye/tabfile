unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, Menus, tabfile;

type
  TfrmMain = class(TForm)
    odMain: TOpenDialog;
    grdDump: TDrawGrid;
    MainMenu1: TMainMenu;
    Fichier1: TMenuItem;
    Ouvrir1: TMenuItem;
    N1: TMenuItem;
    Quitter1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure grdDumpDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormDestroy(Sender: TObject);
    procedure Ouvrir1Click(Sender: TObject);
    procedure Quitter1Click(Sender: TObject);
  private
    { Déclarations privées }
    Fichier: ITabFile;

  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}


procedure TfrmMain.FormCreate(Sender: TObject);
var
	tm: TTextMetric;
begin
	// calcul de la taille des lignes et colonnes
	grdDump.Canvas.Font := grdDump.Font;
   GetTextMetrics(grdDump.Canvas.Handle, tm);

   with grdDump do
   begin
	   DefaultRowHeight := tm.tmHeight+tm.tmExternalLeading+1;
      ColWidths[0] := tm.tmMaxCharWidth*8;
      ColWidths[1] := tm.tmMaxCharWidth*16*3;
      ColWidths[2] := tm.tmMaxCharWidth*17;
   end;
end;


{
	Affichage des cellules de la grille (owner draw) pour représenter
   les données mappées (par rapport au pointeur FPointeur)
}
procedure TfrmMain.grdDumpDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
	c, r: LongWord;
   s: String;
   ch: Char;
begin
	with TDrawGrid(Sender).Canvas do
   begin
   	if ARow = 0 then
      begin
      	// c'est la première ligne, afficher les en-têtes
      	case ACol of
         	0: DrawText(Handle, ' Offset', -1, Rect, DT_VCENTER+DT_CENTER);
            1: DrawText(Handle, ' Données hexa', -1, Rect, DT_VCENTER+DT_LEFT);
            2: DrawText(Handle, ' Ascii', -1, Rect, DT_VCENTER+DT_CENTER);
         end;
      end else
      begin
      	// s'il n'y a rien en mémoire
			if Fichier = Nil then
		   	exit;

      	r := ARow - 1;	// ligne réelle dans le tableau (-1 pour la ligne d'entête)
      	case ACol of
         	0: DrawText(Handle, PChar(Format('%.8x', [r*16])), -1,
						Rect, DT_VCENTER+DT_CENTER);
            1: begin
      			s := '';
         		for c:=0 to 15 do
         		begin
         			if r*16+c >= Fichier.Size then
            			break;
                  if c=8 then
            			s := s+' -';
         			ch := Fichier[r*16+c];
                  s := s+Format(' %.2x', [ord(ch)]);
         		end;
               DrawText(Handle, PChar(s), -1, Rect, DT_VCENTER+DT_LEFT+DT_NOPREFIX);
            end;
            2: begin
      			s := '';
         		for c:=0 to 15 do
         		begin
         			if r*16+c >= Fichier.Size then
            			break;
         			ch := Fichier[r*16+c];
            		if (ch>' ') And (ord(ch) < 128) then
            			s := s+ch
            		else
            			s := s+'.';
         		end;
               DrawText(Handle, PChar(s), -1, Rect, DT_VCENTER+DT_LEFT+DT_NOPREFIX);
            end;
         end;
      end;
   end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
	Fichier := Nil;
end;

procedure TfrmMain.Ouvrir1Click(Sender: TObject);
begin
	// demander le nom du fichier à dumper
	if odMain.Execute then
   begin
      // ouvrir le fichier
      Fichier := TTabFile.Open(odMain.FileName, amReadOnly);
      // calculer le nombre de ligne dans la grille
	   grdDump.RowCount := Fichier.Size div 16 + 2;
      grdDump.Invalidate;
   end;
end;

procedure TfrmMain.Quitter1Click(Sender: TObject);
begin
	Close;
end;

end.
