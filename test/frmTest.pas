(*
 * $Id: frmTest.pas,v 1.4 2004/10/06 14:25:05 Jean Exp $
 *)

unit frmTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TfrmMain = class(TForm)
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    tsTabFile: TTabSheet;
    tsTabStringsFile: TTabSheet;
    Label1: TLabel;
    btCreation: TButton;
    rgTaille: TRadioGroup;
    eFichier: TEdit;
    bgTest: TGroupBox;
    btInverse: TButton;
    btTaille50: TButton;
    btAleat: TButton;
    bt1moAleat: TButton;
    btRaz: TButton;
    gbChaine: TGroupBox;
    Label2: TLabel;
    btRecherche: TButton;
    eChaine: TEdit;
    btCopie10: TButton;
    btCopieFin: TButton;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    btStringVisualiser: TButton;
    rgTailleFichierTexte: TRadioGroup;
    Label3: TLabel;
    btCreationFichierTexte: TButton;
    eFichierTexte: TEdit;
    rgSeparateur: TRadioGroup;
    Button6: TButton;
    Button7: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    Button8: TButton;
    Button5: TButton;
    btStringRecherche: TButton;
    eChaineRecherche: TEdit;
    Button10: TButton;
    btCompterLignes: TButton;
    Button9: TButton;
    procedure btInverseClick(Sender: TObject);
    procedure btRechercheClick(Sender: TObject);
    procedure btTaille50Click(Sender: TObject);
    procedure btCreationClick(Sender: TObject);
    procedure btAleatClick(Sender: TObject);
    procedure bt1moAleatClick(Sender: TObject);
    procedure btCopie10Click(Sender: TObject);
    procedure btRazClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btCopieFinClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btStringVisualiserClick(Sender: TObject);
    procedure btCreationFichierTexteClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure btStringRechercheClick(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure btCompterLignesClick(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    { Déclarations privées }
    procedure Debut(s: String);
    procedure Fin;
    function TailleFichier: integer;
    function TailleFichierTexte: integer;
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

uses tabfile, bmsearch, frmTestVisu;

{$R *.DFM}

procedure TfrmMain.btInverseClick(Sender: TObject);
var
   ft: ITabFile;
   i: longword;
   temp: char;
begin
   // inverser un fichier
   Debut('Inversion');
   ft := TTabFile.Open(eFichier.Text);
   for i := 0 to ft.Size div 2 do
   begin
      // swap des données
      temp := ft[i];
      ft[i] := ft[ft.size-i-1];
      ft[ft.size-i-1] := temp;
   end;
   Fin;
end;

procedure TfrmMain.btRechercheClick(Sender: TObject);
var
   bms: IBMSearch;
   ft: ITabFile;
   offset, pos: integer;
begin
   // recherche d'une chaîne dans le tableau
   ft := TTabFile.Open(eFichier.Text, amReadOnly);
   bms:= TBMSearch.Create(eChaine.Text);

   offset := 0;
   repeat
      Debut('Début de recherche');
      pos := bms.Search(ft.Ptr, ft.Size, offset);
      Fin;
      if pos<>-1 then
         if MessageDlg(Format('Trouvé offset %d'#13#10'Continuer ?', [pos]), mtInformation, [mbYes, mbNo], 0)<>mrYes then
            break;
      offset := pos+1;
   until pos=-1;
end;

procedure TfrmMain.btTaille50Click(Sender: TObject);
var
   ft: ITabFile;
begin
   Debut('Réduction de la taille à 50 octets');
   // réduire la taille du tableau (et du fichier)
   ft := TTabFile.Open(eFichier.Text);
   ft.Size := 50;
   fin;
end;

procedure TfrmMain.btCreationClick(Sender: TObject);
var
   ft: ITabFile;
begin
   Debut('Création tableau / fichier');
   ft := TTabFile.CreateNew(eFichier.Text, TailleFichier);
   fin;
end;

procedure TfrmMain.btAleatClick(Sender: TObject);
var
   ft: ITabFile;
   i: integer;
begin
   Debut('Remplissage avec des valeurs aléatoires');
   ft := TTabFile.Open(eFichier.Text);
   for i:=0 to ft.Size-1 do
      ft[i]:=Char(random(256));
   Fin;
end;

procedure TfrmMain.bt1moAleatClick(Sender: TObject);
var
   ft: ITabFile;
   i: integer;
begin
   Debut('Remplissage aléatoire');
   ft := TTabFile.Open(eFichier.Text);
   for i:=0 to 1000000 do
      ft[random(ft.Size-1)]:=Char(random(256));
   Fin;
end;

procedure TfrmMain.btCopie10Click(Sender: TObject);
var
   ft: ITabFile;
   i: integer;
begin
   Debut('Copie de la chaîne à 10 position aléatoires dans le fichier');
   ft := TTabFile.Open(eFichier.Text);
   for i:=1 to 10 do
      ft.StringAccess[random(ft.Size-1-LongWord(Length(eChaine.Text)))] := eChaine.Text;
   Fin;
end;

procedure TfrmMain.btRazClick(Sender: TObject);
var
   ft: ITabFile;
   i: integer;
begin
   Debut('Remise à 0');
   ft := TTabFile.Open(eFichier.Text);
   for i:=0 to ft.Size-1 do
      ft[i]:=#0;
   Fin;
end;

procedure TfrmMain.Debut(s: String);
begin
   StatusBar1.SimpleText := s+'...';
   StatusBar1.Tag := GetTickCount;
end;

procedure TfrmMain.Fin;
begin
   StatusBar1.SimpleText := StatusBar1.SimpleText + Format(' %g s',[(GetTickCount-DWord(StatusBar1.Tag)) / 1000]);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
   Randomize;
end;

procedure TfrmMain.btCopieFinClick(Sender: TObject);
var
   ft: ITabFile;
begin
   Debut('Copie de la chaîne à la fin du fichier');
   ft := TTabFile.Open(eFichier.Text);
   ft.StringAccess[ft.Size-1-LongWord(Length(eChaine.Text))] := eChaine.Text;
   Fin;
end;

function TfrmMain.TailleFichier: integer;
begin
   case rgTaille.ItemIndex of
    0: result := 1024;
    1: result := 1024*1024;
    2: result := 10*1024*1024;
    3: result := 100*1024*1024;
    else result := 0;
   end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
   ft: ITabFile;
begin
   Debut('Insertion de 1000 octets au début');
   ft := TTabFile.Open(eFichier.Text);
   ft.Insert(0, 1000, 0);

   Fin;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
   ft: ITabFile;
begin
   Debut('Suppression de 500 octets au début');
   ft := TTabFile.Open(eFichier.Text);
   ft.Delete(0, 500);
   Fin;
end;

procedure TfrmMain.Button4Click(Sender: TObject);
var
   ft: ITabFile;
begin
   Debut('Ajouter 10 # à la fin');
   ft := TTabFile.Open(eFichier.Text);
   ft.Append(10, Byte('#'));
   Fin;
end;

procedure TfrmMain.btStringVisualiserClick(Sender: TObject);
var
   f: TfrmVisu;
   its: ITabStringsFile;
begin
   Debut('Chargement fichier texte');
   its := TTabStringsFile.Open('fichier.txt');
   f := TfrmVisu.Create(self, its);
   Fin;
   try
      f.ShowModal;
   finally
      f.Release;
   end;
end;

procedure TfrmMain.btCreationFichierTexteClick(Sender: TObject);
var
   ft: ITabFile;
   i: longword;
   size: Longword;
const
   chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWHYZ 1234567890';
begin
   Debut('Création fichier texte');
   ft := TTabFile.CreateNew(eFichierTexte.Text, TailleFichierTexte);
   i := 0;
   size := ft.Size;
   while i<size do
   begin
      ft[i] := PChar(chars)[random(length(chars))];
      inc(i);
      if (random(128)=0) And (i<ft.Size) then
      begin
         case rgSeparateur.ItemIndex of
            0: ft[i] := #13;
            1: ft[i] := #10;
            2: if i<ft.Size-1 then
               begin
                   ft[i] := #13;
                   inc(i);
                   ft[i] := #10;
               end;
         end;
         inc(i);
      end;

   end;
   fin;
end;

function TfrmMain.TailleFichierTexte: integer;
begin
   case rgTailleFichierTexte.ItemIndex of
    1: result := 1024;
    2: result := 1024*1024;
    3: result := 10*1024*1024;
    4: result := 100*1024*1024;
    else result := 0;
   end;
end;

procedure TfrmMain.Button6Click(Sender: TObject);
var
   its: ITabStringsFile;
begin
   Debut('Modification de la première ligne');
   its := TTabStringsFile.Open(eFichierTexte.Text);
   its[0] := 'Ligne modifiée';
   Fin;
end;

procedure TfrmMain.Button7Click(Sender: TObject);
var
   its: ITabStringsFile;
begin
   Debut('Insertion d''une ligne au début');
   its := TTabStringsFile.Open(eFichierTexte.Text);
   its.Insert(0, 'Ligne insérée');
   Fin;
end;


procedure TfrmMain.Button3Click(Sender: TObject);
var
   its: ITabStringsFile;
begin
   OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
   if OpenDialog1.Execute then
   begin
      Debut('Chargement d''un fichier');
      its := TTabStringsFile.Open(eFichierTexte.Text);
      its.LoadFromFile(OpenDialog1.FileName);
      Fin;
   end;
end;


procedure TfrmMain.Button8Click(Sender: TObject);
var
   its: ITabStringsFile;
begin
   Debut('Ajout d''une ligne vide');
   its := TTabStringsFile.Open(eFichierTexte.Text);
   its.Add('');
   Fin;
end;

procedure TfrmMain.Button5Click(Sender: TObject);
var
   its: ITabStringsFile;
begin
   Debut('Insertion d''une ligne vide');
   its := TTabStringsFile.Open(eFichierTexte.Text);
   its.Insert(0, '');
   Fin;
end;

procedure TfrmMain.btStringRechercheClick(Sender: TObject);
var
   its: ITabStringsFile;
   l: LongWord;
begin
   its := TTabStringsFile.Open(eFichierTexte.Text);
   Debut('Recherche d''un texte');
   l := its.Search(eChaineRecherche.Text);
   Fin;
   if l<>$FFFFFFFF then
      ShowMessage('Trouvé à la ligne: '+ IntToStr(l));
end;

procedure TfrmMain.Button10Click(Sender: TObject);
var
   its: ITabStringsFile;
begin
   Debut('Suppression de la première ligne');
   its := TTabStringsFile.Open(eFichierTexte.Text);
   ShowMessage(IntToStr(its.Count));
   its.Delete(0);
   Fin;
end;


procedure TfrmMain.btCompterLignesClick(Sender: TObject);
var
   its: ITabStringsFile;
begin
   Debut('Compter les lignes');
   its := TTabStringsFile.Open(eFichierTexte.Text);
   Fin;
   ShowMessage(Format('%d ligne(s) dans le fichier', [its.Count]));
end;

procedure TfrmMain.Button9Click(Sender: TObject);
var
   i: integer;
   its: ITabStringsFile;
   sl: TStringList;
begin
   sl := TStringList.Create;
   try
      for i:=1 to 30000 do
         sl.Add(Format('Ligne n° %d', [i]));

      Debut('Ajouter 3000 lignes');
//      its := TTabStringsFile.Open(eFichierTexte.Text);
      its := TTabStringsFile.CreateNew('xxx.txt', lsCRLF);
      its.AddStrings(sl);
      Fin;
   finally
      sl.Free;
   end;
end;

end.

