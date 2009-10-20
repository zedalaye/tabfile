unit frmViewFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tabfile, Menus, ComCtrls;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    MainMenu1: TMainMenu;
    Fichier1: TMenuItem;
    Ouvrir1: TMenuItem;
    Quitter1: TMenuItem;
    Edition1: TMenuItem;
    Rechercher1: TMenuItem;
    OccurrenceSuivante: TMenuItem;
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure Quitter1Click(Sender: TObject);
    procedure Ouvrir1Click(Sender: TObject);
    procedure ListView1CustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure Rechercher1Click(Sender: TObject);
  private
    { Déclarations privées }
    FTab: ITabStringsFile;
    sSearchText: string;
    iSearchStart: cardinal;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.ListView1Data(Sender: TObject; Item: TListItem);
begin
  Item.Caption := IntToStr(Item.Index);
  Item.SubItems.Add(FTab[Item.Index]);
end;

procedure TForm1.Quitter1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Ouvrir1Click(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    Filter := 'Text files (*.txt)|*.txt|Tous fichiers (*.*)|*.*';
    if Execute then
    begin
      ListView1.Items.Count := 0;
      FTab := TTabStringsFile.Open(FileName);
      ListView1.Items.Count := FTab.Count;
      Caption := FileName;
    end;
  finally
    Free;
  end;
end;

procedure TForm1.ListView1CustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if not (cdsSelected in State) then
    if Item.Index mod 2 <> 0 then
      Sender.Canvas.Brush.Color := clInfoBk;
end;

procedure TForm1.Rechercher1Click(Sender: TObject);
var
  i: cardinal;
begin
  if Assigned(FTab) then
  begin
    if sender = Rechercher1 then
    begin
      iSearchStart := 0;
      sSearchText := InputBox('Recherche', 'Chaîne', '');
    end
    else
      Inc(iSearchStart);
    i := FTab.Search(sSearchText, iSearchStart);

    if i <> $FFFFFFFF then
      with ListView1.Items[i] do
      begin
        iSearchStart := i;
        Selected := true;
        MakeVisible(true);
      end
    else
      ShowMessage('Non trouvé');
    OccurrenceSuivante.Enabled := i <> $FFFFFFFF;
  end;
end;

end.

