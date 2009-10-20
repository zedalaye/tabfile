(*
 * $Id: tabfile.pas,v 1.2 2004/09/16 12:56:37 Hitech Exp $
 *)

(**
 * @TITLE TabFile
 * @AUTHOR Jean Lacoste
 * @COPYRIGHT 2001-2002
 * Freeware
 *)

unit tabfile;

interface

uses windows, sysutils, classes;

type
   (**
    * Modes d'accès pour les TabFiles (lecture seule ou lecture écriture).
    *)
   TAccessMode = (amReadOnly, amReadWrite);

   (**
    * La classe des exceptions générées spécifiquement par les membres
    * de la classe TTabFile
    * @INHERITS_FROM Exception
    *)
   ETabFileError = class(Exception)
   end;

   (**
    * La classe des exceptions générées spécifiquement par les membres
    * de la classe TTabStringsFile
    * @INHERITS_FROM Exception
    *)
   ETabStringsFileError = class(ETabFileError)
   end;

   (**
    * Interface de définition des TabFiles<P>
    * But : permet d'utiliser un fichier comme un tableau.<P>
    * L'implémentation est faite par la classe TTabFile qui fonctionne par
    * comptage de référence.
    * C'est par l'intermédiaire de cette interface que les objets TTabFile sont
    * manipulés.<P>Les
    * tableaux sont créés par les constructeurs de la classe TTabFile et sont
    * manipulés par une variable de type ITabFile.<P>
    *     La classe propose 2 constructeurs :
    * <ul>
    *   <li>
    *    <B>Open</B> : crée un tableau sur un fichier existant</li>
    *   <li><B>CreateNew</B> : crée un tableau sur un fichier créé pour l'occasion</li>
    * </ul>
    *  <P>
    *    Les propriétés essentielles de l'interface sont :
    * <ul>
    *   <li><b>Size</b> : taille du tableau (indices de 0 à Size-1). <b>Attention</b>, on peut
    *      également modifier Size pour changer la taille du tableau et donc,
    *      du fichier qui est associé.</li>
    *   <li><b>Ptr</b> : pointeur PChar sur le début du tableau.</li>
    *   <li><b>Content[pos]</b> : propriété indicée permettant l'accès direct à n'importe
    *      quel élément du tableau, c'est la propriété par défaut et donc,
    *      il n'est pas nécessaire de saisir son nom.</li>
    *   <li><b>StringAccess[pos]</b> : accède à la chaîne placée à la position indiquée
    *      et permet de placer une chaîne à cet endroit.</li>
    *   <li><b>ReadOnly</b> :  indique s'il est possible d'accéder ou non au tableau en
    *      écriture.</li>
    * </ul>
    * <p>Exemple d'utilisation :</p>
    *     <pre><b>var</b>
    *  ft: ITabFile; <font color="#008000">// la variable contenant le tableau</font>
    *  i: integer;
    * <b>begin</b>
    *  <font color="#008000">// création du tableau sur le fichier indiqué</font>
    *  ft := TTabFile.Open('test1.txt');
    *  <font color="#008000">// parcourir tous les éléments du tableau</font>
    *  <b>for</b> i:=0 <b>to</b> ft.Size-1 <b>do</b>
    *    <font color="#008000">// mettre chaque élément du tableau en majuscule</font>
    *    ft[i]:=UpCase(ft[i]);
    * <b>end</b>;</pre>
    * <p>Note : Les objets TTabFile manipulés par l'intermédiaire de cette interface
    * sont gérés par comptage de référence. Il n'est donc pas nécessaire de
    * détruire le tableau lorsqu'il n'est plus utile (il ne faut pas le libérer par
    * Free ou Destroy). Toutefois, pour fermer le tableau, si cela s'avère
    * nécessaire (pour libérer le fichier), il suffit d'affecter la valeur Nil à la
    * variable.</p>
    *)
   ITabFile = interface
      (**
       * Retourne un pointeur PChar sur le début de la mémoire mappée. Ce pointeur
       * peut être utilisé pour accéder à n'importe quel caractère du fichier par
       * déréférencement.
       *)
      function GetPtr: PChar;
      (**
       * Retourne l'adresse de l'offset indiqué en paramètre
       * Permet de faire pointer un record à cette adresse
       *)
      function Offset(offset: LongWord): Pointer;
      (**
       * Retourne la taille du fichier mappé.
       *)
      function GetSize: LongWord;
      (**
       * Modifie la taille du fichier. Le dernier indice du tableau devient alors
       * la taille du fichier -1 (la numérotation commence à 0).
       * @PARAM v Taille du fichier
       *)
      procedure SetSize(v: LongWord);
      (**
       * Retourne le caractère à la position indiquée
       * @PARAM pos Position du caractère dans le fichier (de 0 à GetSize-1)
       *)
      function GetContent(pos: LongWord): Char;
      (**
       * Modifie le caractère à la position indiquée
       * @PARAM pos Position du caractère dans le fichier (de 0 à GetSize-1)
       * @PARAM value Caractère à placer dans le fichier à la position indiquée
       *)
      procedure SetContent(pos: LongWord; const Value: Char);
      (**
       * Retourne une chaîne de caractères débutant à la position indiquée. La
       * chaîne se termine à la fin du fichier sur un séparateur (CR ou LF)
       * @PARAM pos Position du caractère dans le fichier (de 0 à GetSize-1)
       *)
      function GetStringAccess(pos: LongWord): String;
      (**
       * Place la chaîne indiquée dans le fichier à la position indiquée
       * @PARAM pos Position du caractère dans le fichier (de 0 à GetSize-1)
       * @PARAM Value Chaîne de caractères à placer dans le fichier
       *)
      procedure SetStringAccess(pos: LongWord; const Value: String);
      (**
       * Ouvre le fichier indiqué dans le mode précisé. Si un fichier était ouvert
       * il est fermé.
       * @PARAM fileName Nom du fichier à ouvrir
       * @PARAM accessMode Mode d'ouverture du fichier (par défaut amReadWrite)
       *)
      procedure OpenFile(fileName: TFileName; accessMode: TAccessMode=amReadWrite);
      (**
       * Crée et ouvre le fichier indiqué avec la taille précisée. Si un fichier était ouvert
       * il est fermé. Un fichier portant le même nom est éventuellement écrasé pendant
       * l'opération.
       * @PARAM fileName Nom du fichier à ouvrir
       * @PARAM tabSize Taille du fichier (en octets)
       * @PARAM bOverwrite Boolean autorisant l'écrasement d'un éventuel fichier portant le même nom
       * (par défaut true)
       *)
      procedure CreateFile(fileName: TFileName; tabSize: LongWord; bOverwrite: boolean=true);
      (**
       * Ferme le fichier associé au tableau.
       *)
      procedure CloseFile;
      (**
       * Retourne True si le tableau est en mode lecture seule.
       *)
      function IsReadOnly: boolean;
      (**
       * Insère un certain nombre d'octets à une position quelconque du tableau
       * @PARAM pos Position où l'insertion est effectuée (de 0 à GetSize). Si la position
       * est égale à GetSize, alors le fonctionnement est similaire à la procédure Append
       * @PARAM sizebloc Taille du bloc à insérer (en octets)
       * @PARAM fill Caractère utilisé pour le remplissage de la zone insérée (par défaut 0)
       *)
      procedure Insert(pos: LongWord; sizebloc: LongWord; fill: byte=0);
      (**
       * Ajoute un certain nombre d'octets à la fin du tableau.
       * @PARAM sizebloc Taille du bloc à ajouter (en octets)
       * @PARAM fill Caractère utilisé pour le remplissage de la zone ajoutée (par défaut 0)
       *)
      procedure Append(sizebloc: LongWord; fill: byte=0);
      (**
       * Efface une partie du tableau (la taille du tableau est réduite)
       * @PARAM pos Position où la suppression est effectuée (de 0 à GetSize-1)
       * @PARAM sizebloc Taille du bloc à effacer (en octets)
       *)
      procedure Delete(pos: LongWord; sizebloc: LongWord);
      (**
       * Taille du tableau (et du fichier associé). Note, cette propriété fonctionne en
       * lecture / écriture.
       * @TYPE LongWord
       *)
      property Size: LongWord read GetSize write SetSize;
      (**
       * Retourne un pointeur sur le premier caractère du fichier (dans la mémoire mappée).
       * @TYPE PChar
       *)
      property Ptr: PChar read GetPtr;
      (**
       * Accède à n'importe quel caractère du fichier par son indice. C'est la propriété par
       * défaut et il n'est donc pas nécessaire de préciser le nom Content pour y accéder.<P>
       * Si le mode le permet, cette propriété fonctionne en mode lecture / écriture.<P>
       * Equivalent aux méthodes GetContent et SetContent.
       * @TYPE Char
       * @PARAM pos Position du caractère dans le tableau (de 0 à GetSize-1)
       *)
      property Content[pos: LongWord]: Char read GetContent write SetContent; default;
      (**
       * Accède à n'importe quel endroit du tableau sous la forme d'une chaine de caractères.<P>
       * Si le mode le permet, cette propriété fonctionne en mode lecture / écriture.<P>
       * Equivalent aux méthodes GetStringAccess et SetStringAccess.
       * @TYPE String
       * @PARAM pos Position du caractère dans le tableau (de 0 à GetSize-1)
       *)
      property StringAccess[pos: LongWord]: String read GetStringAccess write SetStringAccess;
      (**
       * Propriété en lecture seule indiquant si l'écriture est autorisée.
       * @TYPE Boolean
       *)
      property ReadOnly: boolean read IsReadOnly;
   end;

   (**
    * L'interface de définition des TabStringsFiles<P>
    * C'est par l'intermédiaire de cette interface que les objets TTabStringsFile sont
    * manipulés.
    *)
   ITabStringsFile = interface
      (**
       * Retourne la taille du fichier contenant le tableau (en octets).
       *)
      function GetFileSize: LongWord;
      (**
       * Retourne une chaîne de caractères contenant la totalité du fichier texte.<P>
       * Le séparateur de lignes est laissé entre chaque chaîne de caractères du tableau.
       *)
      function GetTextStr: string;
      (**
       * Remplace la totalité du fichier (et du tableau) par le contenu de la chaîne de
       * caractères transmise. Le nouveau contenu du fichier est examiné pour trouver
       * les séparateurs et ainsi former les lignes du tableau.
       * @PARAM Value Nouvelle chaîne de caractères
       *)
      procedure SetTextStr(const Value: string);
      (**
       * Retourne la chaîne de caractères se trouvant à l'index indiqué.
       * @PARAM Index Indice dans le tableau
       *)
      function Get(Index: LongWord): string;
      (**
       * Remplace la chaîne de caractères se trouvant à l'index indiqué par celle qui est
       * transmise.
       * @PARAM Index Indice dans le tableau
       * @PARAM s Chaîne de caractères à placer dans le tableau
       *)
      procedure Put(Index: LongWord; const S: string);
      (**
       * Retourne le nombre de lignes dans le tableau (en commençant à 0)
       *)
      function GetCount: LongWord;
      (**
       * Ajoute une chaîne de caractères à la fin du tableau.
       * @PARAM s Chaîne de caractères à placer dans le tableau
       *)
      function Add(const S: string): LongWord;
      (**
       * Ajoute les chaînes contenues dans Strings à la fin du tableau.
       * @PARAM Strings Objet TStrings contenant les chaînes à ajouter
       *)
      procedure AddStrings(Strings: TStrings); overload;
      (**
       * Ajoute les chaînes contenues dans Strings à la fin du tableau.
       * @PARAM Strings Objet ITabStringsFile contenant les chaînes à ajouter
       *)
      procedure AddStrings(Strings: ITabStringsFile); overload;
      (**
       * Ajoute les chaînes contenues dans le stream à la fin du tableau.
       * @PARAM Strings Objet TStream contenant les chaînes à ajouter
       *)
      // procedure AddStrings(Strings: TStream); overload; // delphi n'a même pas une fonction pour lire une chaîne dans un stream !
      (**
       * Efface le contenu du tableau et réduit la taille du fichier à 0 octet
       *)
      procedure Clear;
      (**
       * Efface la chaîne se trouvant à l'indice précisé. Les chaînes suivantes sont
       * décalées pour utiliser la place libérée.
       * @PARAM Index Indice de la chaîne dans le tableau
       *)
      procedure Delete(Index: DWord);
      (**
       * Compare le tableau avec celui transmis. La comparaison donne un résultat
       * True si les deux tableaux comportent le même nombre d'éléménts et que
       * ceux-ci sont identiques.
       * @PARAM Strings Tableau de type ITabStringsFile à comparer
       *)
      function Equals(Strings: ITabStringsFile): Boolean;
      (**
       * Echange les chaînes se trouvant aux indices indiqués.
       * @PARAM Index1 Indice de la première chaîne
       * @PARAM Index2 Indice de la deuxième chaîne
       *)
      procedure Exchange(Index1, Index2: LongWord);
      (**
       * Alloue et retourne un buffer de type PChar contenant la totalité du fichier texte.<P>
       * <I>Note : Il est de la responsabilité de l'appelant de libérer la mémoire allouée pour
       * le tableau retourné</I><P>
       * Le séparateur de lignes est laissé entre chaque chaîne de caractères du tableau.
       *)
      function GetText: PChar;
      (**
       * Retourne l'index de la première occurence de la chaîne dans le tableau.
       * @PARAM S Chaîne de caractères à rechercher
       *)
      function IndexOf(const S: string): LongWord;
      (**
       * Retourne l'index de la première chaîne contenant la chaîne recherchée
       * en utilisant une recherche BM
       * @PARAM Index Position à partir de laquelle la recherche commence
       * @PARAM S Chaîne de caractères à rechercher
       *)
      function Search(const S: string; Index: LongWord=0): LongWord;
      (**
       * Recherche l'index de la ligne dans laquelle se trouve l'offset
       * @PARAM offset Offset dans le fichier de la position recherchée
       *)
      function OffsetIndex(offset: LongWord): LongWord;
      (**
       * Insère la chaîne de caractères à l'indice indiqué. Les chaînes de caractères
       * suivantes sont décalées pour permettre à la chaîne transmise d'être insérée.
       * @PARAM Index Position à laquelle doit être placée la chaîne dans le tableau
       * @PARAM S Chaîne de caractère à insérer
       *)
      procedure Insert(Index: LongWord; const S: string);
      (**
       * Remplace le contenu du tableau par le contenu du fichier indiqué. Une fois
       * les données placées dans le fichier, elles sont examinées pour retrouver
       * les éventuels séparateurs de lignes.
       * @PARAM FileName Nom du fichier contenant les données à importer
       *)
      procedure LoadFromFile(const FileName: string);
      (**
       * Remplace le contenu du tableau par le contenu du stream indiqué. Une fois
       * les données placées dans le fichier, elles sont examinées pour retrouver
       * les éventuels séparateurs de lignes.
       * @PARAM Stream TStream contenant les données à importer
       *)
      procedure LoadFromStream(Stream: TStream);
      procedure Move(CurIndex, NewIndex: LongWord);
      (**
       * Enregistre l'ensemble du fichier contenant les chaînes dans un nouveau
       * fichier (créé pour l'occasion).
       * @PARAM FileName Nom du fichier de destination
       *)
      procedure SaveToFile(const FileName: string);
      (**
       * Enregistre l'ensemble du fichier contenant les chaînes dans le stream
       * indiqué. C'est en fait une copie du fichier vers le stream.<P>
       * Les données sont copiées par bloc.
       * @PARAM Stream TStream dans lequel les données sont placées
       *)
      procedure SaveToStream(Stream: TStream);
      (**
       * Remplace le contenu du fichier par le contenu de la chaîne de caractère
       * pointée par Text. Une fois la chaîne placée dans le fichier, elle
       * est examinée pour retrouver les éventuels séparateurs de lignes.
       * @PARAM Text Pointeur PChar sur la chaîne à placer dans le fichier
       *)
      procedure SetText(Text: PChar);
      (**
       * Propriété indiquant le nombre de lignes dans le tableau (en commençant à 0)
       * @TYPE LongWord
       *)
      property Count: LongWord read GetCount;
      (**
       * Accède à n'importe quelle chaîne de caractères du tableau. C'est la propriété par
       * défaut et il n'est donc pas nécessaire de préciser le nom Strings pour y accéder.<P>
       * Si le mode le permet, cette propriété fonctionne en mode lecture / écriture.<P>
       * Equivalent aux méthodes Get et Put.
       * @TYPE String
       * @PARAM Index Indice dans le tableau
       *)
      property Strings[Index: LongWord]: string read Get write Put; default;
      (**
       * Accède à l'ensemble du fichier sous la forme d'une chaîne de caractère<P>
       * Si le mode le permet, cette propriété fonctionne en mode lecture / écriture.<P>
       * Equivalent aux méthodes GetTextStr et SetTextStr.
       * @TYPE String
       *)
      property Text: string read GetTextStr write SetTextStr;
      (**
       * Propriété en lecture seule indiquant la taille du fichier
       * dans lequel est contenu l'ensemble des chaînes en octets.
       * @TYPE LongWord
       *)
      property FileSize: LongWord read GetFileSize;
   end;

   (**
    * La classe de base pour les classes TTabFile et TTabStringFiles.<P>
    * C'est une classe qui est basée sur TInterfacedObject pour la gestion
    * du comptage de référence.<P>
    * Elle définit la plupart des méthodes utiles en protected. Les classes
    * qui en dérivent doivent les rendre publique le cas échéant.
    *
    * @INHERITS_FROM TInterfacedObject
    *)
   TInternalTabFile = class(TInterfacedObject)
   private
      FFileHandle: THandle;
      FMap: THandle;
      FPtr: PChar;
      FAccessMode: TAccessMode;
      FSize: LongWord;

   protected
      procedure CheckBounds(pos: LongWord);
      procedure CheckReadOnly;
      procedure CheckOpen;
      function GetPtr: PChar;
      function GetSize: LongWord;
      procedure SetSize(v: LongWord);
      procedure InternalSetSize(v: LongWord);
      function GetContent(pos: LongWord): Char;
      procedure SetContent(pos: LongWord; const Value: Char);
      function GetStringAccess(pos: LongWord): String;
      procedure SetStringAccess(pos: LongWord; const Value: String);
      procedure MapFileToTab;
      procedure UnmapFile;
      procedure CalcFileSize;
      procedure OpenFile(fileName: TFileName; accessMode: TAccessMode=amReadWrite); virtual;
      procedure CreateFile(fileName: TFileName; tabSize: LongWord; bOverwrite: boolean=true); virtual;
      procedure CloseFile; virtual;

      // 17/5/2002
      procedure Insert(pos: LongWord; sizebloc: LongWord; fill: byte=0);
      procedure Append(sizebloc: LongWord; fill: byte=0);
      procedure Delete(pos: LongWord; sizebloc: LongWord);

      function IsReadOnly: boolean;
      property Size: LongWord read GetSize write SetSize;
      property Content[pos: LongWord]: Char read GetContent write SetContent; default;
      property StringAccess[pos: LongWord]: String read GetStringAccess write SetStringAccess;

   public
      destructor Destroy; override;
   end;

   (**
    * C'est la classe qui définissant l'objet TTabFile. Elle implémente l'interface
    * ITabFile pour pouvoir accéder à ses fonctions.<P>
    * Elle propose deux constructeur pour la création des objets qui sont ensuite
    * utilisés via l'interface ITabFile.
    *
    * @INHERITS_FROM TInternalTabFile
    * @IMPLEMENTS ITabFile
    *)
   TTabFile = class(TInternalTabFile, ITabFile)
   public
      constructor Open(fileName: TFileName; accessMode: TAccessMode=amReadWrite);
      constructor CreateNew(fileName: TFileName; tabSize: LongWord; bOverwrite: boolean=true);
      function Offset(off: longWord): pointer;
   end;

   (**
    * Types des séparateurs de lignes reconnus. lsUndef est utilisé à l'ouverture d'un
    * fichier, lorsque le séparateur n'est pas encore défini.
    *)
   TLineSeparator = (lsUndef, lsCR, lsLF, lsCRLF);

   (**
    * C'est la classe qui définissant l'objet TTabStringsFile. Elle implémente l'interface
    * ITabStringsFile pour pouvoir accéder à ses fonctions.<P>
    * Toutes les opérations concernant la taille du fichier suite aux modifications
    * des chaînes se trouvant aux indices indiqués sont gérées automatiquement.
    * Elle propose deux constructeur pour la création des objets qui sont ensuite
    * utilisés via l'interface ITabStringFile.
    *
    * @INHERITS_FROM TInternalTabFile
    * @IMPLEMENTS ITabStringsFile
    *)
   TTabStringsFile = class(TInternalTabFile, ITabStringsFile)
   private
      FLines: TList;
      FLineSeparator: TLineSeparator;
      FLineSeparatorStr: PChar;
      FLineSeparatorSize: longword;
      FLastLineSeparator: Boolean;
      procedure SetLineSeparator(const Value: TLineSeparator);

      procedure CheckTabBounds(index: Longword);
      (**
       * Retourne des informations sur la chaîne se trouvant à l'indice précisé.<P>
       * Le résultat est placé dans les variables passées en référence.
       * @PARAM index Indice de la ligne
       * @PARAM pos Position physique dans le fichier (en octets)
       * @PARAM len Longueur de la chaîne (sans le séparateur)
       *)
      procedure GetLineInfos(index: Longword; var pos: Longword; var len: Longword);
      (**
       * Retourne des informations sur la chaîne se trouvant à l'indice précisé.
       * La fonction prend en compte le séparateur de chaîne s'il est présent (il
       * peut ne pas être présent en fin de fichier, pour la dernière chaîne)<P>
       * Le résultat est placé dans les variables passées en référence.
       * @PARAM index Indice de la ligne
       * @PARAM pos Position physique dans le fichier (en octets)
       * @PARAM len Longueur de la chaîne (avec le séparateur)
       *)
      procedure GetLineInfosSep(index: Longword; var pos: Longword; var len: Longword);

      function GetFileSize: LongWord;
      function Search(const S: string; Index: LongWord=0): LongWord;
      function OffsetIndex(offset: LongWord): LongWord;

      //
      function GetTextStr: string;
      procedure SetTextStr(const Value: string);
      function Get(Index: LongWord): string;
      procedure Put(Index: LongWord; const S: string);
      function GetCount: LongWord;
      //////////////////////////////
      function Add(const S: string): LongWord;
      procedure AddStrings(Strings: TStrings); overload;
      procedure AddStrings(Strings: ITabStringsFile); overload;
      procedure Clear;
      procedure Delete(Index: DWord);
      function Equals(Strings: ITabStringsFile): Boolean;
      procedure Exchange(Index1, Index2: LongWord);
      function GetText: PChar;
      function IndexOf(const S: string): LongWord;
      procedure Insert(Index: LongWord; const S: string);
      procedure LoadFromFile(const FileName: string);
      procedure LoadFromStream(Stream: TStream);
      procedure Move(CurIndex, NewIndex: LongWord);
      procedure SaveToFile(const FileName: string);
      procedure SaveToStream(Stream: TStream);
      procedure SetText(Text: PChar);

      (**
       * Initialisation interne de l'objet
       *)
      procedure Init;
      (**
       * Initialisation du tableau<P>
       * Recherche le séparateur.<P>
       * Contruit le tableau contenant les offsets des lignes dans le fichier.
       *)
      procedure InitLignes;

   public
      constructor Open(fileName: TFileName; accessMode: TAccessMode=amReadWrite);
      constructor CreateNew(fileName: TFileName; ls: TLineSeparator; bOverwrite: boolean=true);
      destructor Destroy; override;

      (**
       * Propriété permettant de connaître ou de préciser le séparateur de lignes.
       * @TYPE TLineSeparator
       *)
      property LineSeparator: TLineSeparator read FLineSeparator write SetLineSeparator;
   end;

implementation


{$DEFINE BMSEARCH}

{$IFDEF BMSEARCH}
uses bmsearch;
{$ENDIF}

{ TTabFile }

//
// fonction utilitaire (manque opérateur ?: )
function DWChoose(cond: Boolean; dw1, dw2: DWord): DWord;
begin
   if cond then result := dw1 else result := dw2;
end;

//
// Constructeur :
// Crée un tableau en ouvrant le fichier dans le mode indiqué
//
constructor TTabFile.Open(fileName: TFileName;
   accessMode: TAccessMode=amReadWrite);
begin
   inherited Create;
   OpenFile(fileName, accessMode);
end;

//
// Constructeur :
// Crée un fichier de la taille indiquée et ouvre le tableau
// sur ce fichier, overwrite permet de contrôler l'écrasement
// d'un ancien fichier
//
constructor TTabFile.CreateNew(fileName: TFileName; tabSize: LongWord; bOverwrite: boolean=true);
begin
   inherited Create;
   CreateFile(fileName, tabSize, bOverwrite);
end;

//
// Destructeur
//
destructor TInternalTabFile.Destroy;
begin
   CloseFile;
   inherited;
end;

//
//
function TInternalTabFile.IsReadOnly: boolean;
begin
   result := FAccessMode = amReadOnly;
end;

//
//
function TInternalTabFile.GetContent(pos: LongWord): Char;
begin
   result := FPtr[pos];
end;

//
//
procedure TInternalTabFile.SetContent(pos: LongWord; const Value: Char);
begin
   FPtr[pos] := Value;
end;

//
//
function TInternalTabFile.GetSize: LongWord;
begin
   result := FSize;
end;

//
//
function TInternalTabFile.GetPtr: PChar;
begin
   result := FPtr;
end;

//
// Génère une exception sur la position est hors limites
//
procedure TInternalTabFile.CheckBounds(pos: LongWord);
begin
   if pos>=FSize then
      raise ETabFileError.CreateFmt('Indice %d hors limites', [pos]);
end;

procedure TInternalTabFile.CheckReadOnly;
begin
   if IsReadOnly then
      raise ETabFileError.Create('Impossible de changer la taille d''un tableau en lecture seule');
end;

//
//
procedure TInternalTabFile.OpenFile(fileName: TFileName; accessMode: TAccessMode);
begin
   CloseFile;
   FAccessMode := accessMode;

   // ouvrir le fichier physiquement
   FFileHandle := Windows.CreateFile(PChar(fileName),
      DWChoose(IsReadOnly, GENERIC_READ, GENERIC_READ Or GENERIC_WRITE),
      FILE_SHARE_READ, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
   if FFileHandle=INVALID_HANDLE_VALUE then
      raise ETabFileError.CreateFmt('Erreur [%x] à l''ouverture de %s', [GetLastError, fileName]);

   // taille du fichier (et donc du tableau)
   CalcFileSize;

   // ouvre le mapping
   MapFileToTab;
end;

//
//
procedure TInternalTabFile.CloseFile;
begin
   UnmapFile;
   if FFileHandle<>0 then
      CloseHandle(FFileHandle);
   FFileHandle := 0;
end;

//
//
procedure TInternalTabFile.MapFileToTab;
begin
   CheckOpen;
   if FSize<>0 then
      try
         FMap := CreateFileMapping(FFileHandle, NIL,
            DWChoose(IsReadOnly, PAGE_READONLY, PAGE_READWRITE),
            0, 0, NIL);
         if FMap=0 then
            raise ETabFileError.CreateFmt('Erreur [%x] avec CreateFileMapping', [GetLastError]);

         FPtr := MapViewOfFile(FMap,
            DWChoose(IsReadOnly, FILE_MAP_READ, FILE_MAP_ALL_ACCESS),
            0, 0, 0);
         if FPtr=Nil then
            raise ETabFileError.CreateFmt('Erreur [%x] avec MapViewOfFile', [GetLastError]);
      except
         UnmapFile;
         raise;
      end;
end;

//
//
procedure TInternalTabFile.UnmapFile;
begin
   if FPtr<>Nil then
      UnmapViewOfFile(FPtr);
   FPtr := Nil;
   if FMap<>0 then
      CloseHandle(FMap);
   FMap := 0;
end;

//
//
procedure TInternalTabFile.CalcFileSize;
begin
   // note : pointeur 32 bits, pascal object ne gère pas
   // les pointeurs sur 64 bits
   FSize := GetFileSize(FFileHandle, Nil);
   if FSize=$FFFFFFFF then
      raise ETabFileError.CreateFmt('Erreur [%x] avec GetFileSize', [GetLastError]);
end;


//
//
procedure TInternalTabFile.InternalSetSize(v: LongWord);
begin
   SetFilePointer(FFileHandle, v, Nil, FILE_BEGIN);
   SetEndOfFile(FFileHandle);
   CalcFileSize;
end;

//
//
procedure TInternalTabFile.SetSize(v: LongWord);
begin
   CheckReadOnly;
   UnmapFile;
   InternalSetSize(v);
   MapFileToTab;
end;

//
// Génère une exception si le fichier n'est pas ouvert
//
procedure TInternalTabFile.CheckOpen;
begin
   if FFileHandle=0 then
      raise ETabFileError.Create('Fichier non ouvert');
end;


//
//
procedure TInternalTabFile.CreateFile(fileName: TFileName; tabSize: LongWord; bOverwrite: boolean);
begin
   CloseFile;
   FAccessMode := amReadWrite;

   // ouvrir le fichier physiquement
   FFileHandle := Windows.CreateFile(PChar(fileName),
      DWChoose(IsReadOnly, GENERIC_READ, GENERIC_READ Or GENERIC_WRITE),
      FILE_SHARE_READ, NIL,
      DWChoose(bOverwrite, CREATE_ALWAYS, CREATE_NEW),
      FILE_ATTRIBUTE_NORMAL, 0);
   if FFileHandle=INVALID_HANDLE_VALUE then
      raise ETabFileError.CreateFmt('Erreur [%x] à là création de %s', [GetLastError, fileName]);

   // taille du fichier (et donc du tableau)
   InternalSetSize(tabSize);

   // ouvre le mapping
   MapFileToTab;
end;

//
// retourne la chaîne se trouvant à la position P
// elle se termine sur un 0D ou 0A ou fin de fichier
function TInternalTabFile.GetStringAccess(pos: LongWord): String;
var
   i: DWord;
begin
   CheckBounds(pos);

   i := Pos;
   while (i<FSize) And (FPtr[i]<>#$d) And (FPtr[i]<>#$a) do
      inc(i);
   SetString(Result, FPtr+pos, i-pos);
end;

//
// place une chaîne de caractères à une position du fichier
// (le 0 de fin n'est pas copié)
procedure TInternalTabFile.SetStringAccess(pos: LongWord; const Value: String);
begin
   CheckBounds(pos+LongWord(Length(Value)));
   MoveMemory(FPtr+Pos, PChar(Value), Length(Value));
end;

procedure TInternalTabFile.Insert(pos, sizebloc: LongWord; fill: byte);
var
   oldsize: LongWord;
begin
   CheckReadOnly;

   if pos=FSize then
      Append(sizebloc, fill)
   else
   begin
      CheckBounds(pos);

      oldsize := Size;
      Size := oldsize + sizebloc;
      MoveMemory(FPtr + pos +sizebloc, FPtr + pos, oldsize-pos);
      FillMemory(FPtr + pos, sizebloc, fill);
   end;
end;

procedure TInternalTabFile.Append(sizebloc: LongWord; fill: byte);
var
   oldsize: LongWord;
begin
   CheckReadOnly;

   oldsize := Size;
   Size := oldsize + sizebloc;
   FillMemory(FPtr + oldsize, sizebloc, fill);
end;

procedure TInternalTabFile.Delete(pos, sizebloc: LongWord);
begin
   CheckReadOnly;
   CheckBounds(pos);

   // si la taille déborde sur la fin, ajuster la taille du bloc
   if (pos+sizebloc)>FSize then
      sizebloc := FSize-pos;

   // ne copier que si nécessaire
   if FSize>(pos+sizebloc) then
      MoveMemory(FPtr + pos, FPtr + pos + sizebloc, FSize - (pos+sizebloc));
   Size := FSize - sizebloc;
end;

(*
 * TTabStringsFile
 *)

function TTabStringsFile.Add(const S: string): LongWord;
var
   old: Longword;
   l: integer;
begin
   old := FSize;
   l := Length(s);
   // s'il n'y a pas de séparateur pour la dernière ligne
   // ajouter de la place pour le placer
   if not FLastLineSeparator then
      inc(old, FLineSeparatorSize);
   Size := old + LongWord(l) + FLineSeparatorSize;
   // s'il n'y a pas de séparateur pour la dernière ligne
   // l'ajouter
   if not FLastLineSeparator then
      CopyMemory(Fptr+old-FLineSeparatorSize, PChar(FLineSeparatorStr), FLineSeparatorSize);
   // si la chaîne n'était pas vide, l'ajouter
   if l>0 then
      CopyMemory(Fptr+old, PChar(s), l);
   // puis placer le séparateur de ligne
   CopyMemory(Fptr+old+l, PChar(FLineSeparatorStr), FLineSeparatorSize);
   FLastLineSeparator := true;
   result := FLines.Add(Pointer(old));
end;

procedure TTabStringsFile.AddStrings(Strings: TStrings);
var
   i: integer;
begin
   for i:= 0 to Strings.Count-1 do
      Add(Strings[i]);
end;

procedure TTabStringsFile.AddStrings(Strings: ITabStringsFile);
var
   l: LongWord;
begin
   for l:= 0 to Strings.Count-1 do
      Add(Strings[l]);
end;

procedure TTabStringsFile.CheckTabBounds(index: Longword);
begin
   if index>=LongWord(FLines.Count) then
      raise ETabStringsFileError.CreateFmt('Indice %d hors limites', [index]);
end;

procedure TTabStringsFile.Clear;
begin
   FLines.Clear;
   FLastLineSeparator := true;
   Size := 0;
end;

constructor TTabStringsFile.CreateNew(fileName: TFileName;
  ls: TLineSeparator; bOverwrite: boolean);
begin
   inherited Create;
   CreateFile(fileName, 0, bOverwrite);
   Init;
   InitLignes;
   LineSeparator := ls;
end;

procedure TTabStringsFile.Delete(Index: DWord);
var
   pos, len: Longword;
   i: integer;
begin
   CheckTabBounds(index);
   GetLineInfosSep(index, pos, len);
   inherited Delete(pos, len);
   // si on a effacé la dernière ligne, alors il y a forcemment un séparateur
   if integer(Index+1)=FLines.Count then
      FLastLineSeparator := true;
   FLines.Delete(index);
   // remettre à jour les positions à partir de la ligne
   for i:=index to LongWord(FLines.Count-1) do
      FLines.Items[i] := Pointer(LongWord(FLines.Items[i]) - len);
end;


destructor TTabStringsFile.Destroy;
begin
   FLines.Free;
   inherited;
end;

function TTabStringsFile.Equals(Strings: ITabStringsFile): Boolean;
var
   l, c: LongWord;
begin
   Result := False;
   c := GetCount;
   if c <> Strings.Count then
      exit;
   for l := 0 to c-1 do
      if Get(l) <> Strings.Get(l) then
         exit;
   result := True;
end;

procedure TTabStringsFile.Exchange(Index1, Index2: LongWord);
var
   tmp: string;
begin
   tmp := Get(Index1);
   Put(Index1, Get(Index2));
   Put(Index2, tmp);
end;

function TTabStringsFile.Get(Index: LongWord): string;
var
   pos, len: Longword;
begin
   CheckTabBounds(index);
   GetLineInfos(index, pos, len);
   SetString(Result, FPtr+pos, len);
end;

function TTabStringsFile.GetCount: LongWord;
begin
   result := FLines.Count;
end;

function TTabStringsFile.GetFileSize: LongWord;
begin
   result := FSize;
end;

procedure TTabStringsFile.GetLineInfos(index: Longword; var pos, len: Longword);
var
   i: Longword;
begin
   // position du début de la ligne
   pos := Longword(FLines.items[Index]);
   // si on est sur la dernière ligne
   if (Index+1)=LongWord(FLines.Count) then
   begin
      // alors la fin de la ligne est la fin du fichier
      i := FSize;
      if FLastLineSeparator then
         dec(i, FLineSeparatorSize);
   end
   else
      i := Longword(FLines.items[integer(Index+1)]) - FLineSeparatorSize;
   len := i - pos;
end;


// retourne la taille incluant le séparateur s'il est là
procedure TTabStringsFile.GetLineInfosSep(index: Longword; var pos,
  len: Longword);
begin
   // position du début de la ligne
   pos := Longword(FLines.items[Index]);
   // si on est sur la dernière ligne
   if integer(Index+1)=FLines.Count then
      len := FSize-pos
   else
      len := Longword(FLines.items[integer(Index+1)])-pos;
end;

function TTabStringsFile.GetText: PChar;
begin
   result := AllocMem(Size+1);
   MoveMemory(result, GetPtr, Size);
   result[size] := #0;
end;

function TTabStringsFile.GetTextStr: string;
begin
   SetString(Result, nil, Size);
   MoveMemory(@result[1], GetPtr, Size);
end;

function TTabStringsFile.IndexOf(const S: string): LongWord;
begin
   for Result := 0 to GetCount - 1 do
      if AnsiCompareText(Get(Result), S) = 0 then Exit;
   Result := $FFFFFFFF;
end;

procedure TTabStringsFile.Init;
begin
   FLines := TList.Create;
   LineSeparator := lsUndef;
end;

procedure TTabStringsFile.InitLignes;
var
   i, j: Longword;
begin
   FLines.Clear;

   // si le fichier a une taille non nulle
   if FSize<>0 then
   begin
      // trouver le séparateur
      for i:=0 to FSize-1 do
      begin
         if FPtr[i]=#13 then
         begin
            LineSeparator := lsCR;
            if i+1<FSize then
               if FPtr[i+1]=#10 then
                  LineSeparator := lsCRLF;
            break;
         end;
         if FPtr[i]=#10 then
         begin
            LineSeparator := lsLF;
            break;
         end;
      end;
      // s'il n'y a pas de séparateur, utiliser CRLF
      if LineSeparator = lsUndef then
         LineSeparator := lsCRLF;

      FLastLineSeparator := false;

      // trouver les lignes
      i := 0;
      FLines.Add(Pointer(i));
      repeat
         j := 0;
         while (i+j<FSize) And (FLineSeparatorStr[j]<>#0) do
         begin
            if FPtr[i+j]<>FLineSeparatorStr[j] then
               break;
            inc(j);
         end;
         // si on est sur un séparateur (tous les caractères du séparateur ont été lus)
         if FLineSeparatorStr[j]=#0 then
         begin
            inc(i, j);  // i est sur le début de la chaîne suivant le séparateur
            // n'ajouter la chaîne que si elle n'est pas vide
            if i<FSize then
               FLines.Add(Pointer(i))
            else
               FLastLineSeparator := true;
         end
         else
            inc(i);
      until i >= FSize;
   end
   else
      FLastLineSeparator := true;

   // s'il n'y a pas de séparateur, utiliser CRLF
   if LineSeparator = lsUndef then
      LineSeparator := lsCRLF;
end;

procedure TTabStringsFile.Insert(Index: LongWord; const S: string);
var
   pos: LongWord;
   len: LongWord;
   i: longWord;
begin
   CheckReadOnly;

   if Index=LongWord(FLines.Count) then
      Add(s)
   else
   begin
      CheckBounds(index);

      len := Length(s);
      pos := LongWord(FLines.Items[index]);
      inherited Insert(pos, len + FLineSeparatorSize);

      MoveMemory(FPtr + pos, @s[1], len);
      MoveMemory(FPtr + pos + len, FLineSeparatorStr, FLineSeparatorSize);

      FLines.Insert(Index, Pointer(pos));

      // remettre à jour les positions à partir de la ligne
      for i:=index+1 to LongWord(FLines.Count-1) do
         FLines.Items[i] := Pointer(LongWord(FLines.Items[i]) + len + FLineSeparatorSize);
   end;
end;

procedure TTabStringsFile.LoadFromFile(const FileName: string);
var
   str: TFileStream;
begin
   str := TFileStream.Create(FileName, fmOpenRead);
   try
      LoadFromStream(str);
   finally
      str.Free;
   end;
end;

procedure TTabStringsFile.LoadFromStream(Stream: TStream);
var
   offset: integer;
   n: integer;
const
   blocsize = 4096;
begin
   offset := 0;
   Size := 0;
   repeat
      Size := Size + blocsize;
      n := Stream.Read((FPtr+offset)^, blocsize);
      inc(offset, n);   // prochaine lecture
   until n<>blocsize;
   Size := offset;
   InitLignes;
end;

procedure TTabStringsFile.Move(CurIndex, NewIndex: LongWord);
var
   tmp: string;
begin
   if CurIndex <> NewIndex then
   begin
      tmp := Get(CurIndex);
      Delete(CurIndex);
      Insert(NewIndex, tmp);
   end;
end;

(*
 * Recherche dichotomique de la ligne dans laquelle se trouve l'offset indiqué
 *)
function TTabStringsFile.OffsetIndex(offset: LongWord): LongWord;
var
   low, high: LongWord;
   i: Longword;
   inddeb, indfin: Longword;
begin
   Result := $FFFFFFFF;
   low := 0;
   high := FLines.Count-1;

   while low <= high do
   begin
      // couper en 2
      i := low + ((high - low) shr 1);

      // rechercher les bornes de la ligne
      inddeb := Longword(FLines.items[i]);
      if (i+1)=LongWord(FLines.Count) then
         // alors la fin de la ligne est la fin du fichier
         indfin := FSize
      else
         indfin := Longword(FLines.items[integer(i+1)]);


      if offset < inddeb then
         high := i-1
      else if offset >= indfin then
         low := i+1
      else
      begin
         Result := i;
         break;
      end;
   end;
end;

constructor TTabStringsFile.Open(fileName: TFileName;
  accessMode: TAccessMode);
begin
   inherited Create;
   OpenFile(fileName, accessMode);
   Init;
   InitLignes;
end;

procedure TTabStringsFile.Put(Index: LongWord; const S: string);
var
   pos, len: Longword;
   l: Longword;
   p: Longword;
   off, i: integer;
begin
   CheckTabBounds(index);
   GetLineInfos(index, pos, len);

   l := Length(s);
   p := LongWord(FLines.Items[index]);

   off := integer(l)-integer(len); // décalage

   if off>0 then
      Inherited Insert(p, l-len)
   else if off<0 then
      Inherited Delete(p, len-l);
   MoveMemory(FPtr + p, @s[1], l);

   // remettre à jour les positions à partir de la ligne
   for i:=index+1 to LongWord(FLines.Count-1) do
      FLines.Items[i] := Pointer(Integer(FLines.Items[i]) + off);
end;

procedure TTabStringsFile.SaveToFile(const FileName: string);
var
   f: TFileStream;
begin
   f := TFileStream.Create(FileName, fmOpenWrite);
   try
      SaveToStream(f);
   finally
      f.Free;
   end;
end;

procedure TTabStringsFile.SaveToStream(Stream: TStream);
const
   blocsize=4096; // une page
var
   i: longword;
begin
   i := 0;
   while Stream.Write(FPtr[i], blocsize)=blocsize do
      inc(i, blocsize);
end;

function TTabStringsFile.Search(const S: string; Index: LongWord=0): LongWord;
var
   bms: IBMSearch;
   pos: LongWord;
begin
   CheckBounds(index);
   // pas de recherche de chaîne vide
   if Length(s)=0 then
   begin
      Result := $FFFFFFFF;
      exit;
   end;

   // recherche d'une chaîne dans le tableau
   bms:= TBMSearch.Create(s);
   Result := bms.Search(FPtr, Size, Longword(FLines.items[Index]));
   // si la chaîne est trouvée
   if Result<>$FFFFFFFF then
   begin
      // position du début de la ligne
      result := OffsetIndex(Result);
   end;
end;

procedure TTabStringsFile.SetLineSeparator(const Value: TLineSeparator);
begin
   FLineSeparator := Value;
   case FLineSeparator of
      lsUndef: FLineSeparatorStr := '';
      lsCR:    FLineSeparatorStr := #13;
      lsLF:    FLineSeparatorStr := #10;
      lsCRLF:  FLineSeparatorStr := #13#10;
   end;
   FLineSeparatorSize := StrLen(FLineSeparatorStr);
end;

procedure TTabStringsFile.SetText(Text: PChar);
begin
   Size := StrLen(Text);
   MoveMemory(FPtr, text, FSize);
   InitLignes;
end;

procedure TTabStringsFile.SetTextStr(const Value: string);
begin
   SetText(PChar(Value));
end;



   (**
    * Fonction comparant 2 chaînes pour le tri
    *)
//type   TCompareTabStrings = function(l, h: integer; tsf: TTabStringsFile): integer;




{procedure QuickSort(var tab: array of integer; l, r: Integer; SCompare: TListSortCompare);
var
  i, j: Integer;
  p, t: Pointer;
begin
  repeat
    i := l;
    j := r;
    p := SortList^[(l + r) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do
        Inc(I);
      while SCompare(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;
}

{
procedure TTabStringsFile.SortToStream(str: TStream);
var
   i: integer;
   ti: Array of integer;

begin
   SetLength(ti, FLines.Count);
   for i:=low(ti) to high(ti) do
      ti[i]:=i;
end;
}

function TTabFile.Offset(off: longWord): pointer;
begin
   result := FPtr + off;
end;

end.
