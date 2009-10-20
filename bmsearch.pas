(*
bmsearch

Algo de recherche de chaîne Booyer-Moore

Jean Lacoste 2001
*)
unit bmsearch;

interface

uses windows, sysutils;

type
   //
   //
   IBMSearch = interface
      function Search(pBuffer: PChar; lenBuffer: integer; offStart: integer=0): integer;
   end;

   //
   //
   TBMSearch = class(TInterfacedObject, IBMSearch)
   private
      FTrans: array[0..255] of byte;
      FString: String;
      FPtr: PChar;
      FLen: integer;
      procedure Init(p: PChar; l: integer);
      function InternalBM(pChaine, pWhere: PChar; lenChaine, lenBuffer: integer): integer;
      function Search(pBuffer: PChar; lenBuffer: integer; offStart: integer=0): integer;
   public
      constructor Create(toFind: String); overload;
      constructor Create(toFind: PChar; l: integer); overload;
      destructor Destroy; override;
   end;


implementation

{ TBMSearch }

constructor TBMSearch.Create(toFind: String);
begin
   FString := toFind;
   FPtr := PChar(FString);
   Create(FPtr, length(toFind));
end;

constructor TBMSearch.Create(toFind: PChar; l: integer);
begin
   inherited Create;
   Init(toFind, l);
end;

destructor TBMSearch.Destroy;
begin
   inherited;
end;

//
// mise en place du tableau
procedure TBMSearch.Init(p: PChar; l: integer);
var
   i: integer;
   j: byte;
   len: byte;
begin
   FLen := l;
   if l>255 then
      len := 255
   else
      len := byte(l);

   for i:=0 to 255 do
      FTrans[i]:=len;

   for i:=l-1 downto 0 do
   begin
   	j:=Byte(p[i]);
      if FTrans[j]=len then
         FTrans[j] := len-i-1;
   end;
end;

//
//
function TBMSearch.InternalBM(pChaine, pWhere: PChar; lenChaine, lenBuffer: integer): integer;
var
   kx, ki, kj: integer;
begin
   // adaptation d'un vieil algo écrit en C
   result := -1;
   ki:=lenChaine-1;
   kj:=lenChaine-1;
   while kj>=0 do
   begin
      while pWhere[ki] <> pChaine[kj] do
      begin
         kx:=FTrans[integer(pWhere[ki])];
         if lenChaine-kj>kx then
            inc(ki, lenChaine-kj)
         else
            inc(ki, kx);
         if ki>=lenBuffer then
            exit;
         kj := lenChaine-1;
      end;
      dec(ki);
      dec(kj);
   end;
   result := ki+1;
end;

//
//
function TBMSearch.Search(pBuffer: PChar; lenBuffer: integer; offStart: integer): integer;
begin
   // pas la peine d'essayer de chercher après la fin du buffer
   if (FLen+offStart)>lenBuffer then
      result := -1
   else
      // rechercher
      result := InternalBM(FPtr, pBuffer+offStart, FLen, lenBuffer-offStart);
   // si la recherche a aboutie, ajouter l'offset transmis
   if result <> -1 then
      inc(result, offStart);
end;

end.
