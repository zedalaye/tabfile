(*
 * $Id: frmTestVisu.pas,v 1.2 2004/09/16 12:18:33 Jean Exp $
 *)

unit frmTestVisu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tabfile, Grids, StdCtrls, ExtCtrls;

type
  TfrmVisu = class(TForm)
    DrawGrid1: TDrawGrid;
    Panel1: TPanel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
  private
    { Déclarations privées }
    FFile: ITabStringsFile;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent; tsf: ITabStringsFile); reintroduce;
  end;

var
  frmVisu: TfrmVisu;

implementation

{$R *.DFM}

procedure TfrmVisu.FormCreate(Sender: TObject);
begin
   Label1.Caption := Format('%d lignes', [FFile.Count]);
   DrawGrid1.Align := alClient;
   DrawGrid1.ColWidths[1] := 500;
   DrawGrid1.RowCount := FFile.Count;
end;

procedure TfrmVisu.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
   r: TRect;
begin
   if ARow < FFile.Count then
   begin
      CopyRect(r, Rect);
      InflateRect(r, -3, -1);
      case ACol of
         0: DrawText(DrawGrid1.Canvas.Handle, PChar(IntToStr(ARow)), -1, r,
            DT_SINGLELINE Or DT_VCENTER Or DT_RIGHT Or DT_EXTERNALLEADING);
         1: DrawText(DrawGrid1.Canvas.Handle, PChar(FFile[ARow]), -1, r,
            DT_EXPANDTABS Or DT_SINGLELINE Or DT_VCENTER Or DT_LEFT Or DT_EXTERNALLEADING);
      end;
   end;
end;

constructor TfrmVisu.Create(AOwner: TComponent; tsf: ITabStringsFile);
begin
   FFile := tsf;
   inherited Create(AOwner);
end;

end.
