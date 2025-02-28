unit DLG_Assets_FillList;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, DB, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ButtonPanel, ExtCtrls, Buttons, Thread_SearchBase, Market;


Type

  { TSearchThread }
  TSearchThread = class(TThread)
  private
    fSearchTerm: String;
    fList: TAssetArray;
  protected
    procedure Execute; override;
  public
    constructor Create(const SearchTerm: String);
    property List: TAssetArray read fList;
  end;


Type

  { TDLGAssetsFillList }

  TDLGAssetsFillList = class(TForm)
    imglstButton: TImageList;
    lblCount: TLabel;
    Panel1: TPanel;
    pnlButtons: TButtonPanel;
    lblProgress: TLabel;
    barProgress: TProgressBar;
    btnStart: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    fTblWork : TBufDataset;
    fProcessing : Boolean;
    fStop : Boolean;

    procedure _MsgProgress(const aMsg : String);
    procedure _MsgProgressCount(const aMsg : String);

    procedure _btnStart;
    procedure _btnStop;

    procedure _SearchProcess;
  protected

  end;


implementation

uses Data_Assets, HeartBeat, LCLType, Data_Provider, Tools;

{$R *.lfm}

const
  VALID_CHARACTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789. ';
  BTN_START  = '&Iniciar';
  BTN_STOP   = '&Detener';

// =============================================================================
// == TSearchThread
// =============================================================================

constructor TSearchThread.Create(const SearchTerm: String);
begin
  inherited Create(True);
  fSearchTerm := SearchTerm;
end;

procedure TSearchThread.Execute;
begin
  DataProvider.Search(fSearchTerm, fList);
end;


// =============================================================================
// == DIALOGO
// =============================================================================

procedure TDLGAssetsFillList.FormCreate(Sender: TObject);
begin
  lblProgress.Caption  := '';
  fProcessing := False;
  _btnStart;
end;

///////////////////////////////////////////////////////////////////////////////
// GUI

procedure TDLGAssetsFillList._btnStart;
begin
  with btnStart do
   begin
    Caption := BTN_START;
    ImageIndex := 0;
    Down := False;
   end;
end;

procedure TDLGAssetsFillList._btnStop;
begin
  with btnStart do
   begin
    Caption := BTN_STOP;
    ImageIndex := 1;
    Down := True;
   end;
end;

procedure TDLGAssetsFillList._MsgProgress(const aMsg : String);
begin
  lblProgress.Caption := aMsg;
  Application.ProcessMessages;
end;

procedure TDLGAssetsFillList._MsgProgressCount(const aMsg : String);
begin
  lblCount.Caption := aMsg;
  Application.ProcessMessages;
end;

// =============================================================================
// == GUI
// =============================================================================

procedure TDLGAssetsFillList.btnStartClick(Sender: TObject);
begin
  fStop := fProcessing;
  if not fProcessing then
   begin
    FProcessing := True;
    _btnStop;
    _SearchProcess;
   end;
end;

procedure TDLGAssetsFillList._SearchProcess;
var
  i, j, Max  : Integer;
  SearchTerm : String;
  SearchThread : TSearchThread;
  aList: TAssetArray;

  CountFind : Integer;
  Asset : TAsset;
  Flag : Boolean;

begin
  Max := Length(VALID_CHARACTERS) * Length(VALID_CHARACTERS);

  // Armar términos a buscar =============================
  for i := 1 to Length(VALID_CHARACTERS) do
   begin
    for j := 1 to Length(VALID_CHARACTERS) do
     begin
      SearchTerm := VALID_CHARACTERS[i] + VALID_CHARACTERS[j];
      barProgress.Position := Trunc(((i - 1) * Length(VALID_CHARACTERS) + j) / Max * 100);;
      _MsgProgress('Buscando ' + SearchTerm + ' ...');

      // Buscar ---------------------------------------
      if fStop then Break;
      SearchThread := TSearchThread.Create(SearchTerm);
      SearchThread.Start;

      while not SearchThread.Finished do
        Application.ProcessMessages;

      aList := SearchThread.List;
      SearchThread.Free;
      if fStop then Break;
      // ----------------------------------------------

      // Agregar -------------------------------------
      CountFind:=0;
      for Asset in aList do
       with Asset do
        if HeartBeat_IsValidType(Asset.ID.TypeAsset) then // Solo HB validos
         begin
          Application.ProcessMessages;
          if fStop then Break;

          // Agregar ==============================
          Flag := (Currency = crcyARS);
          Flag := Flag and ( (ID.TypeAsset = asstSTOCK) or (ID.TypeAsset = asstCDAR) or
                             (ID.TypeAsset = asstTREASURY_BILL) or (ID.TypeAsset = asstBOND) );

          if Flag then DataAssets.Add(Asset); // Agregar
         end;

      _MsgProgressCount(SearchTerm + ': ' + IntToStr(CountFind) + ' encontrados');
      if fStop then Break;
     end;

    if fStop then Break;
   end;

  _MsgProgress('- Proceso ' + IfThenStr(fStop, 'detenido' ,'completado') + ' -');
  _btnStart;
end;

end.



