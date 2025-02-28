unit DLG_AssetAdd;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, LCLType, Menus, EditBtn, Market, HeartBeat,
  Data_Provider, Data_Assets, Types;

type
  TSearchAddThread = class(TThread)
   private
     fSearchTerm: string;
     fAssetList : TAssetArray;
     fOnSearchComplete: TNotifyEvent;

     fFilterByType     : Boolean;
     fTypeFilter       : TAssetType;
     fFilterByCurrency : Boolean;
     fCurrencyFilter   : TCurrencyType;
   public
     constructor Create(aSearchTerm : String;
        aByType : Boolean;  aTypeFilter : TAssetType;
        aByCurrency : Boolean; aCurrencyFilter : TCurrencyType);
     procedure Execute; override;
     property OnSearchComplete: TNotifyEvent read FOnSearchComplete write FOnSearchComplete;
   end;

type

  { TDLGAssetAdd }

  TDLGAssetAdd = class(TForm)
    Bevel1: TBevel;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    cmbCurrency: TComboBox;
    cmbType: TComboBox;
    ComboBox1: TComboBox;
    edtSearch: TEditButton;
    grpSearch: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    pnlButtons: TPanel;
    procedure cmbTypeChange(Sender: TObject);
    procedure cmbTypeDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure edtSearchButtonClick(Sender: TObject);
    procedure edtSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstFoundDblClick(Sender: TObject);
    procedure lstFoundShowHint(Sender: TObject; HintInfo: PHintInfo);

  private
    fSearchThread   : TSearchAddThread;
    fAssetFoundList : TAssetArray;
    fAssetToAddList : TAssetArray;

    fValidAssetTypes : array of TAssetType;

    procedure OnSearchComplete(Sender: TObject);
    function AssetToStringItem(aAsset: TAsset) : String;

  public
    property AssetsToAdd : TAssetArray read fAssetToAddList;

  published
    btnRemove: TSpeedButton;
    lblSearching: TLabel;
    lstFound: TListBox;
    lstToAdd: TListBox;
    pnlFound: TPanel;
    pnlButton: TPanel;
    pnlToAdd: TPanel;
    btnAdd: TSpeedButton;
    TimerBlink: TTimer;
    TimerSearch: TTimer;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstFoundSelectionChange(Sender: TObject; User: boolean);
    procedure lstToAddSelectionChange(Sender: TObject; User: boolean);
    procedure TimerBlinkTimer(Sender: TObject);
    procedure TimerSearchTimer(Sender: TObject);
  private
    //fLastSearch : String;

    procedure _KillSearch;
    function _ValidSearchString : Boolean;

  end;

implementation

uses Tools;

{$R *.lfm}

{ TDLGAssetAdd }

// *****************************************************************************
// * HILO DE LA BUSQUEDA                                                       *
// *****************************************************************************

constructor TSearchAddThread.Create(aSearchTerm : String; aByType : Boolean;  aTypeFilter : TAssetType;
   aByCurrency : Boolean; aCurrencyFilter : TCurrencyType);
begin
  inherited Create(True);
  fSearchTerm := Trim(aSearchTerm);

  fFilterByType     := aByType;
  fTypeFilter       := aTypeFilter;
  fFilterByCurrency := aByCurrency;
  fCurrencyFilter   := aCurrencyFilter;
end;

procedure TSearchAddThread.Execute;
var
  Asset : TAsset;
  aList: TAssetArray;
  i : Integer;
  Flag : Boolean;

  procedure AddAsset(var i: Integer; aAsset: TAsset);
  begin
    if HeartBeat_IsValidType(Asset.ID.TypeAsset) then // Solo HB validos
     begin
      SetLength(fAssetList, i + 1);
      fAssetList[i] := aAsset;
      Inc(i);
     end;
    //else WriteLN(Asset.ID.Symbol, ' filtrado - Tipo HB Invalido');
  end;

begin
  // Buscar en el origen --------------------------
  DataProvider.Search(fSearchTerm, aList);
  // ----------------------------------------------

  // Filtar resultado para HB
  i := 0;
  for Asset in aList do
   if fFilterByType or fFilterByCurrency then
    begin
     Flag := True;
     if fFilterByType     then Flag := Flag and (fTypeFilter     = Asset.ID.TypeAsset);
     if fFilterByCurrency then Flag := Flag and (fCurrencyFilter = Asset.Currency);

     if Flag then AddAsset(i, Asset);
    end
   else AddAsset(i, Asset);

  // Disparar evento para indicar que la búsqueda ha finalizado
  if Assigned(FOnSearchComplete) then
   TThread.Synchronize(nil, TThreadMethod(FOnSearchComplete));
end;

procedure TDLGAssetAdd.OnSearchComplete(Sender: TObject);
var
  Asset : TAsset;
begin
  if Assigned(fSearchThread) then
   begin
    lstFound.Items.Clear;
    SetLength(fAssetFoundList, 0);

    for Asset in fSearchThread.fAssetList do
     begin
      lstFound.Items.Add(AssetToStringItem(Asset));
      SetLength(fAssetFoundList, Length(fAssetFoundList) + 1);
      fAssetFoundList[High(fAssetFoundList)] := Asset;
     end;
   end;

  lstFoundSelectionChange(nil, False);
  lstFound.Enabled := True;
  //btnSave.enabled := not(lstToAdd.Items.Count = 0);
end;

////////////////////////////////////////////////////////////////////////////////
// DIALOGO                                                                    //
////////////////////////////////////////////////////////////////////////////////

procedure TDLGAssetAdd.FormCreate(Sender: TObject);
var
  i : Integer;
  AssetType    : TAssetType;
begin
  fSearchThread := nil;

  // Cargar filtros =====================================
  with cmbCurrency.Items do
   begin
    Add('Todas las monedas');
    for i := Ord(Low(TCurrencyType)) to Ord(High(TCurrencyType)) do
      Add(CurrencyTypeToStr(TCurrencyType(i)));
    Move(1, Count - 1); // Unknow al final
  end;

  i := 0;
  with cmbType.Items do
   begin
    Add('Todos los tipos');
    for AssetType in HB_AssetTypeValid do
     begin
      Add(AssetTypeToStr(AssetType));
      SetLength(fValidAssetTypes, i + 1);
      fValidAssetTypes[i] := AssetType;
      Inc(i);
     end;
    //Move(1, Count - 1); // Unknow al final
  end;

  cmbType.ItemIndex := 0;
  cmbCurrency.ItemIndex := 0;
  lstFoundSelectionChange(nil, False);
  lstToAddSelectionChange(nil, False);
end;

procedure TDLGAssetAdd.FormDestroy(Sender: TObject);
begin
  _KillSearch;
end;

procedure TDLGAssetAdd.FormShow(Sender: TObject);
begin
  edtSearch.SetFocus;
end;

// *****************************************************************************
// * BUSCAR                                                                    *
// *****************************************************************************

procedure TDLGAssetAdd.edtSearchButtonClick(Sender: TObject);
begin
  edtSearch.Text := '';
  _KillSearch;
end;

// Dispara busqueda con el timer =============================================
procedure TDLGAssetAdd.edtSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  TimerSearch.Enabled := False;

  if _ValidSearchString then
   begin
     _KillSearch;

    // Si tecla ENTER fue presionado dispar directamente la busqueda
    if (Key = VK_RETURN) and (Shift = []) then
     TimerSearchTimer(Self) // NOTA: Esto detiene el timer
    else TimerSearch.Enabled := True; // Disparar Timer
   end;
end;

procedure TDLGAssetAdd.cmbTypeChange(Sender: TObject);
begin
  if _ValidSearchString then TimerSearchTimer(Self);
end;

procedure TDLGAssetAdd.cmbTypeDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  Txt : String;
  X, Y: Integer;
begin
  with (Control as TComboBox), Canvas do
   begin
    if odSelected in State then
     begin
      Brush.Color := clHighlight;
      Font.Color  := clHighlightText;
     end;

     // Rellenar el fondo del ítem
     FillRect(ARect);

     // Calcular las coordenadas para centrar el texto
     Txt := Items[Index];
     X := ARect.Left + (ARect.Width  - TextWidth (Txt)) div 2;
     Y := ARect.Top  + (ARect.Height - TextHeight(Txt)) div 2;
     TextOut(X, Y, Txt); // Dibujar el texto centrado
  end;
end;

// *****************************************************************************
// * BUSQUEDA                                                                  *
// *****************************************************************************

function TDLGAssetAdd._ValidSearchString : Boolean;
begin
  // Al menos menos dos letras y sin espacios en blanco
  Result := (Length(edtSearch.Text) >= 2) and not(IsEmptyString(edtSearch.Text));
end;

procedure TDLGAssetAdd._KillSearch;
begin
  // Terminar cualquier otro hilo que se este ejecutando
  if Assigned(fSearchThread) then
   begin
    fSearchThread.Terminate;
    fSearchThread.WaitFor;
    FreeAndNil(fSearchThread);
   end;
end;

// Lanzar busqueda cuando el tiempo se cumple ==================================
procedure TDLGAssetAdd.TimerSearchTimer(Sender: TObject);
var
  ByType         : Boolean;
  FilterType     : TAssetType;
  ByCurrency     : Boolean;
  FilterCurrency : TCurrencyType;
begin
  TimerSearch.Enabled := False; // Detener timer
  _killSearch;  // Terminar cualquier otro hilo que se este ejecutando

  // Indicar segundo plano ------------------------------
  lblSearching.Caption := 'Buscando ...';
  TimerBlink.Enabled := True;

  // Especificar filtro ==========================================
  ByType := False;
  with cmbType do
   if not (ItemIndex = 0) then
    begin
     ByType := True;
     FilterType := fValidAssetTypes[ItemIndex - 1]
    end
   else FilterType := asstUNKNOWN;

  ByCurrency := False;
  with cmbCurrency do
   if not (ItemIndex = 0) and not(ItemIndex = (Items.Count - 1)) then
    begin
     ByCurrency := True;
     FilterCurrency := TCurrencyType(ItemIndex)
    end
   else FilterCurrency := crcyUNKNOWN;

  // Crear el hilo de busqueda =====================================
  fSearchThread:= TSearchAddThread.Create(edtSearch.Text, ByType, FilterType, ByCurrency, FilterCurrency);

  fSearchThread.OnSearchComplete := @OnSearchComplete;
  fSearchThread.Start;
  lstFound.Enabled := False; { #todo : Implementar alguna forma de ReadOnly }
end;

// TIMER BLINK ========================================================
procedure TDLGAssetAdd.TimerBlinkTimer(Sender: TObject);
begin
  lblSearching.Visible := not (lblSearching.Visible);
  if (Assigned(fSearchThread)) then
   if fSearchThread.Finished then
    begin
     lblSearching.Hide;
     TimerBlink.Enabled := False;
    end;

  Refresh;
end;

// *****************************************************************************
// * INTERACCION ENTRE LAS LISTAS                                              *
// *****************************************************************************

function TDLGAssetAdd.AssetToStringItem(aAsset: TAsset) : String;
begin
  with aAsset do
   Result := ID.Symbol +
    ' [' + AssetTypeToStr(ID.TypeAsset, False) + '] - ' + Description;
end;

procedure TDLGAssetAdd.lstFoundSelectionChange(Sender: TObject; User: boolean);
begin
  btnAdd.Enabled  := not (lstFound.SelCount = 0);
end;

procedure TDLGAssetAdd.lstToAddSelectionChange(Sender: TObject; User: boolean);
begin
  btnRemove.Enabled := not (lstToAdd.SelCount = 0);
  btnSave.Enabled   := not (lstToAdd.Count = 0);
end;

procedure TDLGAssetAdd.btnAddClick(Sender: TObject);
var
  i, j: Integer;
  SelectedAsset: TAsset;
  AssetFound: Boolean;
begin
  for i := 0 to lstFound.Items.Count - 1 do
    if lstFound.Selected[i] then
      begin
        SelectedAsset := fAssetFoundList[i];
        AssetFound := False;

        // Buscar si ya está en la lista de añadir
        for j := 0 to High(fAssetToAddList) do
         if SelectedAsset.ID = fAssetToAddList[j].ID then
          begin
           AssetFound := True;
           Break;
          end;

        // Si no está en el ListBox, añadirlo
        if not AssetFound then
          begin
            lstToAdd.Items.Add(AssetToStringItem(SelectedAsset));
            SetLength(fAssetToAddList, Length(fAssetToAddList) + 1);
            FAssetToAddList[High(FAssetToAddList)] := SelectedAsset;
          end;
      end;

  lstFoundSelectionChange(nil, False);
  lstToAddSelectionChange(nil, False);
end;

procedure TDLGAssetAdd.lstFoundDblClick(Sender: TObject);
begin
  btnAdd.Click;
end;

procedure TDLGAssetAdd.lstFoundShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  ItemIndex: Integer;
begin
  // Obtén el índice del ítem bajo el cursor
  ItemIndex := lstFound.ItemAtPos(HintInfo^.CursorPos, True);

  // Si el índice es válido, asigna un hint personalizado
  if not (ItemIndex = -1) then
   with fAssetFoundList[ItemIndex] do
    HintInfo^.HintStr := ID.Symbol + ' (' + ID.Market + ')' + sLineBreak +
                          Description + sLineBreak + AssetTypeToStr(Id.TypeAsset) +
                          ' en ' + CurrencyTypeToStr(Currency)
  else
    HintInfo^.HintStr := ''; // Sin hint si no hay ítem
end;

procedure TDLGAssetAdd.btnRemoveClick(Sender: TObject);
var
  i: Integer;
begin
  for i := lstToAdd.Items.Count - 1 downto 0 do
    if lstToAdd.Selected[i] then
      begin
        lstToAdd.Items.Delete(i);
        if i < High(fAssetToAddList) then
          Move(fAssetToAddList[i + 1], fAssetToAddList[i], (High(fAssetToAddList) - i) * SizeOf(TAsset));
        SetLength(fAssetToAddList, Length(fAssetToAddList) - 1);
      end;

  lstToAddSelectionChange(nil, False);
end;


end.

