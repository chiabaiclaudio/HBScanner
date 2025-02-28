unit Tab_Filter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ExtCtrls, Buttons, cyCheckbox, Types, ksplitter;

Type
  TRange = record
    Text   : String;
    ValueA : Integer;
    ValueB : Integer;
  end;

Type
  TRangeArray = array of TRange;

type

  { TtabFilter }

  TtabFilter = class(TForm)
    btnResetFilters: TSpeedButton;
    chckOnlyFavorite: TCheckBox;
    chckOnlyPortfolio: TCheckBox;
    chckBlackListed: TCheckBox;
    chckHBVolConfirm: TCheckBox;
    cmbCATEGORY: TComboBox;
    cmbCURRENCY: TComboBox;
    cmbHBRange: TComboBox;
    cmbVOLPERCENT: TComboBox;
    cmbTYPE: TComboBox;
    cmbPRICE: TComboBox;
    edtSYMBOL: TEditButton;
    KSplitter1: TKSplitter;
    Label1: TLabel;
    stxtTYPE: TStaticText;
    stxtHBRange: TStaticText;
    stxtCATEGORY: TStaticText;
    stxtPRICE: TStaticText;
    stxtCURRENCY: TStaticText;
    stxtSYMBOL: TStaticText;
    procedure btnResetFiltersClick(Sender: TObject);
    procedure chckOnlyFavoriteChange(Sender: TObject);
    procedure chckOnlyPortfolioChange(Sender: TObject);
    procedure chckBlackListedChange(Sender: TObject);
    procedure chckHBVolConfirmChange(Sender: TObject);
    procedure cmbCATEGORYChange(Sender: TObject);
    procedure cmbCURRENCYChange(Sender: TObject);
    procedure cmbHBRangeChange(Sender: TObject);
    procedure cmbPRICEChange(Sender: TObject);
    procedure cmbTYPEChange(Sender: TObject);
    procedure cmbTYPEDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure cmbVOLPERCENTChange(Sender: TObject);
    procedure edtSYMBOLButtonClick(Sender: TObject);
    procedure edtSYMBOLChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fPriceRanges  : TRangeArray;
    fVolumeRanges : TRangeArray;
    procedure _AddRange(aText : String; aMax : Integer; aMin : Integer; var aArray : TRangeArray; acmbBox : TComboBox);

    procedure _SetTYPEItems;
    procedure _SetCATEGORYItems;
    procedure _SetPRICERangesItems;
    procedure _SetCURRENCYItems;

    procedure _RefreshChecked (chckBox : TCheckBox);
    procedure _RefreshBtnReset;
  public

  end;

var
  tabFilter: TtabFilter;

implementation

uses Market, HeartBeat, Config, LCLType, Data_Assets, Tools, Data_Assets_Const;

{$R *.lfm}

const
  NO_VALUE = FLD_MARK_CATEGORY_NOVALUE;

{ TtabFilter }

procedure TtabFilter.FormCreate(Sender: TObject);
begin
  // Crear los items ---------------------
  _SetTYPEItems;
  _SetCATEGORYItems;
  _SetPRICERangesItems;
  _SetCURRENCYItems;

  // Inicializar todos los controles ----------------------
  btnResetFiltersClick(nil);
end;

// Tipo de activo -------------------------------------------
procedure TtabFilter._SetTYPEItems;
var
  I : Integer;
  AssetType : TAssetType;
begin
  with cmbTYPE.Items do
   begin
    Clear; AddPair('Todos los tipos', IntToStr(NO_VALUE)); I := 0;
    for AssetType in TAssetType do
     begin
      if HeartBeat_IsValidType(AssetType) then AddPair(AssetTypeToStr(AssetType), IntTostr(Ord(AssetType)));
      Inc(I);
     end;
   end;
end;

// Tipo de moneda --------------------------------------------
procedure TtabFilter._SetCURRENCYItems;
var
  I : Integer;
  CurrencyType : TCurrencyType;
begin
  with cmbCURRENCY.Items do
   begin
    Clear;
    AddPair('Todas las monedas', IntToStr(NO_VALUE)); I := 0;

    for CurrencyType in TCurrencyType do
     if not (CurrencyType = crcyUNKNOWN) then
      begin
       AddPair(CurrencyTypeToStr(CurrencyType), IntTostr(Ord(CurrencyType)));
       Inc(I);
      end;
    //Move(1, Count - 1);
   end;
end;

// Categorías -------------------------------------------
procedure TtabFilter._SetCATEGORYItems;
var
  I : Integer;
begin
  with cmbCATEGORY do
   begin
    Clear;
    Items.AddPair('Todas las categorías', IntToStr(NO_VALUE));

    with Configuration.Categories do
     for I := 0 to Count - 1 do
      Items.AddPair(Values[Names[I]], Names[I]);

    ItemIndex := 0;
   end;
end;

////////////////////////////////////////////////////////////////////////////////
// Ramgos                                                                     //
////////////////////////////////////////////////////////////////////////////////

// Cargar rango -----------------------------
procedure Ttabfilter._AddRange(aText : String; aMax : Integer; aMin : Integer; var aArray : TRangeArray; acmbBox : TComboBox);
var
  Count : Integer;
begin
  Count := Length(aArray);
  SetLength(aArray, Count + 1);

  // Cargar array --------------------------
  aCmbBox.Items.AddPair(aText, IntToStr(NO_VALUE));
  with aArray[Count] do begin ValueA := aMin; ValueB := aMax; end;
end;

// Precios -------------------------------------------
procedure TtabFilter._setPRICERangesItems;
begin
  cmbPRICE.Items.Clear;
  SetLength(fPriceRanges, 0);
  _AddRange('Todos los valores', NO_VALUE, NO_VALUE, fPriceRanges, cmbPRICE);

  _AddRange('Menos de 1k',   1000, 0, fPriceRanges, cmbPRICE);
  _AddRange('Menos de 5k',   5000, 0, fPriceRanges, cmbPRICE);
  _AddRange('Menos de 10k', 10000, 0, fPriceRanges, cmbPRICE);
  _AddRange('Menos de 15k', 15000, 0, fPriceRanges, cmbPRICE);
  _AddRange('Menos de 20k', 20000, 0, fPriceRanges, cmbPRICE);

  _AddRange('De 5k a 10k',  10000,  5000, fPriceRanges, cmbPRICE);
  _AddRange('De 10k a 15k', 15000, 10000, fPriceRanges, cmbPRICE);
  _AddRange('De 15k a 20k', 20000, 15000, fPriceRanges, cmbPRICE);
  _AddRange('De 20k a 25k', 25000, 20000, fPriceRanges, cmbPRICE);
end;

// Incrfemento precio --------------------------------------------------------
procedure TtabFilter.cmbVOLPERCENTChange(Sender: TObject);
begin
//
end;


// =============================================================================
// == DIBUJAR CENTRADO                                                        ==
// =============================================================================

procedure TtabFilter.cmbTYPEDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
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

     // FUENTE =======================================================
     // Remarcado si es es el item seleccionado
     with Canvas.Font do
      begin
       if not (Index = 0) and (ItemIndex = Index) then
        Style := Style + [fsBold]
       else Style := Style - [fsBold];

       if odSelected in State then
        Color := clHighlightText
       else
        if Enabled then
         Color := clWindowText
        else
         Color := clGrayText;
      end;
     // =============================================================

     // Calcular las coordenadas para centrar el texto
     Txt := Items.Names[Index];
     X := ARect.Left + (ARect.Width  - TextWidth (Txt)) div 2;
     Y := ARect.Top  + (ARect.Height - TextHeight(Txt)) div 2;
     TextOut(X, Y, Txt); // Dibujar el texto centrado
  end;
end;


// =============================================================================
// == SETEAR FILTRO                                                           ==
// =============================================================================

// REINICIAR TODOS LOS FILTROS =================================================

procedure TtabFilter.btnResetFiltersClick(Sender: TObject);
begin
  DataAssets.Filter.Clear;

  { #todo : Evitar todos estos dobles disparos.
    Evitar hardcodeado. Obtener datos de DataAssets.Filter. }
  // Resetear componentes ----------------------
  edtSYMBOL.Text         := '';
  cmbCATEGORY.ItemIndex  := 0;

  cmbTYPE.ItemIndex      := 0;
  cmbHBRange.ItemIndex   := 0;
  cmbPRICE.ItemIndex     := 0;
  cmbCURRENCY.ItemIndex  := 0;

  chckOnlyFavorite.Checked  := False;
  chckOnlyPortfolio.Checked := False;
  chckBlackListed.Checked   := False;
  chckHBVolConfirm.Checked  := False;

  _RefreshBtnReset; // Resetear filtro y restaurar boton
end;

procedure TtabFilter._RefreshBtnReset;
begin
  btnResetFilters.Enabled :=
    not IsEmptyString(edtSYMBOL.Text)  or
    not (cmbCATEGORY.ItemIndex = 0) or
    not (cmbTYPE.ItemIndex = 0)     or
    not (cmbHBRange.ItemIndex = 0)  or
    not (cmbPRICE.ItemIndex = 0)    or
    not (cmbCURRENCY.ItemIndex = 0) or

    chckHBVolConfirm.Checked  or
    chckBlackListed.Checked   or
    chckOnlyPortfolio.Checked or
    chckOnlyFavorite.Checked;
end;

// SIMBOLO =====================================================================
procedure TtabFilter.edtSYMBOLButtonClick(Sender: TObject);
begin
  edtSYMBOL.Text := ''; //_RefreshBtnReset; --> Implicito
end;

procedure TtabFilter.edtSYMBOLChange(Sender: TObject);
var
  CursorPos : Integer;
  NewSymbolFilter : String;
begin
  with edtSYMBOL do
   begin
    CursorPos := SelStart; // Guarda la posición actual del cursor en el TEdit
    NewSymbolFilter := Trim(Text);

    // Actualiza el texto solo si ha cambiado
    if not (Text = NewSymbolFilter) then
     begin
      Text := NewSymbolFilter;
      if CursorPos > Length(NewSymbolFilter) then CursorPos := Length(NewSymbolFilter);
      SelStart := CursorPos;
     end;
   end;

  with edtSYMBOL do
   if not( IsEmptyString(Text)) then
     Font.Style := [fsBold]
   else Font.Style := [fsItalic];

  // Pasar el filtro -------------------------
  DataAssets.Filter.Symbol := NewSymbolFilter;
  _RefreshBtnReset;
end;

// TIPO ========================================================================
procedure TtabFilter.cmbTYPEChange(Sender: TObject);
var
  NewValue : Integer;
begin
  with cmbTYPE do NewValue := StrToIntDef(Items.ValueFromIndex[ItemIndex], -1);
  with DataAssets.Filter do
   if not( NewValue = NO_VALUE ) then
     AssetType := TAssetType(NewValue)
    else ByAssetType := False;

  _RefreshBtnReset;
end;

// CATEGORIA ===================================================================
procedure TtabFilter.cmbCATEGORYChange(Sender: TObject);
var
  NewValue : Integer;
begin
  with DataAssets.Filter, cmbCATEGORY do
   begin
    NewValue := StrToIntDef(Items.ValueFromIndex[ItemIndex], NO_VALUE);

    if not (NewValue = NO_VALUE) then
      Category := NewValue
     else ByCategory := False;
   end;

  _RefreshBtnReset;
end;

// RANGO HEART BEAT ============================================================
procedure TtabFilter.cmbHBRangeChange(Sender: TObject);
begin
  with DataAssets.Filter, cmbHBRange do
   if ItemIndex > 0 then
    HBRange := THBRange(ItemIndex - 1)
   else ByHBRange := False;

  _RefreshBtnReset;
end;

// PRECIO ======================================================================
procedure TtabFilter.cmbPRICEChange(Sender: TObject);
var
  Indx : Integer;
begin
  Indx := cmbPrice.ItemIndex;

  with DataAssets.Filter do
   if not(Indx = 0) then // --> 'TODOS'
    with fPriceRanges[Indx] do
     SetPriceRange(ValueB, ValueA)
   else ByPrice := False;

  _RefreshBtnReset;
end;

// MONEDA ======================================================================
procedure TtabFilter.cmbCURRENCYChange(Sender: TObject);
var
  NewValue : Integer;
begin
  with DataAssets.Filter, cmbCURRENCY do
   begin
    NewValue := StrToIntDef(Items.ValueFromIndex[ItemIndex], NO_VALUE); // Todas por defecto

    if not(NewValue = NO_VALUE) or not(ItemIndex = 0) then
     Currency := TCurrencyType(NewValue)
    else ByCurrency := False;
   end;

  _RefreshBtnReset;
end;

// MARCADORES ==================================================================

procedure TTabFilter._RefreshChecked (chckBox : TCheckBox);
begin
  with chckBox, Font do
   if Checked then Style := Style + [fsBold] else Style := Style - [fsBold];

  _RefreshBtnReset;
end;

procedure TtabFilter.chckOnlyFavoriteChange(Sender: TObject);
begin
  DataAssets.Filter.OnlyFavorite := chckOnlyFavorite.Checked;
  _RefreshChecked(chckOnlyFavorite);
end;

procedure TtabFilter.chckOnlyPortfolioChange(Sender: TObject);
begin
  DataAssets.Filter.OnlyPortfolio := chckOnlyPortfolio.Checked;
  _RefreshChecked(chckOnlyPortfolio);
end;

procedure TtabFilter.chckBlackListedChange(Sender: TObject);
begin
  DataAssets.Filter.ShowBlackListed := chckBlackListed.Checked;
  _RefreshChecked(chckBlackListed);
end;

procedure TtabFilter.chckHBVolConfirmChange(Sender: TObject);
begin
  DataAssets.Filter.HBVolConfirm := chckHBVolConfirm.Checked;
  _RefreshChecked(chckHBVolConfirm);
end;

end.

