unit Data_Assets_Filter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Market, HeartBeat;

Type

  { TDataAssetsFilter : Objeto para armar el filtro. }

  TDataAssetsFilter = class(TObject)
  private
    fFilter  : String;      // Filtro completo
    fFilters : TStringList; // SubFiltros

    fOnChange : TNotifyEvent; // Evento de cambio

    // Por simbolo ------------------------
    fSymbol : String;

    // Por tipo de activo -----------------
    fByType    : Boolean;
    fAssetType : TAssetType;

    // Por moneda ------------------------
    fCurrency     : TCurrencyType;
    fByCurrency   : Boolean;

    // Por precio ------------------------
    fByPrice  : Boolean;
    fMaxPrice : Real;
    fMinPrice : Real;

    // Por rango de HeartBeat ------------
    fByHBRange : Boolean;
    fHBRange : THBRange;
    fHBVolConfirm: Boolean;

    // Exclusivos ------------------------
    fByCategory : Boolean;
    fCategory   : Integer;

    // Marcadores ------------------------
    fViewBlackListed   : Boolean;
    fViewOnlyFavorite  : Boolean;
    fViewOnlyPortfolio : Boolean;
    //fViewVolumeZero    : Boolean;

    procedure _SetHBVolConfirm(aValue: Boolean);
    procedure _Refresh;

    // Metodos SET ====================================
    procedure _SetSymbol(aValue : String);

    procedure _SetByCategory (aValue : Boolean);
    procedure _SetCategory(aValue : Integer);

    procedure _SetByType(aByType : Boolean);
    procedure _SetAssetType(aAssetType : TAssetType);

    procedure _SetByCurrency(aValue: Boolean);
    procedure _SetCurrency(aValue: TCurrencyType);

    procedure _SetByPrice(aValue: Boolean);
    procedure _SetMaxPrice(aValue: Real);
    procedure _SetMinPrice(aValue: Real);

    procedure _SetByHBRange(aValue : Boolean);
    procedure _SetHBRange(aValue: THBRange);

    procedure _SetViewBlackListed(aValue: Boolean);
    procedure _SetViewOnlyFavorite(aValue: Boolean);
    procedure _SetViewOnlyPortfolio(aValue: Boolean);
    //procedure _SetViewVolumeZero(aValue: Boolean);

    // Notificación de cambio ===========================
    procedure _SetOnChange(aValue: TNotifyEvent);

  public
    constructor Create;
    destructor Destroy; override;

    property Filter : String read fFilter;
    property OnChange : TNotifyEvent read FOnChange write _SetOnChange;

    procedure Clear;

    // SIMBOLO -------------------------------------------
    property Symbol : String read fSymbol write _SetSymbol;

    // Tipo ----------------------------------------------
    property ByAssetType : Boolean    read fByType    write _SetByType;
    property AssetType   : TAssetType read fAssetType write _SetAssetType;

    // Categoría -----------------------------------------
    property ByCategory : Boolean read fByCategory write _SetByCategory;
    property Category   : Integer read fCategory   write _SetCategory;

    // Moneda ------------------------------------------
    property ByCurrency : Boolean       read fByCurrency write _SetByCurrency;
    property Currency   : TCurrencyType read fCurrency   write _SetCurrency;

    // Heart Beat ----------------------------------------
    property ByHBRange : Boolean  read fByHBrange write _SetByHBRange;
    property HBRange   : THBRange read fHBRange   write _SetHBRange;

    property HBVolConfirm : Boolean read fHBVolConfirm write _SetHBVolConfirm;

    // Precio -----------------------------------------
    property ByPrice  : Boolean read fByPrice write _SetByPrice;
    property MinPrice : Real read fMinPrice  write _SetMinPrice;
    property MaxPrice : Real read fMaxPrice  write _SetMaxPrice;
    procedure SetPriceRange(aMax : Real; aMin : Real = 0);

    // Inclusivos ------------------------------------------------
    property ShowBlackListed : Boolean read fViewBlackListed write _SetViewBlackListed;
    //property ShowVolumeZero   : Boolean read FViewVolumeZero  write _SetViewVolumeZero;

    // Exclusivos --------------------------------------------------
    property OnlyPortfolio : Boolean read FViewOnlyPortfolio write _SetViewOnlyPortfolio;
    property OnlyFavorite  : Boolean read FViewOnlyFavorite  write _SetViewOnlyFavorite;
  end;


implementation

{ TDataAssetsFilter }

uses Tools, Config, LazLogger, Data_Assets, Data_Assets_Const;

constructor TDataAssetsFilter.Create;
begin
  inherited Create;
  fFilters := TStringList.Create;

  Clear;
end;

procedure TDataAssetsFilter.Clear;
begin
  fFilter := '';

  // Por datos del activo -----------------
  fSymbol := '';

  // Categoría ---------------------------------
  fCategory   := FLD_MARK_CATEGORY_NOVALUE;
  fByCategory := False;

  // tipo -----------------------------------
  fByType := False;
  fAssetType := asstUNKNOWN; // El primer valor por defecto

  fByCurrency := False;
  fCurrency   := crcyUNKNOWN; // Por pesos

  // Precio --------------------------------
  fByPrice := False;
  fMaxPrice := -1;
  fMinPrice := -1;

  // Heart Beat ---------------------------
  fByHBRange := False; fHBRange := hbrngBullZone;
  fHBVolConfirm := False;

  // Marcadores ------------------------
  fViewBlackListed   := False;
  fViewOnlyFavorite  := False;
  fViewOnlyPortfolio := False;
  //fViewVolumeZero    := False;

  _Refresh;
end;

destructor TDataAssetsFilter.Destroy;
begin
  fFilters.Free;

  inherited Destroy;
end;

procedure TDataAssetsFilter._SetOnChange(aValue: TNotifyEvent);
begin
  if not (fOnChange = aValue) then fOnChange := aValue;
end;

////////////////////////////////////////////////////////////////////////////////
// ARMAR FILTRO                                                               //
////////////////////////////////////////////////////////////////////////////////

procedure TDataAssetsFilter._Refresh;
var
  I : Integer;
  NewFilter : String;
begin
  with fFilters do
   begin
    Clear;

    // Simbolo -----------------------------------------
    if not IsEmptyString(fSymbol) then
      fFilters.Add(FLD_ASSET_SYMBOL + ' = ' + QuotedStr('*' + Trim(fSymbol) + '*')); // Filtro por simbolo

    // Tipo --------------------------------------------
    if fByType then fFilters.Add(FLD_ASSET_TYPE + ' = ' + IntToStr(Ord(fAssetType)));

    // Categorías --------------------------------------
    if fByCategory then fFilters.Add(FLD_MARK_CATEGORY + ' = ' + IntTostr(fCategory));

    // Moneda ------------------------------------------
    if fByCurrency then
     if fCurrency = crcyUSD then
       fFilters.Add('( ' + FLD_ASSET_CURRENCY + ' = ' + IntToStr(Ord(crcyUSD))     + ') OR ' +
                    '( ' + FLD_ASSET_CURRENCY + ' = ' + IntToStr(Ord(crcyUSD_CCL)) + ') OR ' +
                    '( ' + FLD_ASSET_CURRENCY + ' = ' + IntToStr(Ord(crcyUSD_MEP)) + ')')
      else fFilters.Add(FLD_ASSET_CURRENCY + ' = ' + IntToStr(Ord(fCurrency)));

    // Precio ------------------------------------------
    if fByPrice then
     fFilters.Add('(' + FLD_PRICE_CLOSE + ' >= ' + FloatToStr(fMinPrice) + ') AND ' +
                  '(' + FLD_PRICE_CLOSE + ' <= ' + FloatToStr(fMaxPrice) + ')');

    // Otros --------------------------------------------
    // Exclusivos ....
    if fViewOnlyPortfolio then fFilters.Add(FLD_MARK_PORTFOLIO);
    if fViewOnlyFavorite  then fFilters.Add(FLD_MARK_FAVORITE);
    if fHBVolConfirm      then
      fFilters.Add('( (' + FLD_HB_VOLUME + ' > ' + FLD_HB_FORCE + ') AND (' + FLD_HB_FORCE + ' > 0 ) OR ' +
                     '(' + FLD_HB_VOLUME + ' < ' + FLD_HB_FORCE + ') AND (' + FLD_HB_FORCE + ' < 0 ) )');

    // Inclusivos ....
    if not fViewBlackListed then fFilters.Add( 'NOT ' + FLD_MARK_BLACKLIST);
    //if not fViewVolumeZero  then fFilters.Add('( ' + FLD_VOLUME + ' <> 0 )'); // No movimiento (Volumen cero) (filtro inclusivo)

    // Rango HB ----------------------------------------
    if fByHBRange then
     case fHBRange of
      hbrngBearZone           : // -Infinite To Zero
        fFilters.Add(FLD_HB_FORCE + ' <= 0');

      hbrngBearConfirm    : // -Infinite To HB_BEAR_CONFIRM
        fFilters.Add('(' + FLD_HB_FORCE + ' <= ' + IntTostr(HB_BEAR_CONFIRM) + ')' );

      hbrngBearish        : // HB_BEAR_CONFIRM To HB_BEAR
        fFilters.Add(
         '(' + FLD_HB_FORCE + ' >= ' + IntTostr(HB_BEAR_CONFIRM) + ') AND ' +
         '(' + FLD_HB_FORCE + ' <= ' + IntToStr(HB_BEAR) + ')' );

      hbrngBearishLateral : // HB_BEAR To Zero
        fFilters.Add(
         '(' + FLD_HB_FORCE + ' >= ' + IntTostr(HB_BEAR) + ') AND ' +
         '(' + FLD_HB_FORCE + ' <= 0)' );

      hbrngLateral        : // HB_BEAR To HB_BULL
        fFilters.Add(
         '(' + FLD_HB_FORCE + ' >= ' + IntTostr(HB_BEAR) + ') AND ' +
         '(' + FLD_HB_FORCE + ' <= ' + IntToStr(HB_BULL) + ')' );

      hbrngBullishLateral : // Zero To HB_BULL
        fFilters.Add(
         '(' + FLD_HB_FORCE + ' >= 0) AND ' +
         '(' + FLD_HB_FORCE + ' <= ' + IntToStr(HB_BULL) + ')' );

      hbrngBullish        : // HB_BULL To HB_BULL_CONFIRM
        fFilters.Add(
         '(' + FLD_HB_FORCE + ' >= ' + IntTostr(HB_BULL) + ') AND ' +
         '(' + FLD_HB_FORCE + ' <= ' + IntToStr(HB_BULL_CONFIRM) + ')' );

      hbrngBullConfirm    : // HB_BEAR_CONFIRM To +Infinite
        fFilters.Add(FLD_HB_FORCE + ' >= ' + IntTostr(HB_BULL_CONFIRM) );

      hbrngBullZone           : // Zero To +Infinite
         fFilters.Add(FLD_HB_FORCE + ' >= 0');
     end;
   end;

  // ====================================================
  // == Armar el filtro
  // ====================================================
  NewFilter := '';
  for I := 0 to fFilters.Count - 1 do
   begin
    NewFilter := NewFilter + ' ( ' + fFilters[i] + ' ) ';
    if i < fFilters.Count - 1 then NewFilter := NewFilter + ' AND ';
   end;

   // Inclusivos ------------------------------------


  //Disparar notificación =============================
  if not (NewFilter = fFilter) then // Si no es igual, notificar
   begin
    fFilter := NewFilter;
    if Assigned(fOnChange) then
     begin
      //DebugLn('Filter: Change ' + Newfilter );
      fOnChange(Self);
    end;
   end; // else DebugLn('Filter: NO Change ');
end;


////////////////////////////////////////////////////////////////////////////////
// VALORES                                                                    //
////////////////////////////////////////////////////////////////////////////////

// Símbolo ---------------------------------------------------------------------
procedure TDataAssetsFilter._SetSymbol(aValue : String);
var
  aSymbol : String;
begin
  aSymbol := Trim(aValue);
  if not (aSymbol = fSymbol) then
   begin
    fSymbol := aSymbol;
    _Refresh;
   end;
end;

// Tipo ------------------------------------------------------------------------
procedure TDataAssetsFilter._SetByType(aByType : Boolean);
begin
  if not (aByType = fByType) then
   begin
    fByType := aByType;
    _Refresh;
   end;
end;

procedure TDataAssetsFilter._SetAssetType(aAssetType : TAssetType);
begin
  if not (aAssetType = fAssetType) then
   begin
    fAssetType := aAssetType;
    fByType := True;
    _Refresh;
   end;
end;

// Categoría -------------------------------------------------------------------
procedure TDataAssetsFilter._SetByCategory(aValue : Boolean);
begin
  if not (aValue = fByCategory) then
   begin
    fByCategory := aValue;
    _Refresh;
   end;
end;

procedure TDataAssetsFilter._SetCategory(aValue : Integer);
begin
  aValue := Abs(aValue);

  if not (aValue = fCategory) then
   begin
    // #TODO COntrolar el valor este dentro de los valores
    fCategory   := aValue;
    fByCategory := True;
    _Refresh;
   end;
end;

// Heart Beat ------------------------------------------------------------------
procedure TDataAssetsFilter._SetByHBRange(aValue: Boolean);
begin
 if not (aValue = fByHBRange) then
   begin
    fByHBRange := aValue;
    _Refresh;
   end;
end;

procedure TDataAssetsFilter._SetHBRange(aValue: THBRange);
begin
  if not (aValue = fHBRange) then
   begin
    fHBRange := aValue;
    fByHBRange := True;
    _Refresh;
   end;
end;

procedure TDataAssetsFilter._SetHBVolConfirm(aValue: Boolean);
begin
  if not (aValue = fHBVolConfirm) then
   begin
    fHBVolConfirm := aValue;
    _Refresh;
   end;
end;

// Precio ----------------------------------------------------------------------
procedure TDataAssetsFilter._SetByPrice(aValue: Boolean);
begin
  if not(fByPrice = aValue) then
   begin
    fByPrice := aValue;
    _Refresh;
   end;
end;

procedure TDataAssetsFilter._SetMaxPrice(aValue: Real);
begin
  if not (fMaxPrice = aValue) then
   if aValue <= fMinPrice then
    begin
     fMaxPrice := aValue;
     if fByPrice then _Refresh;
    end
   else WriteLN('Filtro: Precio máximo rechazado. Es menos al mínimo.');
end;

procedure TDataAssetsFilter._SetMinPrice(aValue: Real);
begin
  if not (fMinPrice = aValue) then
   if aValue >= FMaxPrice then
    begin
     fMinPrice := aValue;
     if fByPrice then _Refresh;
    end
   else WriteLN('Filtro: Precio mínimo rechazado. Es mayor al máximo.');
end;

procedure TDataAssetsFilter.SetPriceRange(aMax: Real; aMin: Real);
var
  tmp : Real;
begin
  // Ambos números invalidos --> Sin Filtro
  fByPrice := (aMax >= 0) and (aMin >= 0);
  // Verificar que el orden está correcto.
  if (aMax < aMin) then begin tmp := aMax; aMax := aMin; aMin := tmp; end;

  // Diferentes valores a lo anterior? --> Actualizar filtro
  if not(aMax = fMaxPrice) or not(aMin = fMinPrice) then
   begin
    fMaxPrice := aMax;
    fMinPrice := aMin;
    fByPrice := True;
    _Refresh;
   end
  else fByPrice := False;
end;

// Moneda ----------------------------------------------------------------------
procedure TDataAssetsFilter._SetByCurrency(aValue: Boolean);
begin
  if not (fByCurrency = aValue) then
   begin
    fByCurrency := aValue;
    _Refresh;
   end;
end;

procedure TDataAssetsFilter._SetCurrency(aValue: TCurrencyType);
begin
  if not (fCurrency = aValue) then
   begin
    fCurrency  := aValue;
    fByCurrency := True;
    _Refresh;
   end;
end;

// Marcadores ------------------------------------------------------------------

// Inclusivos ........
procedure TDataAssetsFilter._SetViewBlackListed(aValue: Boolean);
begin
  if not (fViewBlackListed = aValue) then
   begin
    fViewBlackListed := aValue;
    _Refresh;
   end;
end;

// Exclusivos ........
procedure TDataAssetsFilter._SetViewOnlyFavorite(aValue: Boolean);
begin
  if not (fViewOnlyFavorite = aValue) then
   begin
    fViewOnlyFavorite := aValue;
    _Refresh;
   end;
end;

procedure TDataAssetsFilter._SetViewOnlyPortfolio(aValue: Boolean);
begin
  if not (fViewOnlyPortfolio = aValue) then
   begin
    fViewOnlyPortfolio := aValue;
    _Refresh;
   end;
end;

end.

