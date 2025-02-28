unit Thread_Update;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Market,
  syncobjs, Thread_SearchBase, BufDataset;

Type
  
  { TThreadUpdate }

  TThreadUpdate = class(TThread)
   private
     fCS : TCriticalSection;
     fTblWork : TBufDataset;

     fOnComplete  : TNotifyEvent;
     fOnProgress  : TNotifyStringEvent;

     fProgressMessage     : String;
     fLastProgressMessage : String;

     fIsGetData : Boolean;

     fPortfolioList : TAssetArray;

     fAssetID : TAssetID;
     fCurrentData, fPReviousData : TAssetData;

     fHistoric   : TAssetDataArray;
     fHBHistoric : array of Real;

     procedure _SendMessageProgress(const Msg : String = '');
     procedure _SendMessageProgressSync;

     // 1er. paso : Obtener cartera ================================
     procedure _Portfolio_Sync;
     function _InPortfolio (aAssetID : TAssetID): Boolean;

     // 2do. paso : Pocesrar datos ==============================================
     function _GetData : Boolean;

     procedure _ProcessData_Sync;
     procedure _ProcessData;
     procedure _ProcessPrices;
     procedure _ProcessVolume;

     // Heart Beat ---------------------------
     procedure _ProcessHB;

     // Derivados ..................
     function _ProcessHB_SMA(aLength : Integer) : Real;
     function _ProcessHB_EMA(aLength : Integer) : Real;
     function _ProcessHB_WMA(aLength : Integer) : Real;
     function _ProcessHB_SWMA(aLength : Integer = 4) : Real;

     procedure _GenerateScore;

     // 3er. paso : Actualizar ======================================
     procedure _Update_Sync;

   public
     constructor Create;
     destructor Destroy; override;
     procedure Execute; override;

     property OnProgress: TNotifyStringEvent read fOnProgress write fOnProgress;
     property OnComplete: TNotifyEvent read fOnComplete write fOnComplete;
   end;

implementation

uses Data_Assets, Tools, Data_Provider, Config, HeartBeat, Data_Assets_Const, Data_Historic;


const
  MA_LENGTH = 10; { #todo : Hacer flxible este valor. }


constructor TThreadUpdate.Create;
begin
  inherited Create(True);
  fCS := TCriticalSection.Create;
  fProgressMessage := '';
  fLastProgressMessage := '';

  // Copiar datos de trabajo
  fTblWork := TBufDataset.Create(nil);
  DataAssets.CopyTo(fTblWork);
end;

destructor TThreadUpdate.Destroy;
begin
  fCS.Free; fTblWork.Free;
  inherited Destroy;
end;

procedure TThreadUpdate._SendMessageProgress(const Msg : String);
begin
  if Assigned(fOnProgress) then
   begin
    fLastProgressMessage := fProgressMessage;
    fProgressMessage := Msg;

    TThread.Synchronize(nil, @_SendMessageProgressSync);
   end;
end;

procedure TThreadUpdate._SendMessageProgressSync;
begin
  fOnProgress(fProgressMessage);
end;

// =============================================================================
// == Proceso principal                                                       ==
// =============================================================================

{$MACRO ON}
{$DEFINE TERMINATED_CHECK := if Terminated then Exit; }

procedure TThreadUpdate.Execute;
begin
  // Refrescar la lista de la cartera ============================
  fCS.Enter;

  try
   if not(Terminated) then begin fCS.Enter; TThread.Synchronize(nil, @_Portfolio_Sync); fCS.Leave; end;
   if not(Terminated) then begin fCS.Enter; TThread.Synchronize(nil, @_ProcessData_Sync);  fCS.Leave; end;
   if not(Terminated) then begin fCS.Enter; TThread.Synchronize(nil, @_Update_Sync);  fCS.Leave; end;

   if Terminated then _SendMessageProgress('Actualización cancelada.');

   // { #todo : Revisar por la detención ------------------------- }
  finally;
   if not Terminated and Assigned(fOnComplete) then TThread.Synchronize(nil, TThreadMethod(fOnComplete));
   fCS.Leave;
  end;
end;

// *****************************************************************************
// ** 1ER. PASO : CARTERA                                                     **
// *****************************************************************************

procedure TThreadUpdate._Portfolio_Sync;
var
  Asset : TAsset;
begin
  WriteLN('UPD: Portfolio ... ');
  _SendMessageProgress('Agregando cartera ...');

  // Obtener el listado ------------------
  DataProvider.Portfolio(fPortfolioList);
  // -------------------------------------
  TERMINATED_CHECK;

  // Agregar a los datos a actualizar -------------------------------
  for Asset in fPortfolioList do // Recorrer el listado
   if Asset.ID.TypeAsset in HB_AssetTypeValid then // Filtrar por tipo HB Valido
     DataAssets.Add(Asset, fTblWork); // Agregar a la tabla de trabajo
end;

// Está en la cartera? -----------------------------
function TThreadUpdate._InPortfolio (aAssetID : TAssetID) : Boolean;
var
  Asset : TAsset;
begin
  Result := False;
  for Asset in fPortfolioList do
   begin
    Result := ( aAssetID = Asset.ID );
    if Result then Break;
   end;
end;

// *****************************************************************************
// ** 2DO. PASO : GENERAR DATOS                                               **
// *****************************************************************************

procedure TThreadUpdate._ProcessData_Sync;
var
  Total, Count, Progress : Integer;
begin
  // Inicializar ----------------------------------------------------
  with fTblWork do
   begin
    Count := -1; Progress := 0; // 0% a 99%
    //Close; Open; // Refrescar ---------------
    Last; Total := RecordCount; First; // Recorrer la tabla para tener un numero correcto

    // Recorrer la tabla =====================================
    while not EOF do
     begin
      // - Incrementar contadores -----------------------------
      Inc(Count); Progress := Round((Count * 100) / Total);
      _SendMessageProgress('Obteniendo datos ['+ FieldByName(FLD_ASSET_SYMBOL).AsString +'] ... ' + IntToStr(Progress) + '%');

      // - En Lista Negra? No procesar (borrar
      if not FieldByName(FLD_MARK_BLACKLIST).AsBoolean then
       begin
        // Obtener los datos =================================
        fAssetID := AssetIDInit( // - Setear ID --------------
          FieldByName(FLD_ASSET_SYMBOL).AsString,
          FieldByName(FLD_ASSET_TYPE).AsInteger,
          FieldByName(FLD_ASSET_MARKET).AsString );

        fIsGetData := _GetData;   // - Buscar datos
        _ProcessData; // - Procesar datos

        // Seguir -------------------
        Next;
       end
      else Delete;
     end; // FIN Recorrer la tabla ------------------------
   end;
end;

function TThreadUpdate._GetData: Boolean;
//const
//  ColWidth = 12; // Ancho de cada columna para alinear los datos
begin
  Result := False;

  _SendMessageProgress; // Actualizar interface
  if DataProvider.Current(fCurrentData, fAssetID) then
   begin
    { #todo : Crear algun mecanismo fuera de rueda. Lso valores suelen ser invalidos . Solo Close }
    Result := DataHistoric.Historic(fHistoric, fAssetID, 45); { #todo : Algo más variable al largo del pedido. }

    // Valores invalidos? Usar historico
    with fCurrentData.Prices do
    if Result and (Length(fHistoric) > 0) and
    ( (Close = 0) or (Low = 0) or (High = 0) or (Open = 0) or (fCurrentData.Volume = 0) ) then
     fCurrentData := fHistoric[0];

   { WriteLN(' Comparación ', fAssetID.SYMBol, ': ', ifThenStr(FCurrentData.Prices = Fhistoric[0].Prices, 'iguales', 'diferentes'));

    with FCurrentData.Prices do
     WriteLN(Format('Current --> Fecha: %s  C:%-*s O:%-*s H:%-*s L:%-*s',
      [ DateToStr(FCurrentData.DateTime), ColWidth, FloatToStr(Close), ColWidth,
       FloatToStr(Open), ColWidth, FloatToStr(High), ColWidth, FloatToStr(Low)]));

    with Fhistoric[0].Prices do
     WriteLN(Format('Search  --> Fecha: %s  C:%-*s O:%-*s H:%-*s L:%-*s',
      [ DateToStr(Fhistoric[0].DateTime), ColWidth, FloatToStr(Close), ColWidth,
       FloatToStr(Open), ColWidth, FloatToStr(High), ColWidth, FloatToStr(Low)]));

    WriteLN('  '); }

   end;

  _SendMessageProgress; // Actualizar interface
  TERMINATED_CHECK;
end;

// *****************************************************************************
// ** Procesar datos                                                          **
// *****************************************************************************

procedure TThreadUpdate._ProcessData;
begin
  with fTblWork do
   begin
    Edit; // Preparar para editar -----------------------

    // Marcadores ---------------------------------------
    FieldByName(FLD_MARK_PORTFOLIO).AsBoolean := _InPortfolio(fAssetID); // ¿Está en la cartera?
    if FieldByName(FLD_MARK_PORTFOLIO).AsBoolean then FieldByName(FLD_MARK_BLACKLIST).AsBoolean := False; // Sacar de lista negra si está en cartera.

    // Mensajes del origen de datos -----------------------
    with DataProvider.ActiveOrigin do
     begin
      FieldByName(FLD_UPDATE_CODE).AsInteger  := ServerCode;
      FieldByName(FLD_UPDATE_MSG).AsString    := ServerMessage + ' // ' + InternalError;
      FieldByName(FLD_UPDATE_TIME).AsDateTime := IfThen(fIsGetData, fCurrentData.DateTime, Now);
     end;

    // EDITAR DATOS ===============================================
    if DataHistoric.FindPreviousData(fPreviousData, fAssetID, 1) then
     begin
      _ProcessPrices;
      _ProcessVolume;
      _ProcessHB;
      _GenerateScore;
      _SendMessageProgress; // Actualizar interface
     end
    else WriteLN(String(fAssetID), 'No previous data.');

    Post;
   end;
end;

// BASICOS =====================================================================
procedure TThreadUpdate._ProcessPrices;
begin
  with fTblWork do
   with fCurrentData.Prices do
    begin
     FieldByName(FLD_PRICE_OPEN) .AsFloat := Open;
     FieldByName(FLD_PRICE_CLOSE).AsFloat := Close;
     FieldByName(FLD_PRICE_LOW)  .AsFloat := Low;
     FieldByName(FLD_PRICE_HIGH) .AsFloat := High;

     // Precio anterior -------------
     FieldByName(FLD_PRICE_CLOSE_LAST).AsFloat  := fPreviousData.Prices.Close; // Precio anterior

     // Cambios porcentuales ---------
     FieldByName(FLD_PRICE_CHANGE_CC).AsFloat := specialize PercentChange<Real>(fPreviousData.Prices.Close, Close); // Cambio interdía: Close -> Close
     FieldByName(FLD_PRICE_CHANGE_OC).AsFloat := specialize PercentChange<Real>(Open, Close); // Cambio intradía: Open  -> Close
    end;
end;

procedure TThreadUpdate._ProcessVolume;
begin
  with fTblWork do
   begin
    // Volumen ----------------------------------------------------
    with fCurrentData  do FieldByName(FLD_VOLUME_ORDER).AsLargeInt      := Trunc( Volume / Prices.Close);
    with fPreviousData do FieldByName(FLD_VOLUME_ORDER_LAST).AsLargeInt := Trunc( Volume / Prices.Close);
    FieldByName(FLD_VOLUME_ORDER_CHANGE).AsFloat :=
      specialize PercentChange<Int64>(FieldByName(FLD_VOLUME_ORDER_LAST).AsLargeInt,
                                      FieldByName(FLD_VOLUME_ORDER).AsLargeInt);

    FieldByName(FLD_VOLUME).AsLargeInt      := Trunc(fCurrentData.Volume); // Volumen actual
    FieldByName(FLD_VOLUME_LAST).AsLargeInt := Trunc(fPreviousData.Volume); // Volumen anterior
    FieldByName(FLD_VOLUME_CHANGE).AsFloat  := specialize PercentChange<Int64>(fPreviousData.Volume, fCurrentData.Volume);

    WriteLN('B ', FieldByName(FLD_VOLUME_ORDER).AsLargeInt);
    WriteLN('A ', FieldByName(FLD_VOLUME_ORDER).AsLargeInt);

    {WriteLN(FieldByName(FLD_ASSET_ID).ASString);
    with fCurrentData do
    begin
      WriteLN(FieldByName(FLD_VOLUME_ORDER).AsLargeInt, ' ',  Volume / Prices.Close,  ' T ', Trunc( Volume / Prices.Close));
      WriteLN('C: ', Prices.Close, 'V: ', Volume);
    end;}

{    with FieldByName(FLD_VOLUME_CHANGE_2) do
     if DataHistoric.FindPreviousData(Data2, fAssetID, 2) then
      AsFloat := specialize PercentChange<Int64>(fPreviousData.Volume, Data2.Volume)
     else AsFloat := 0;

    with FieldByName(FLD_VOLUME_CHANGE_3) do
     if DataHistoric.FindPreviousData(Data3, fAssetID, 2) then
      AsFloat := specialize PercentChange<Int64>(fPreviousData.Volume, Data3.Volume)
     else AsFloat := 0; }
   end;
end;

 // HEART BEAT =================================================================
procedure TThreadUpdate._ProcessHB;
var
  i: Integer;
  Percent : Real;
  HBForce : Real;
  MALength : Integer;
begin
  with fTblWork do
   begin
    // Heart Beat -------------------------------------------------
    HBForce := HeartBeat_Force(fCurrentData.Prices, fPreviousData.Prices, 1);
    FieldByName(FLD_HB_BASE).AsInteger  := Trunc(HBForce); // Sensibilidad = 1
    FieldByName(FLD_HB_FORCE).AsInteger := FieldByName(FLD_HB_BASE).AsInteger * FieldByName(FLD_HB_SENSIBILITY).AsInteger;

    // Ajustar por volumen ----------------------------------------
    Percent := specialize IfThen<Real>(fPreviousData.Volume > 0, (fCurrentData.Volume / fPreviousData.Volume), 1);
    FieldByName(FLD_HB_VOLUME).AsInteger := Trunc(HBForce * Percent);

    // Spread HB -- HB por volumen
    FieldByName(FLD_HB_SPREAD_VOL).AsInteger := FieldByName(FLD_HB_VOLUME).AsInteger - FieldByName(FLD_HB_FORCE).AsInteger;
    //WriteLN(' HB -- Vol ', FieldByName(FLD_HB_SPREAD_VOL).AsInteger);

    // Agregar HB historico al historico de precios --------------------
    { #note : Posibemente esto ejecutar una sola vez cuando se trae el hisrtorico.
      Esto no va a actualizarse si HBSensibility cambia. No hay cambio al vuelo. }
    SetLength(fHBHistoric, Length(fHistoric));
    //WriteLN(FieldBYName(FLD_ASSET_ID).ASString, ' ====================');
    for i := Low(fHistoric) to High(fHistoric) - 1 do
     fHBHistoric[i] := HeartBeat_Force(fHistoric[i].Prices, fHistoric[i + 1].Prices, 1)
                         * FieldByName(FLD_HB_SENSIBILITY).AsInteger;
       {with FHistoric[i].Prices do
         WriteLn(Format('Index %3d: %10s ---> %5d  O %8s  H %8s  L %8s  C %8s',
           i, DateToStr(FHistoric[i].DateTime), Trunc(fHBHistoric[i]),
             FloatToStr(Open), FloatToStr(High),
             FloatToStr(Low), FloatToStr(Close)]));}

    // Derivados ---------------------------------
    MALength := IfThen(MA_LENGTH > Length(fHBHistoric), Length(fHBHistoric), MA_LENGTH); // Propeger con un historico muy corto
    FieldByName(FLD_DERIV_SMA).AsInteger  := 0;
    FieldByName(FLD_DERIV_EMA).AsInteger  := 0;
    FieldByName(FLD_DERIV_WMA).AsInteger  := 0;
    FieldByName(FLD_DERIV_SWMA).AsInteger := 0;

    if not (MALength = 0 ) then
     begin
      FieldByName(FLD_DERIV_SMA).AsInteger  := Trunc(_ProcessHB_SMA(MALength));
      FieldByName(FLD_DERIV_EMA).AsInteger  := Trunc(_ProcessHB_EMA(MALength));
      FieldByName(FLD_DERIV_WMA).AsInteger  := Trunc(_ProcessHB_WMA(MALength));
      FieldByName(FLD_DERIV_SWMA).AsInteger := Trunc(_ProcessHB_SWMA);
     end;
   end;
end;

function TThreadUpdate._ProcessHB_SMA(aLength : Integer) : Real;
var
  i: Integer;
  Total: Real;
begin
  Total := 0.0; Result := Total;

  for i := 0 to aLength - 1 do
   Total := Total + fHBHistoric[i];

  Result := Total / aLength;
end;

function TThreadUpdate._ProcessHB_EMA(aLength : Integer) : Real;
var
  Alpha, PrevEMA: Real;
  i: Integer;
begin
  Alpha := 2 / (aLength + 1);
  PrevEMA := fHBHistoric[High(fHBHistoric) - aLength + 1];
  for i := High(fHBHistoric) - aLength + 2 to High(fHBHistoric) do
    PrevEMA := (fHBHistoric[i] - PrevEMA) * Alpha + PrevEMA;
  Result := PrevEMA;
end;

function TThreadUpdate._ProcessHB_WMA(aLength : Integer) : Real;
var
  SumWeighted, SumWeights: Real;
  i, Weight: Integer;
begin
  SumWeighted := 0; SumWeights := 0;

  for i := High(fHBHistoric) downto High(fHBHistoric) - aLength + 1 do
   begin
    Weight := (High(fHBHistoric) - i + 1);
    SumWeighted := SumWeighted + fHBHistoric[i] * Weight;
    SumWeights  := SumWeights  + Weight;
   end;

  Result := SumWeighted / SumWeights;
end;

function TThreadUpdate._ProcessHB_SWMA(aLength: Integer): Real;
var
  i: Integer;
  Weights, SumWeights, SumProduct: Double;
begin
  SumWeights := 0; SumProduct := 0;
  for i := 0 to aLength - 1 do
   begin
    Weights := (aLength - i) / aLength;
    SumWeights := SumWeights + Weights;
    SumProduct := SumProduct + fHBHistoric[High(fHBHistoric) - i] * Weights;
  end;
  Result := SumProduct / SumWeights;
end;

///////////////////////////////////////////////////////////////////////////////
// GENERAR PUNTAJE
//

procedure TThreadUpdate._GenerateScore;
var
  BullScore, BearScore : Integer;
  BullDescription, BearDescription : String;

  HB, HBVol : Integer;
  VOL_INC_1, VOL_INC_2, VOL_INC_3 : Integer;
  SMA, EMA, WMA, SWMA : Integer;
  SMA_WMA : Integer;

  function _Get(FieldName :String) : Integer;
  begin
    Result := fTblWork.FieldByName(FieldName).AsInteger;
  end;

  procedure _UpdateScore(var Score: Integer; var Description : String;
     aCondition : Boolean; aMsg : String; aValue: Integer);
  begin
    if aCondition then
     begin
      Inc(Score, aValue);
      if (not IsEmptyString(aMsg)) then
       Description := Description + LineEnding + '[' + IntTostr(aValue) + ']  ' + aMsg;
     end;
  end;

  procedure _Bull(aCondition : Boolean; aMsg : String = ''; aValue: Integer = 1);
  begin
    _UpdateScore(BullScore, BullDescription, aCondition, aMsg, aValue);
  end;

  procedure _Bear(aCondition : Boolean; aMsg : String = ''; aValue: Integer = 1);
  begin
    _UpdateScore(BearScore, BearDescription, aCondition, aMsg, aValue);
  end;

begin
  BullScore := 0; BullDescription := '';
  BearScore := 0; BearDescription := '';

  // HB directo ==================================
  HB := _Get(FLD_HB_FORCE);
  HBVOL := _Get(FLD_HB_VOLUME);

  // Heart Beat ------------------------------
  _Bull(HB > HB_BULL, 'HB alcista');
  _Bear(HB < HB_BEAR, 'HB bajista');

  _Bull(HB > HB_BULL_CONFIRM, 'HB confirmación alcista');
  _Bear(HB < HB_BEAR_CONFIRM, 'HB confirmación bajista');

  // Por volumen ------------------------------
  _Bull(HBVOL > HB_BULL, 'HB vol. alcista', 2);
  _Bear(HBVOL > HB_BEAR, 'HB vol. bajista', 2);

  _Bull(HBVOL > HB_BULL_CONFIRM, 'HB vol. confirmación alcista', 2);
  _Bear(HBVOL > HB_BULL_CONFIRM, 'HB vol. confirmación bajista', 2);

  _Bull(HBVOL > HB, 'HB vol. confirma HB');

  // Volumen =====================================
  { VOL_INC_1 := _Get(FLD_VOLUME_CHANGE);
  VOL_INC_2 := _Get(FLD_VOLUME_CHANGE_2);
  VOL_INC_3 := _Get(FLD_VOLUME_CHANGE_3);

  _Bull( (VOL_INC_1 > 0) and (VOL_INC_2 > 0) and (VOL_INC_3 > 0), 'Aumento volumen (3 días)', 3);
  _Bear( (VOL_INC_1 < 0) and (VOL_INC_2 < 0) and (VOL_INC_3 < 0), 'Descenso volumen (3 días)', 3);

  _Bull( (VOL_INC_1 >  50), 'Aumento volumen +50%');
  _Bull( (VOL_INC_1 > 100), 'Aumento volumen +100%');
  _Bull( (VOL_INC_1 > 150), 'Aumento volumen +150%');

  _Bear( (VOL_INC_1 <  50), 'Descenso volumen +50%');
  _Bear( (VOL_INC_1 < 100), 'Descenso volumen +100%');
  _Bear( (VOL_INC_1 < 150), 'Descenso volumen +150%'); }

  // Derivados ===================================
  // SMA -----------------------
  SMA := _Get(FLD_DERIV_SMA);
  _Bull(SMA > 10, 'SMA positivo (>+10)');
  _Bear(SMA < -10, 'SMA negativo (<-10)');
  _Bull(SMA > HB_BULL, 'SMA alcista', 3); // Más dificl de alcanzar
  _Bear(SMA < HB_BEAR, 'SMA bajista');
  _Bull(SMA > HB_BULL_CONFIRM, 'SMA confirmación alcista', 2);
  _Bear(SMA < HB_BEAR_CONFIRM, 'SMA confirmación bajista');

  // EMA -----------------------
  EMA := _Get(FLD_DERIV_EMA);
  _Bull(EMA > 10,  'EMA positivo (>+10)');
  _Bear(EMA < -10, 'EMA negativo (<-10)');
  _Bull(EMA > HB_BULL, 'EMA alcista', 2);
  _Bear(EMA < HB_BEAR, 'EMA bajista');
  _Bull(EMA > HB_BULL_CONFIRM, 'EMA confirmación alcista', 2);
  _Bear(EMA < HB_BEAR_CONFIRM, 'EMA confirmación bajista');

  // WMA -----------------------
  WMA := _Get(FLD_DERIV_WMA);
  _Bull(WMA > 10, '+ WMA');
  _Bear(WMA < -10, '- WMA');
  _Bull(WMA > HB_BULL, 'WMA Bull', 2);
  _Bear(WMA < HB_BEAR, 'WMA Bear');
  _Bull(WMA > HB_BULL_CONFIRM, 'WMA Bull confirm', 2);
  _Bear(WMA < HB_BEAR_CONFIRM, 'WMA Bear confirm');

  // SWMA -----------------------
  SWMA := _Get(FLD_DERIV_SWMA);
  //_Bear(SWMA >  10, 'SWMA positivo (+10)');
  //_Bear(SWMA < -10, 'SWMA negativo (-10)');
  _Bull(SWMA > HB_BULL, 'SWMA alcista', 2);
  _Bear(SWMA < HB_BEAR, 'SWMA bajista', 2);

  _Bull(SWMA > HB_BULL_CONFIRM, 'SWMA confirmación alcista', 2);
  _Bull(SWMA > HB_BEAR_CONFIRM, 'SWMA confirmación bajista', 2);

  // En orden -------------------------------
  // Primeras ......
  _Bull( (WMA > EMA) and (EMA > SMA), 'Medias móviles en orden alcista', 2 );
  _Bull( (WMA > EMA) and (EMA > SMA) and (EMA > HB_BULL), 'Medias móviles en orden alcista', 2 );
  _Bull( (WMA > EMA) and (EMA > SMA) and (EMA > HB_BULL_CONFIRM), 'Medias móviles en orden alcista con confirmación', 2);

  // Tdas ......
  _Bull( (SWMA > WMA) and (WMA > EMA) and (EMA > SMA) and (EMA > HB_BULL), 'Medias móviles en orden alcista', 2 );
  _Bull( (SWMA > WMA) and (WMA > EMA) and (EMA > SMA) and (EMA > HB_BULL_CONFIRM), 'Medias móviles en orden alcista con confirmación', 2);

  // Spread SMA - WMA ------
  SMA_WMA := _Get(FLD_DERIV_SMA_WMA);
  _Bull( SMA_WMA >  10, 'Spread SMA-WMA positivo de +10');
  _Bear( SMA_WMA < -10, 'Spread SMA-WMA negativo de -10');
  _Bull( SMA_WMA >  25, 'Spread SMA-WMA positivo de +25', 2);
  _Bear( SMA_WMA < -25, 'Spread SMA-WMA negativo de -25', 2);

  // ===============================================
  with ftblWork do
   begin
    FieldByName(FLD_SCORE).AsInteger      := BullScore - BearSCore;
    FieldByName(FLD_SCORE_BULL).AsInteger := BullScore;
    FieldByName(FLD_SCORE_BEAR).AsInteger := BearScore;
    FieldByName(FLD_SCORE_DESCRIPTION).AsString :=
    ' - Puntación Toro --------------------------------' + LineEnding +
        BullDescription + LineEnding + LineEnding +
    ' - Puntación Oso ---------------------------------' + LineEnding +
        BearDescription;
   end;
end;


// *****************************************************************************
// ** 3ER. PASO : ACTUALiZAR DATOS                                            **
// *****************************************************************************

procedure TThreadUpdate._Update_Sync;

  procedure CopyFields (const aFieldNames : array of String);
  var
    aFieldName : String;
  begin
    for aFieldName in aFieldNames do
     DataAssets.tblData.FieldByName(aFieldName).AsVariant := ftblWork.FieldByName(aFieldName).AsVariant;
  end;

begin
  with DataAssets do
   begin
    TERMINATED_CHECK;

    _SendMessageProgress('Actualizando activos ...');

    // Quitar filtro ===================================
    tblData.Filtered := False;
    MarkPosition;
    fTblWork.First;

    // Copiar ==========================================
    while not fTblWork.EOF do
     begin
      if not tblData.Locate(FLD_ASSET_ID, fTblWork.FieldByName(FLD_ASSET_ID).AsString, []) then
       begin
        tblData.Append;
        // Copiar principales -----------------------------------
        CopyFields([FLD_ASSET_ID, FLD_ASSET_MARKET, FLD_ASSET_TYPE, FLD_ASSET_SYMBOL, // ID
                    FLD_ASSET_SETTLEMENT, // Posible en ID
                    FLD_ASSET_DESCRIPTION, FLD_ASSET_CURRENCY ]); // Descripción
       end
      else tblData.Edit;

      // Copiar datos ---------------------------------------
      CopyFields([FLD_MARK_PORTFOLIO]); // Marcadores
      CopyFields([FLD_PRICE_CLOSE_LAST, FLD_PRICE_CLOSE, FLD_PRICE_OPEN,
                  FLD_PRICE_HIGH, FLD_PRICE_LOW]);
      CopyFields([FLD_VOLUME, FLD_VOLUME_LAST, FLD_VOLUME_CHANGE]); // Volumen
      CopyFields([FLD_VOLUME_ORDER, FLD_VOLUME_ORDER_LAST, FLD_VOLUME_ORDER_CHANGE] ); // Ordenes
      CopyFields([FLD_PRICE_CHANGE_CC, FLD_PRICE_CHANGE_OC, FLD_VOLUME_CHANGE]); // Cambios porcentuales
      CopyFields([FLD_HB_BASE, FLD_HB_FORCE, FLD_HB_VOLUME, FLD_HB_SPREAD_VOL]);
      CopyFields([FLD_DERIV_SMA, FLD_DERIV_EMA, FLD_DERIV_WMA, FLD_DERIV_SWMA, FLD_DERIV_SMA_WMA]); // Derivados
      CopyFields([FLD_SCORE, FLD_SCORE_BEAR, FLD_SCORE_BULL, FLD_SCORE_DESCRIPTION]); // Puntaje
      CopyFields([FLD_UPDATE_TIME, FLD_UPDATE_CODE, FLD_UPDATE_MSG]); // Actualización

      // Guardar los cambios y borrar de la lista ====================
      tblData.Post;
      TERMINATED_CHECK;
      fTblWork.Next;
     end;

    // Restaurar filtro ===================================
    tblData.Filtered := True;
    RestorePosition;
   end;
end;

end.

