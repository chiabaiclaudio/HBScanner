unit Data_Assets;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Forms, Market,
  Data_Assets_Filter, BufDataset, DB, DateUtils, data_assets_const;


Type
  { TDataAssets }

  TDataAssets = class(TDataModule)
    tblData: TBufDataset;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure tblBeforePost(DataSet: TDataSet);

  private
    // Posición ===================================================
    fCurrentID : String;
    fBookMark : TBookMark;

    // Filtro =====================================================
    fFilter : TDataAssetsFilter;
    procedure _SetFilter(Sender: TObject);

    // GCarga de datos  ==============================================
    procedure _InitializeDataSet;
    procedure _LoadDataFiles;

    // CAMPOS =====================================================
    procedure _AdjustFields;

    procedure _OnGetTextCURRENCY (Sender: TField; var aText: string; DisplayText: Boolean);
    procedure _OnGetTextTYPE (Sender: TField; var aText: string; DisplayText: Boolean);
    procedure _OnGetTextSETTLEMENT (Sender: TField; var aText: string; DisplayText: Boolean);

    function  _GetTextFieldByList(aList: TStringList; Sender: TField; DisplayText: Boolean) : String;
    procedure _OnGetTextCATEGORY(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure _OnGetTextZONE(Sender: TField; var aText: string; DisplayText: Boolean);

    procedure _OnGetTextDESCRIPTION_LONG(Sender: TField; var aText: string; DisplayText: Boolean);

    procedure _OnGetTextPRICE_FIELD (Sender: TField; var aText: string; DisplayText: Boolean);
    procedure _OnGetTextVOLUME_FIELD (Sender: TField; var aText: string; DisplayText: Boolean);
    procedure _OnGetTextDATE_FIELD (Sender: TField; var aText: string; DisplayText: Boolean);
    procedure _OnGetTextBOOLEAN_FIELD (Sender: TField; var aText: string; DisplayText: Boolean);

  public
    // Posición ----------------------------------------
    function GetActualAssetID: TAssetID;
    //function GetActualAssetID(aUpdateTable : Boolean = False) : TAssetID;

    procedure MarkPosition (aUseBookMark : Boolean = False);
    function  RestorePosition: Boolean;

    // ABM ========================================================

    // Alta --------------------------------------------
    function Add(aAsset : TAsset) : Boolean;
    function Add(aAsset : TAsset; aTable : TBufDataset) : Boolean;

    // Modificación ------------------------------------
    procedure SetFieldBoolean(FieldName : String; aValue : Boolean = False);
    procedure SetFieldInteger(FieldName : String; aValue : Integer = 0);
    procedure SetFieldFloat  (FieldName : String; aValue : Real = 0.0);

    procedure SetBlackList(aValue: Boolean = True);
    procedure ToggleBlackList;
    procedure SetFavorite(aValue: Boolean = True);
    procedure ToggleFavorite;

    procedure SetCategory(aValue: Integer = FLD_MARK_CATEGORY_NOVALUE);
    procedure SetZone(aValue: Integer = FLD_MARK_ZONE_NOVALUE);

    // Baja --------------------------------------------
    function Delete : Boolean;

    // Filtro y Orden =============================================
    property Filter : TDataAssetsFilter read fFilter;
    function SetOrder(aFieldName : String): Boolean;

    // Proceso de Actualización ===================================
    procedure CopyTo(aDataset: TBufDataset; aFiltered : Boolean = False; aCopyData: Boolean = True);

    // Exportar / importar listado ================================
    procedure ExportCSV(aFileName : TFileName);
    procedure ImportCSV(aFileName : TFileName);

  end;

var
  DataAssets: TDataAssets;

implementation

uses LazFileUtils, Config, Math, FileUtil, Tools;

{$R *.lfm}

const
  EMPTY_VALUE_TEXT = '---';

{ TDataAssets }

procedure TDataAssets.DataModuleCreate(Sender: TObject);
begin
  fCurrentID := '';

  // Crear dataset y configurarlo ============================
  tblData.FileName := Configuration.AssetsDataFile;
  _LoadDataFiles;
  _AdjustFields;
  tblData.First;

   // Filtros ================================================
  fFilter := TDataAssetsFilter.Create;
  fFilter.OnChange := @_SetFilter;

  with tblData do
    begin
     Filter   := fFilter.Filter;
     Filtered := True;
    end;
end;

procedure TDataAssets.DataModuleDestroy(Sender: TObject);
var
  BackupName : TFileName;
begin
  // Hacer copia antes de grabar --------
  if Configuration.Backup then
   begin
    BackupName := ExtractFileName(tblData.FileName) + '-' + FormatDateTime('YYYYMMDD', Date);
    CopyFile(tblData.FileName, Configuration.BackupDir + BackupName, [cffOverwriteFile]);
   end;

  // Grabar datos -----------------------
  with tblData do
   begin
    Filtered := False; // Al parecer, al grabar filtrado los datos desaparecen
    SaveToFile;
   end;

  fFilter.Free;
end;

// =============================================================================
// == INICIALIZACION DE LA TABLA                                              ==
// =============================================================================

procedure TDataAssets._LoadDataFiles;
begin
  with tblData do
   begin
    if FileExists(FileName) and FileIsReadable(FileName) then
     begin
      try
        LoadFromFile();
        if IsEmpty then _InitializeDataSet;
      except
        WriteLN('INIT: El archivo de activos no se pudo cargar correctamente. Creando vacío.');
        _InitializeDataSet;
      end;
     end
    else
     begin
      WriteLN('INIT : El archivo de activos no existe o no es legible. Creando vacío.');
      _InitializeDataSet;
     end;
   end;
end;

procedure TDataAssets._InitializeDataSet;
begin
  tblData.Close;

  with tblData.FieldDefs do
   begin
     Clear;

     // Actualizacion ========================
     Add(FLD_UPDATE_TIME,  ftDateTime);
     Add(FLD_UPDATE_CODE, ftInteger);
     Add(FLD_UPDATE_MSG,   ftString);

     // ID ===================================
     Add(FLD_ASSET_ID,         ftString, 250, True);  // ID Interno
     Add(FLD_ASSET_MARKET,     ftString, 150, True);
     Add(FLD_ASSET_TYPE,       ftInteger);
     Add(FLD_ASSET_SETTLEMENT, ftInteger);
     Add(FLD_ASSET_SYMBOL,     ftString, 150, True);
     Add(FLD_ASSET_CURRENCY,   ftInteger);

     // Descripción --------------------------
     Add(FLD_ASSET_DESCRIPTION, ftString, 250);
     Add(FLD_ASSET_NOTE,        ftWideMemo);

     // Marcadores ===========================
     Add(FLD_MARK_PORTFOLIO, ftBoolean);
     Add(FLD_MARK_BLACKLIST, ftBoolean);
     Add(FLD_MARK_FAVORITE,  ftBoolean);
     Add(FLD_MARK_CATEGORY,  ftInteger);
     Add(FLD_MARK_ZONE,      ftInteger);
     //Add(FLD_MARKS,     ftBoolean); // Para mostrar los marcadores juntos

     // Datos ================================
     // Precios ------------------------------
     Add(FLD_PRICE_CLOSE_LAST, ftFloat); // Precio anterior
     Add(FLD_PRICE_CLOSE,      ftFloat); // Precio actual
     Add(FLD_PRICE_OPEN,       ftFloat);
     Add(FLD_PRICE_LOW,        ftFloat);
     Add(FLD_PRICE_HIGH,       ftFloat);

     // Volumen -----------------------------
     Add(FLD_VOLUME,        ftLargeint);
     Add(FLD_VOLUME_LAST,   ftLargeint);
     Add(FLD_VOLUME_CHANGE, ftFloat);

     Add(FLD_VOLUME_ORDER,        ftLargeint);
     Add(FLD_VOLUME_ORDER_LAST,   ftLargeint);
     Add(FLD_VOLUME_ORDER_CHANGE, ftFloat);

     // Cambio de precios -------------------
     Add(FLD_PRICE_CHANGE_CC, ftFloat); // Cambio interdía: Close -> Close
     Add(FLD_PRICE_CHANGE_OC, ftFloat); // Cambio intradía: Open  -> Close

     Add(FLD_PRICE_CHANGE_THREE_DAY,  ftFloat);
     Add(FLD_PRICE_CHANGE_WEEK,       ftFloat);
     Add(FLD_PRICE_CHANGE_HALF_MONTH, ftFloat);
     Add(FLD_PRICE_CHANGE_MONTH,      ftFloat);
     //Add(FLD_CHANGE_MONTH_3,    ftFloat);
     //Add(FLD_CHANGE_MONTH_6,    ftFloat);

     // Heart Beat --------------------------
     Add(FLD_HB_BASE,   ftInteger); // HB con Sensibilidad 1
     Add(FLD_HB_FORCE,  ftInteger); // HB_BASE con ajuste de escala con FLD_HB_SENSIBILITY;
     Add(FLD_HB_VOLUME, ftInteger); // HB_FORCE ajustado por volumen

     Add(FLD_HB_SPREAD_VOL,  ftFloat); // HB <-> HB Vol.

     Add(FLD_HB_SENSIBILITY, ftInteger);

     // Derivados ---------------------------
     Add(FLD_DERIV_SMA,  ftFloat); // Media simple
     Add(FLD_DERIV_EMA,  ftFloat); // Media exponencial
     Add(FLD_DERIV_WMA,  ftFloat); // Media ponderada
     Add(FLD_DERIV_SWMA, ftFloat); // .........

     Add(FLD_DERIV_SMA_WMA, ftFloat); // SMA <-> WMA

     // Señales ================================
     Add(FLD_SCORE,             ftInteger);
     Add(FLD_SCORE_BULL,        ftInteger);
     Add(FLD_SCORE_BEAR,        ftInteger);
     Add(FLD_SCORE_DESCRIPTION, ftWideMemo);
     // ftWideMemo); // Quizas cambiar a ftWideString (Menos memoria)
    end;

  tblData.CreateDataset;
end;

// =============================================================================
// == MANEJO DE CAMPOS                                                        ==
// =============================================================================

// Establecer valores por defecto ----------------------------------------------
procedure TDataAssets.tblBeforePost(DataSet: TDataSet);
var
  Fld : TField;
begin
  for Fld in DataSet.Fields do
   with Fld do
    if IsNull then // No hay valor definido
     begin
      if DataType in [ftString]  then AsString := '';

      // Integros a cero ---------------
      if DataType in [ftUnknown, ftString, ftSmallint, ftInteger,
            ftWord, ftFloat, ftCurrency,  ftLargeint]  then AsInteger := 0;

      // Booleanos en falso ------------
      if DataType = ftBoolean then AsBoolean := False;

      // Tiempos -----------------------
      if DataType in [ftDate, ftTime, ftDatetime] then AsDateTime := Now;

      // Valores por defecto diferentes ===========================
      case FieldName of
        FLD_UPDATE_CODE    : AsInteger := FLD_UPDATE_CODE_NOVALUE;
        FLD_MARK_CATEGORY  : AsInteger := FLD_MARK_CATEGORY_NOVALUE; // Sin categoría
        FLD_MARK_ZONE      : AsInteger := FLD_MARK_ZONE_NOVALUE; // Sin zona { #todo :  ACCIONES como Agentina. }
        FLD_HB_SENSIBILITY : AsInteger := Configuration.HBSensibilityDefault;
      end;
     end;
end;

// Textos a mostrar -------------------------------------------------------
procedure TDataAssets._AdjustFields;
begin
 with tblData do
  begin
   // Fechas ------------------------
   FieldByName(FLD_UPDATE_TIME).OnGetText := @_OnGetTextDATE_FIELD;

   // Descripción -------------------
   FieldByName(FLD_ASSET_CURRENCY).OnGetText   := @_OnGetTextCURRENCY;
   FieldByName(FLD_ASSET_TYPE).OnGetText       := @_OnGetTextTYPE;
   FieldByName(FLD_ASSET_SETTLEMENT).OnGetText := @_OnGetTextSETTLEMENT;

   FieldByName(FLD_MARK_CATEGORY).OnGetText   := @_OnGetTextCATEGORY;
   FieldByName(FLD_MARK_ZONE).OnGetText       := @_OnGetTextZONE;

   // Números -----------------------
   FieldByName(FLD_PRICE_CLOSE_LAST).OnGetText := @_OnGetTextPRICE_FIELD;
   FieldByName(FLD_PRICE_CLOSE).OnGetText      := @_OnGetTextPRICE_FIELD;
   FieldByName(FLD_PRICE_OPEN).OnGetText       := @_OnGetTextPRICE_FIELD;
   FieldByName(FLD_PRICE_LOW).OnGetText        := @_OnGetTextPRICE_FIELD;
   FieldByName(FLD_PRICE_HIGH).OnGetText       := @_OnGetTextPRICE_FIELD;

   FieldByName(FLD_VOLUME).OnGetText            := @_OnGetTextVOLUME_FIELD;
   FieldByName(FLD_VOLUME_LAST).OnGetText       := @_OnGetTextVOLUME_FIELD;
   FieldByName(FLD_VOLUME_ORDER).OnGetText      := @_OnGetTextVOLUME_FIELD;
   FieldByName(FLD_VOLUME_ORDER_LAST).OnGetText := @_OnGetTextVOLUME_FIELD;

   // Booleanos ---------------------
   FieldByName(FLD_MARK_BLACKLIST).OnGetText := @_OnGetTextBOOLEAN_FIELD;
   FieldByName(FLD_MARK_PORTFOLIO).OnGetText := @_OnGetTextBOOLEAN_FIELD;
   FieldByName(FLD_MARK_FAVORITE).OnGetText  := @_OnGetTextBOOLEAN_FIELD;
  end;
end;

procedure TDataAssets._OnGetTextTYPE (Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if DisplayText then aText := AssetTypeToStr(TAssetType(Sender.AsInteger), False);
end;

procedure TDataAssets._OnGetTextCURRENCY (Sender: TField; var aText: string;
   DisplayText: Boolean);
begin
  if DisplayText then aText := CurrencyTypeToStr(TCurrencyType(Sender.AsInteger), False, False);
end;

procedure TDataAssets._OnGetTextDESCRIPTION_LONG(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  with tblData do
   if DisplayText and not(RecordCount = 0) then
    aText := FieldByName(FLD_ASSET_SYMBOL).AsString + ' - ' +
             FieldByName(FLD_ASSET_DESCRIPTION).AsString + ' [ ' +
             AssetTypeToStr(TAssetType(FieldByName(FLD_ASSET_TYPE).AsInteger)) + ' / ' +
             CurrencyTypeToStr(TCurrencyType(FieldByName(FLD_ASSET_CURRENCY).AsInteger)) + ' ]'
   else aText := '';
end;

// Utilitarios para CATEGORY y ZONE ============================================
function TDataAssets._GetTextFieldByList(aList: TStringList; Sender: TField; DisplayText: Boolean) : String;
var
  Indx : Integer;
  aName, aValue : String;
begin
  Result := '';

  with tblData do
   if DisplayText and (TAssetType(tblData.FieldByName(FLD_ASSET_TYPE).ASInteger) in AsseTypeCategorisable) then
    begin
     with Configuration do
      begin
       Indx := aList.IndexOfName(Sender.AsString);
       if not (Indx = -1) then
        begin
         Alist.GetNameValue(Indx, aName, aValue);
         Result := aValue;
        end;
      end;
   end else Result := EMPTY_VALUE_TEXT;
end;

procedure TDataAssets._OnGetTextCATEGORY(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  aText := _GetTextFieldByList(Configuration.Categories, Sender, DisplayText);
end;

procedure TDataAssets._OnGetTextZONE(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  aText := _GetTextFieldByList(Configuration.Zones, Sender, DisplayText);
end;

procedure TDataAssets._OnGetTextSETTLEMENT (Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if DisplayText then aText := SettlementTypeToStr(TSettlementType(Sender.AsInteger));
end;

procedure TDataAssets._OnGetTextVOLUME_FIELD (Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if DisplayText then aText := FormatValue(Sender.AsLargeInt);
end;

procedure TDataAssets._OnGetTextDATE_FIELD (Sender: TField; var aText: string; DisplayText: Boolean);
var
  DateTimeValue: TDateTime;
  ElapsedDays: Integer;
begin
  if DisplayText and not(Sender.AsDateTime = 0) then
   begin
     // Aplicar TimeZone local ( Posible BUG)
     DateTimeValue := Sender.AsDateTime - (GetLocalTimeOffset / 1440);
     ElapsedDays := DaysBetween(Now, DateTimeValue);

     case ElapsedDays of
      0        : aText := FormatDateTime('hh:nn:ss', DateTimeValue); // Menos de 1 día
      1 .. 6   : aText := FormatDateTime('ddd hh:nn:ss', DateTimeValue); // Menos de 7 días
      7 .. 364 : aText := FormatDateTime('dd/mmmm hh:nn:ss', DateTimeValue); // Menos de 1 año
     else
      aText := FormatDateTime('dd/mm/yyyy hh:nn:ss', DateTimeValue); // Más de 1 año
    end;
   end
  else aText := '';
end;

procedure TDataAssets._OnGetTextPRICE_FIELD (Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if DisplayText then
   if not (Sender.AsFloat = 0) then
    aText := FormatFloat('#######.##', Sender.AsFloat)
   else
    aText := '-';
end;

procedure TDataAssets._OnGetTextBOOLEAN_FIELD (Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if DisplayText then aText := '';
end;

// =============================================================================
// == A.B.M.                                                                  ==
// =============================================================================

// POSICION ====================================================================

function TDataAssets.GetActualAssetID: TAssetID;
begin
  with tblData do
   Result := AssetIDInit(FieldByName(FLD_ASSET_SYMBOL).AsString,
              FieldByName(FLD_ASSET_TYPE).AsInteger, FieldByName(FLD_ASSET_MARKET).AsString);
end;

{function TDataAssets.GetActualAssetID(aUpdateTable : Boolean = False) : TAssetID;
begin
  Result := GetAssetID(_GetDataset(aUpdateTable));
end; }

procedure TDataAssets.MarkPosition (aUseBookMark : Boolean);
begin
  tblData.DisableControls;
  if not aUseBookMark then
   begin
    with tblData do fCurrentID := IfThenStr(IsEmpty, '', Trim(FieldByName(FLD_ASSET_ID).AsString));
    fBookMark := nil;
   end
  else fBookMark := tblData.Bookmark;
end;

function TDataAssets.RestorePosition : Boolean;
begin
  Result := False;
  if fBookMark = nil  then
   begin
    if not(IsEmptyString(fCurrentID)) then
     try
      Result := tblData.Locate(FLD_ASSET_ID, fCurrentID, []);
     finally
      fCurrentID := '';
     end;
   end else tblData.Bookmark := fBookMark;;

  tblData.EnableControls;
end;

////////////////////////////////////////////////////////////////////////////////
// ALTA
//

function TDataAssets.Add(aAsset : TAsset; aTable : TBufDataset) : Boolean;
var
  ID : String;
begin
  with aTable do
   begin
    ID := aAsset.ID;

    ControlsDisabled;
    aTable.Filtered := False;
    Result := Locate(FLD_ASSET_ID, ID, []);

    if not Result then
     begin
      Append;

      FieldByName(FLD_ASSET_ID).AsString          := ID;    // Construir la cadena ID

      FieldByName(FLD_ASSET_MARKET).AsString      := aAsset.ID.Market;
      FieldByName(FLD_ASSET_TYPE).AsInteger       := Ord(aAsset.ID.TypeAsset);
      FieldByName(FLD_ASSET_SYMBOL).AsString      := aAsset.ID.Symbol;

      FieldByName(FLD_ASSET_DESCRIPTION).AsString := aAsset.Description;

      FieldByName(FLD_ASSET_SETTLEMENT).AsInteger := Ord(aAsset.Settlement);
      FieldByName(FLD_ASSET_CURRENCY).AsInteger   := Ord(aAsset.Currency);

      Post; Result := True;
     end;

    EnableControls;
    tblData.Filtered := True;
    tblData.SaveToFile;
   end;
end;

function TDataAssets.Add(aAsset : TAsset) : Boolean;
begin
  Result := Add(aAsset, tblData);
end;

////////////////////////////////////////////////////////////////////////////////
// MODIFICACION
//

// Metodos básicos ------------------------------------------
procedure TDataAssets.setFieldBoolean(FieldName : String; aValue : Boolean = False);
begin
  with tblData, FieldByName(FieldName) do
   if DataType = ftBoolean then
    if not (AsBoolean = aValue) then
     begin
      Edit;
      AsBoolean := aValue;
      Post;
     end;
end;

procedure TDataAssets.SetFieldInteger(FieldName : String; aValue : Integer = 0);
begin
  with tblData, FieldByName(FieldName) do
   if DataType in [ftSmallint, ftInteger, ftLargeint] then
    if not (AsInteger = aValue) then
     begin
      Edit;
      AsInteger := aValue;
      Post;
     end;
end;

procedure TDataAssets.SetFieldFloat(FieldName : String; aValue : Real = 0.0);
begin
  with tblData, FieldByName(FieldName) do
   if DataType in [ftFloat, ftCurrency] then
    if not (AsFloat = aValue) then
     begin
      Edit;
      AsFloat := aValue;
      Post;
     end;
end;

// MARCADORES -------------------------------------------------------------
procedure TDataAssets.SetBlackList(aValue: Boolean);
begin
  if tblData.FieldByName(FLD_MARK_FAVORITE).AsBoolean then aValue := False;
  setFieldBoolean(FLD_MARK_BLACKLIST, aValue);
end;

procedure TDataAssets.ToggleBlackList;
begin
  SetBlackList(not tblData.FieldByName(FLD_MARK_BLACKLIST).AsBoolean);
end;

procedure TDataAssets.SetFavorite(aValue: Boolean);
begin
  if aValue then SetBlackList(False); // BLACKLIST excluyente con FAVORITE
  setFieldBoolean(FLD_MARK_FAVORITE, aValue);
end;

procedure TDataAssets.ToggleFavorite;
begin
  SetFavorite(not tblData.FieldByName(FLD_MARK_FAVORITE).AsBoolean);
end;

procedure TDataAssets.SetCategory(aValue: Integer);
begin
  if (tblData.FieldByName(FLD_MARK_CATEGORY).AsInteger = aValue) then
   setFieldInteger(FLD_MARK_CATEGORY, FLD_MARK_CATEGORY_NOVALUE) // Eliminar categoría
  else
   setFieldInteger(FLD_MARK_CATEGORY, aValue); //Nueva categoría
end;

procedure TDataAssets.SetZone(aValue: Integer);
begin
  if (tblData.FieldByName(FLD_MARK_ZONE).AsInteger = aValue) then
   setFieldInteger(FLD_MARK_ZONE, FLD_MARK_ZONE_NOVALUE) // Eliminar categoría
  else
   setFieldInteger(FLD_MARK_ZONE, aValue); //Nueva categoría
end;

////////////////////////////////////////////////////////////////////////////////
// BAJA
//

function TDataAssets.Delete : Boolean;
begin
  tblData.Delete;
end;


// *****************************************************************************
// * FILTRO Y ORDEN                                                            *
// *****************************************************************************

// ORDEN =======================================================================
function TDataAssets.SetOrder(aFieldName : String): Boolean;
const
  ORDER_DESC = ' DESC';
begin
  with tblData do
   begin
    GetBookMark;

    try
     if not IsEmptyString(aFieldName) then
      if not ( Pos(aFieldName, IndexFieldNames) = 0 ) then
       begin  // Mismo campo? Cambiar dirección --------------
        if not ( Pos(ORDER_DESC, IndexFieldNames) = 0 ) then
          IndexFieldNames := aFieldName // ASC es por defecto
        else
         IndexFieldNames := aFieldName  + ORDER_DESC;
      end
     else IndexFieldNames := aFieldName; // Nuevo campo y dirección

    finally
     RestorePosition;
     EnableControls;
    end;

    Result := IndexFieldNames = aFieldName;
  end;
end;

// FILTRO ======================================================================
procedure TDataAssets._SetFilter(Sender: TObject);
begin
  MarkPosition;
  tblData.Filter := fFilter.Filter;
  RestorePosition;
end;

// *****************************************************************************
// * Datos                                                                     *
// *****************************************************************************

// == COPIA DE DATOS ===========================================================

procedure TDataAssets.CopyTo(aDataset: TBufDataset; aFiltered : Boolean = False; aCopyData: Boolean = True);
begin
  with tblData do
   begin
    DisableControls;
    Filtered := not (aFiltered);
    aDataSet.CopyFromDataset(tblData, aCopyData);
    Filtered := True;
    EnableControls;
   end;
end;


////////////////////////////////////////////////////////////////////////////////
// Exportar / Importar                                                        //
////////////////////////////////////////////////////////////////////////////////

const
  EXPORT_FIELDS : array of String =
    (FLD_ASSET_SYMBOL, FLD_ASSET_TYPE, FLD_ASSET_MARKET, FLD_ASSET_CURRENCY, FLD_ASSET_SETTLEMENT, FLD_ASSET_DESCRIPTION,
     FLD_MARK_CATEGORY, FLD_MARK_BLACKLIST, FLD_MARK_PORTFOLIO, FLD_MARK_FAVORITE, FLD_HB_SENSIBILITY, FLD_ASSET_NOTE);

procedure TDataAssets.ExportCSV(aFileName : TFileName);
var
  tblWork : TBufDataset;
  CSVFile: TextFile;
  I : Integer;
  Value, Line: string;

  function FormatText(const aText: string): string;
  begin
    // Si el campo contiene comas, comillas dobles o saltos de línea, envuélvelo en comillas dobles.
    if (Pos(',', aText) > 0) or (Pos('"', aText) > 0) or (Pos(#10, aText) > 0) or (Pos(#13, aText) > 0) then
     Result := '"' + StringReplace(aText, '"', '""', [rfReplaceAll]) + '"'
    else Result := aText;
  end;

begin
  // Crear tabla de trabajo --------------------
  tblWork := TBufDataset.Create(Self);
  CopyTo(tblWork); // Copiar datos

  try
   // Abrir archivo de salida ------------------
   AssignFile(CSVFile, aFileName);
   Rewrite(CSVFile); // Crea o vacía el archivo

   with tblWork do
    begin
     First;

     while not EOF do
      begin
       Line := '';
       for i := 0 to High(EXPORT_FIELDS) do
        begin
         Value := FieldByName(EXPORT_FIELDS[i]).AsString;
         Line := Line + FormatText(Value); { #todo : Usar constante standard como separador }

         if not (i = High(EXPORT_FIELDS)) then Line := Line + ',';
        end;

       WriteLN(CSVFile, Line); // Grabar linea
       Next; // Siguente registro
      end;

    end;

  finally
   CloseFile(CSVFile); // Inmediatamente cierra el archivo
  end;
end;

procedure TDataAssets.ImportCSV(aFileName : TFileName);
var
  CSVFile: TextFile;
  ValuesLine : TStringList;
  Line : string;
  I: Integer;
  Asset : TAsset;
begin
  AssignFile(CSVFile, aFileName);
  ValuesLine := TStringList.Create; // Crear instancia de TStringList

  try
   Reset(CSVFile); // Abrir archivo para lectura
   ValuesLine.Delimiter := ',';
   ValuesLine.StrictDelimiter := True; // Evita considerar espacios como separadores

    // Leer el archivo línea por línea
   while not EOF(CSVFile) do
    begin
     ReadLn(CSVFile, Line);
     ValuesLine.DelimitedText := Line;

     // Agregar nombres a los campos exportados
     for I := 0 to High(EXPORT_FIELDS) do
       if I < ValuesLine.Count then
        ValuesLine[I] := EXPORT_FIELDS[I] + '=' + ValuesLine[I];

      // Grabar la línea procesada ===============================
      with ValuesLine do
       Asset := AssetInit(
         Values[FLD_ASSET_SYMBOL],
         TAssetType(StrToIntDef(Values[FLD_ASSET_TYPE], 0)),
         Values[FLD_ASSET_MARKET],
         Values[FLD_ASSET_DESCRIPTION],
         TCurrencyType(StrToIntDef(Values[FLD_ASSET_CURRENCY], 0)));

      with DataAssets do
       if Add(Asset) then
        begin
         SetCategory(StrToIntDef(ValuesLine.Values[FLD_MARK_CATEGORY], FLD_MARK_CATEGORY_NOVALUE));
         SetBlackList(StrToBoolDef(ValuesLine.Values[FLD_MARK_BLACKLIST], False));
         SetFavorite(StrToBoolDef(ValuesLine.Values[FLD_MARK_FAVORITE], False));
        end;
    end;
  finally
   CloseFile(CSVFile); // Cerrar archivo
   ValuesLine.Free; // Liberar memoria
  end;
end;

end.

