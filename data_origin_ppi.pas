unit Data_Origin_PPI;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, DateUtils, httpprotocol, Data_Origin,
  Market;

Type
  
  { TDataOrigin_PPI }

  TDataOrigin_PPI = class(TDataOrigin)
  public

  protected
    function _ID : String;
    function _Description : String; override;
    function _ShortDescription : String; override;
    function _MarketDefault : String; override;

    procedure _PrepareCustom; override;

  private
    // Access Token's ===================================
    fCreationDate   : TDateTime;
    fExpirationDate : TDateTime;
    fExpires        : LongInt;
    fAccessToken    : String;
    fRefreshToken   : String;
    fTokenType      : String;

    // Tablas de conversion =============================
    fAssetTypeTable      : array[TAssetType]      of String;
    fSettlementTypeTable : array[TSettlementType] of String;
    fCurrencyTypeTable   : array[TCurrencyType]   of String;

    procedure _InitConvertionTable(aTable : array of String);
    procedure _LoadConvertionTables;

    // Funciones ------------------------
    function _AssetTypeToStr(const aValue: TAssetType): String;
    function _StrToAssetType(const aValue: String): TAssetType;

    function _SettlementTypeToStr(const aValue: TSettlementType): String;
    function _StrToSettlementType(const aValue: String): TSettlementType;

    function _StrToCurrentType(const aValue: String): TCurrencyType;
    function _CurrentTypeToStr(const aValue: TCurrencyType): String;

    // LOGIN =======================================================
    function _IsLogin  : Boolean;
    function _LoginAPI(aRefresh : Boolean = False): Boolean;

    // Busqueda general --------------------------------------------
    function _Search( var aList : TAssetArray; const aSearchTerm : String; const aTypeFilter : String) : Boolean;

    // -------------------------------------------------------------
    procedure _LoadDataFromJSON (var aData : TAssetData; JSONObject : TJSONObject);

  public
    constructor Create;

    // DATA ============================================================
    function CurrentData(aAssetID : TAssetID; var aData : TAssetData): Boolean; override;

    function HistoricData (const aAssetID : TAssetID; var aList : TAssetDataArray;
          aDays : Integer = 32) : Boolean; override;

    // BUSCAR ==========================================================
    // Con filtro por mercado -----------------------------------------------
    function Search(var aList : TAssetArray; const aSearchTerm : String) : Boolean; override;

    // Con filtro por Tipo y Mercado ----------------------------------------
    function Search(var aList : TAssetArray; const aSearchTerm : String; const aTypeFilter : TAssetType) : Boolean; override;

    // Portfolio ------------------------------------------------------------
    function PortFolio (var aList : TAssetArray) : Boolean; override;

  end;

implementation

uses Tools, Math;

const
  // URL's -----------------------------------------
  URL_BASE_PRODUCTION = 'https://clientapi.portfoliopersonal.com/';
  URL_BASE_SANDBOX    = 'https://clientapi_sandbox.portfoliopersonal.com/';
  URL_BASE            = URL_BASE_PRODUCTION + 'api/1.0/';

  URL_ACCOUNT    = URL_BASE + 'Account/';
  URL_MARKETDATA = URL_BASE + 'MarketData/';
  URL_CONFIG     = URL_BASE + 'Configuration/';

  // Datos cliente ----------------------------------
  API_AUTHORIZED_CLIENT     = 'API_CLI_REST';
  API_CLIENT_KEY_SANDBOX    = 'ppApiCliSB';
  API_CLIENT_KEY_PRODUCTION = 'pp19CliApp12';

  // Credenciales ----------------------------------
  {$INCLUDE 'claves.inc'}

  // Produccion o prueba? ---------------------------------
  API_CLIENT_KEY        = API_CLIENT_KEY_PRODUCTION;
  API_KEY               = API_KEY_PRODUCTION;
  API_SECRET            = API_SECRET_PRODUCTION;

////////////////////////////////////////////////////////////////////////////////
// Chequear que se haya logueado ===============================================
////////////////////////////////////////////////////////////////////////////////

{$MACRO ON}

{$DEFINE LOGIN_CHECK := Result := False; if not(_IsLogin) then Exit;}


// *****************************************************************************
// * META-DATA                                                                 *
// *****************************************************************************

function TDataOrigin_PPI._ID : String;
begin
  Result := 'PPI';
end;

function TDataOrigin_PPI._Description : String;
begin
  Result := 'Portfolio Personal Inversiones';
end;

function TDataOrigin_PPI._ShortDescription : String;
begin
  Result := 'P.P.I.';
end;

function TDataOrigin_PPI._MarketDefault: String;
begin
  Result := 'BYMA';
end;

////////////////////////////////////////////////////////////////////////////////
// INICIALIZAR
////////////////////////////////////////////////////////////////////////////////

constructor TDataOrigin_PPI.Create;
begin
  inherited Create;

  // Inicializar valores de acceso
  fCreationDate   := 0;
  fExpirationDate := 0;
  fExpires        := 0;
  fAccessToken    := '';
  fRefreshToken   := '';

  _loadConvertionTables;
end;

procedure TDataOrigin_PPI._InitConvertionTable(aTable : array of String);
var
  i : Integer;
begin
  for i := Low(aTable) to High(aTable) do
   aTable[i] := '???';
end;

procedure TDataOrigin_PPI._LoadConvertionTables;
begin
  // Cargar tablas de traducción -----------------------
  _initConvertionTable(fAssetTypeTable);
  fAssetTypeTable[asstUNKNOWN]             := 'DESCONOCIDO';
  fAssetTypeTable[asstBOND]                := 'BONOS';
  fAssetTypeTable[asstTREASURY_BILL]       := 'LETRAS';
  fAssetTypeTable[asstNOBAC]               := 'NOBAC';
  fAssetTypeTable[asstLEBAC]               := 'LEBAC';
  fAssetTypeTable[asstON]                  := 'ON';
  fAssetTypeTable[asstMUTUAL_FUND]         := 'FCI';
  fAssetTypeTable[asstSECURITY_BOND]       := 'CAUCIONES';
  fAssetTypeTable[asstSTOCK]               := 'ACCIONES';
  fAssetTypeTable[asstETF]                 := 'ETF';
  fAssetTypeTable[asstCDAR]                := 'CEDEARS';
  fAssetTypeTable[asstOPTION]              := 'OPCIONES';
  fAssetTypeTable[asstFUTURE]              := 'FUTUROS';
  fAssetTypeTable[asstLENDING]             := 'LICITACIONES';
  fAssetTypeTable[asstSTOCK_US]            := 'ACCIONES-USA';
  fAssetTypeTable[asstFOREIGN_MUTUAL_FUND] := 'FCI-EXTERIOR';

  _initConvertionTable(fSettlementTypeTable);
  fSettlementTypeTable[sttmUNKNOWN] := '';
  fSettlementTypeTable[sttmNOW]     := 'INMEDIATA';
  fSettlementTypeTable[sttmHOURS24] := 'A-24HS';
  fSettlementTypeTable[sttmHOURS48] := 'A-48HS';
  fSettlementTypeTable[sttmHOURS72] := 'A-72HS';

  _initConvertionTable(fCurrencyTypeTable);
  fCurrencyTypeTable[crcyUNKNOWN] := '';
  fCurrencyTypeTable[crcyARS]     := 'PESOS';
  fCurrencyTypeTable[crcyUSD]     := 'DOLARES';
  fCurrencyTypeTable[crcyUSD_CCL] := 'DOLARES DIVISA | CCL';
  fCurrencyTypeTable[crcyUSD_MEP] := 'DOLARES BILLETE | MEP';
end;

// *************************************************************************
// * AUXILIARES                                                            *
// *************************************************************************

function TDataOrigin_PPI._StrToAssetType(const aValue : String) : TAssetType;
var
  tp : TAssetType;
begin
  Result := asstUNKNOWN;
  for tp := Low(fAssetTypeTable) to High(fAssetTypeTable) do
   if UpperCase(Trim(aValue)) = fAssetTypeTable[tp] then Result := tp;
end;

function TDataOrigin_PPI._AssetTypeToStr(const aValue : TAssetType) : String;
begin
  Result := fAssetTypeTable[aValue];
end;

function TDataOrigin_PPI._StrToSettlementType(const aValue : String) : TSettlementType;
var
  tp : TSettlementType;
begin
  Result := sttmUNKNOWN;
  for tp := Low(fSettlementTypeTable) to High(fSettlementTypeTable) do
   if UpperCase(Trim(aValue)) = fSettlementTypeTable[tp] then Result := tp;
end;

function TDataOrigin_PPI._SettlementTypeToStr(const aValue : TSettlementType) : String;
begin
  Result := fSettlementTypeTable[aValue];
end;

function TDataOrigin_PPI._StrToCurrentType(const aValue: String) : TCurrencyType;
var
  tp : TCurrencyType;
begin
  Result := crcyUNKNOWN;
  for tp := Low(fCurrencyTypeTable) to High(fCurrencyTypeTable) do
   if UpperCase(Trim(aValue)) = fCurrencyTypeTable[tp] then Result := tp;
end;

function TDataOrigin_PPI._CurrentTypeToStr(const aValue: TCurrencyType) : String;
begin
  Result := fCurrencyTypeTable[aValue];
end;

// *****************************************************************************
// * METODOS                                                                   *
// *****************************************************************************

procedure TDataOrigin_PPI._PrepareCustom;
begin
  _AddHeader('accept : text/plain');
  _AddHeader('authorizedclient: ' + API_AUTHORIZED_CLIENT);
  _AddHeader('clientkey: ' + API_CLIENT_KEY);
end;

function TDataOrigin_PPI._IsLogin : Boolean;
begin
  Result := True;

  if IsEmptyString(fAccessToken) then  // Si no hay AccessToken, pedirlo
   Result := _LoginAPI
  else
    if (fExpirationDate < Now) then  // Expiró el AccesToken? es un RefreshToken
     Result := _LoginAPI(True);

  // Si hay logueo o refresco exitoso agregar el token Bearer
  if Result then _AddHeader('Authorization: Bearer ' + fAccessToken);
end;

function TDataOrigin_PPI._LoginAPI(aRefresh : Boolean): Boolean;
var
  Path         : String;
  Response     : String;
  ResponseJSON : TJSONObject;
begin
  Result := False;
  Response := '';

  // Refrescar o primer logueo? -----------------------------
  if (aRefresh) then
   begin
    Path := 'RefreshToken';
    _AddHeader('refreshToken: ' + fRefreshToken);
   end
  else
   begin
    Path := 'LoginApi';
    _AddHeader('ApiKey: ' + API_KEY);
    _AddHeader('ApiSecret: ' + API_SECRET);
   end;

  // Ejecutar el logueo ==========================================
  Result := _Post(URL_ACCOUNT + Path, Response);
  // =============================================================

  // Load data access =========================================
  if Result then
   try
    ResponseJSON := TJSONObject(GetJSON(Response));
    with ResponseJSON do
     begin
      fCreationDate   := FixISO8601ToDate(Get('creationDate', ''));
      fExpirationDate := FixISO8601ToDate(Get('expirationDate', ''));
      fAccessToken    := Get('accessToken', '');
      fRefreshToken   := Get('refreshToken', '');
      FExpires        := Get('expires', 0);
      fTokenType      := Get('tokenType', '');
     end;
   finally  // Liberar los recursos
    if Assigned(ResponseJSON) then ResponseJSON.Free;
   end;
end;

// *****************************************************************************
// * ACTIVOS                                                                   *
// *****************************************************************************

// NOTA : Sería posible rearmar esto para solo buscar un tipo de activo por vez
//        y luego combinar todo en el array resultante. Agregando { Type string (query) }

function TDataOrigin_PPI.Search( var aList : TAssetArray; const aSearchTerm : String) : Boolean;
begin
  Result := _Search(aList, aSearchTerm,  '');
end;

function TDataOrigin_PPI.Search(var aList : TAssetArray; const aSearchTerm : String;
   const aTypeFilter : TAssetType) : Boolean;
begin
  Result := _Search(aList, aSearchTerm, _AssetTypeToStr(aTypeFilter));
end;

function TDataOrigin_PPI._Search(var aList : TAssetArray; const aSearchTerm : String;
  const aTypeFilter : String) : Boolean;
var
  i : Integer;
  AssetType : TAssetType;
  Response : String;

  JSONData: TJSONArray;
  JSONObject: TJSONObject;
begin
  LOGIN_CHECK;
  SetLength(aList, 0);

  // Parametros =====================================================
  _AddParam('Ticker', HTTPEncode(aSearchTerm));
  _AddParam('Name',   HTTPEncode(aSearchTerm));
  _AddParam('Market', Market);
  if not IsEmptyString(aTypeFilter) then _AddParam('Type', aTypeFilter);
  // _AddParam('Settlement=' + fSettlementTypeTable[sttmHOURS24]); // { #todo : SoLo 24Hs }

  // ******************************************************************
  Result := _Get(URL_MARKETDATA + 'SearchInstrument', Response);
  // ******************************************************************

  if Result then
   begin
    JSONData := GetJSON(Response) as TJSONArray;

    for i := 0 to JSONData.Count - 1 do
     begin
      JSONObject := JSONData.Items[i] as TJSONObject; // Obtener el JSON
      AssetType := _StrToAssetType(JSONObject.Get('type', ''));

      // Cargar datos al array ------------------
      setLength(aList, i + 1);

      with aList[i], JSONObject do
       begin
        ID.Market       := Get('market', Market);
        ID.TypeAsset    := AssetType;
        ID.Symbol       := Get('ticker', '');

        Description  := Get('description', '');
        Currency     := _StrToCurrentType(Get('currency', ''));
        Settlement   := sttmHOURS24;
       end;
     end;

    JSONData.Free; // Liberar memoria
   end;
end;

// Lista desde la cuenta --------------------------------------------------
function TDataOrigin_PPI.Portfolio(var aList : TAssetArray) : Boolean;
var
  i, j: Integer;
  Response : String;
  ActualType : TAssetType;

  JSONData: TJSONObject;
  groupedInstruments: TJSONArray;
  group: TJSONObject;
  JSONAsset: TJSONArray;
  instrument: TJSONObject;
begin
  LOGIN_CHECK;
  SetLength(aList, 0);

  // Ejecutar Request *****************************************************
  _AddParam('accountNumber', IntToStr(ACCOUNT_NUMBER));
  Result := _Get(URL_ACCOUNT + 'BalancesAndPositions', Response);
  // **********************************************************************

  // Cargar lista de activos ===========================================
  if (Result) then
   begin
    JSONData := GetJSON(Response) as TJSONObject;

    try
      // Por tipo =======================================
      groupedInstruments := jsonData.Arrays['groupedInstruments'];

      for i := 0 to groupedInstruments.Count - 1 do
       begin
        group := groupedInstruments.Objects[i];

        // Tipo ----------------------------------------
        ActualType := _StrToAssetType(group.Get('name'));

        // Obtener listado de activos --------------------
        JSONAsset := group.Arrays['instruments'];
        for j := 0 to JSONAsset.Count - 1 do
         begin
          instrument := JSONAsset.Objects[j];

          // Agregar al listado de activos --------------
          SetLength(aList, Length(aList) + 1);
          with instrument do
            aList[High(aList)] := AssetInit(
                Get('ticker'), ActualType,
                Market, // { TODO : Reevalur el traer o no el mercado }
                Get('description'), _StrToCurrentType(Get('currency')),
                sttmHOURS24); // NO trae acuerdo. 24hs por defecto.
         end;
       end;
    finally
      if Assigned(JSONData) then JSONData.Free;
      //WriteLN('Portfolio: ', Length(aList), ' activos.');
    end;
  end;
end;

// *****************************************************************************
// * DATA                                                                      *
// *****************************************************************************

procedure TDataOrigin_PPI._LoadDataFromJSON (var aData : TAssetData; JSONObject : TJSONObject);
var
  JSONText, VolStr : String;
  FSettings : TFormatSettings;

  // PPI devuelve valores con punto como separador de decimales.
  function PreprocessToQWord(const AStr: string; Default: QWord): QWord;
  var
   CleanStr: string;
   PosE: Integer;
   MantissaStr, ExponentStr: string;
   Mantissa: Double;
   Exponent: Integer;
  begin
   CleanStr := Trim(AStr); // Elimina espacios en blanco

   try
    PosE := Pos('E', UpperCase(CleanStr)); // Busca 'E' o 'e'
    if PosE > 0 then
     begin
      // Separar mantisa y exponente -------------------------
      ExponentStr := Copy(CleanStr, PosE + 2, Length(CleanStr));
      MantissaStr := Copy(CleanStr, 1, PosE - 1);

      // Configurar TFormatSettings para usar punto como separador decimal

      // Convertir mantisa a número (sin 'E')
      Mantissa := StrToFloatDef(MantissaStr, 0, FSettings); // Usa StrToFloatDef para la parte decimal
      Exponent := StrToIntDef(ExponentStr, 0);   // Exponente como entero

      // Calcular el valor: Mantissa * 10^Exponent
      Result := Trunc(Mantissa * Power(10, Exponent));

      //WriteLn(MantissaStr, '[', Mantissa, ']  =  ' + ExponentStr, '[', Exponent, '] --> ', Result);
     end
    else Result := StrToQWordDef(CleanStr, Default); // Si no hay 'E', asumir entero directo
   except
     Result := Default; // En caso de error, devolver el valor por defecto
   end;
 end;

begin
  JSONText := JSONObject.AsJSON;
  FSettings.DecimalSeparator := '.';
  FSettings.ThousandSeparator := ','; // O cualquier otro carácter que no sea '.'

  with aData do
   begin
    DateTime  := FixISO8601ToDate( JSONObject.Get('date', '') );

    Prices.Close    := StrToFloatDef( _GetValueFromJSON(JSONText, 'price'), 0, FSettings);
    Prices.Open     := StrToFloatDef( _GetValueFromJSON(JSONText, 'openingPrice'), 0, FSettings);
    Prices.High     := StrToFloatDef( _GetValueFromJSON(JSONText, 'max'), 0, FSettings);
    Prices.Low      := StrToFloatDef( _GetValueFromJSON(JSONText, 'min'), 0, FSettings);
    Prices.Previous := StrToFloatDef( _GetValueFromJSON(JSONText, 'previousClose'), 0, FSettings);

    VolStr := _GetValueFromJSON(JSONText, 'volume');
    Volume := PreprocessToQWord(_GetValueFromJSON(JSONText, 'volume'), 0);
   end;
end;

function TDataOrigin_PPI.CurrentData(aAssetID : TAssetID; var aData : TAssetData): Boolean;
var
  Response : String;
  JSONObject : TJSONObject;
begin
  LOGIN_CHECK;

  // ******************************************************************
  Response := '';
  _AddParam('Ticker', aAssetID.Symbol);
  _AddParam('Type', fAssetTypeTable[aAssetID.TypeAsset]);
  _AddParam('Market', aAssetID.Market);
  _AddParam('Settlement', fSettlementTypeTable[sttmHOURS24]);
  Result := _Get(URL_MARKETDATA + 'Current', Response);
  // ******************************************************************

  WriteLN(aAssetID.Symbol);
  WriteLN(Response);

  if Result then
   begin
    JSONObject := TJSONObject(GetJSON(Response));
    _LoadDataFromJSON(aData, JSONObject);
    JSONObject.Free; // Liberar memoria
   end;
end;

function TDataOrigin_PPI.HistoricData (const aAssetID : TAssetID; var aList : TAssetDataArray; aDays : Integer) : Boolean;
var
  i : Integer;
  Response: String;
  JSONData: TJSONArray;
  JSONObject: TJSONObject;
begin
  LOGIN_CHECK;
  SetLength(aList, 0);

  // ******************************************************************
  Response := '';
  _AddParam('Ticker', aAssetID.Symbol);
  _AddParam('Type', fAssetTypeTable[aAssetID.TypeAsset]);
  _AddParam('DateFrom', DateToISO8601(Date - aDays));
  _AddParam('DateTo', DateToISO8601(Date));
  _AddParam('Settlement', fSettlementTypeTable[sttmHOURS24]);
  Result := _Get(URL_MARKETDATA + 'Search', Response);
  // *******************************************************************

  if Result then
   begin
    JSONData := GetJSON(Response) as TJSONArray;
    SetLength(aList, JSONData.Count);

    for i := 0 to JSONData.Count - 1 do
     begin
      JSONObject := JSONData.Items[i] as TJSONObject; // Obtener el JSON
      _LoadDataFromJSON(aList[i], JSONObject);
      JSONObject.Free; // Liberar memoria
     end;
   end;

  // Verificar si hay datos ---------------------
  Result := not (Length(aList) = 0);
  if Result then SortAssetDataArray(aList); // Ordenar por fecha por precaución
end;

end.

