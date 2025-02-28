unit Data_Origin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Market, Tools, fphttpclient,
  httpprotocol, fpjson, opensslsockets;


Type // Listas de datos --------------------------
  TAssetArray = array of TAsset;
  TAssetDataArray = array of TAssetData;


Type // Origen de datos --------------------------
  TDataOrigin = class(TObject)
  private
    RESTClient : TFPHTTPClient;
    fHeaders : TStringList;
    fParams : TStringList;

    fServerCode    : Integer;
    fServerMessage : String;
    fInternalError : String;

    fMarket : String;
    procedure  _SetMarketDefault(aValue : String);

    procedure _Prepare; // Prepara Request
  protected

    function _ID : String; virtual; abstract;
    function _Description : String; virtual; abstract;
    function _ShortDescription : String; virtual; abstract;
    function _MarketDefault: String; virtual; abstract;

    procedure _PrepareCustom; virtual;
    procedure _AddHeader(aHeader : String);
    procedure _AddParam(aKey, aValue : String);

    function _Execute(aURL: String; aGetMethod : Boolean; var aResponse: String) : Boolean;
    function _Get (aURL: String; var aResponse: String) : Boolean;
    function _Post(aURL: String; var aResponse: String) : Boolean;

    function _GetValueFromJSON(const JSONString: string; aNameValue : String): string;

  public
    constructor Create;
    destructor  Destroy; override;

    // Datos del DataSource ====================================================
    property ID : String read _ID;
    property Description      : String read _Description;
    property ShortDescription : String read _ShortDescription;
    property Market           : String read fMarket write _SetMarketDefault;

    // Mensajes del Servidor ===================================================
    property ServerCode    : Integer read fServerCode;
    property ServerMessage : String  read fServerMessage;
    property InternalError : String  read fInternalError;

    // Search ==================================================================
    // Con filtro por mercado -----------------------------------------------
    function Search(var aList : TAssetArray; const aSearchTerm : String ) : Boolean; virtual; abstract;

    // Con filtro por Tipo y Mercado ----------------------------------------
    function Search( var aList : TAssetArray; const aSearchTerm : String; const aTypeFilter : TAssetType) : Boolean; virtual; abstract;

    // Portfolio ------------------------------------------------------------
    function PortFolio (var aList : TAssetArray) : Boolean; virtual; abstract;

    // Precios =================================================================
    function CurrentData(aAssetID : TAssetID; var aData : TAssetData): Boolean; virtual; abstract;
    function HistoricData (const aAssetID : TAssetID; var aList : TAssetDataArray;
          aDays : Integer = 32) : Boolean; virtual; abstract ;

    // DATA ==============================================================
    // function ExtractVolumeValue(const JSONString: string): string;

  end;

implementation

uses HTTPCodes;

constructor TDataOrigin.Create;
begin
  inherited Create;

  RESTClient := TFPHTTPClient.Create(Nil);
  fHeaders := TStringList.Create;
  fParams  := TStringList.Create;

  fMarket := 'BYMA';
end;

destructor TDataOrigin.Destroy;
begin
  RESTClient.Free;
  fHeaders.Free;
  fParams.Free;

  inherited Destroy;
end;

procedure  TDataOrigin._SetMarketDefault(aValue : String);
begin
  aValue := Trim(aValue);
  if not IsEmptyString(aValue) then fMarket := aValue;
end;

// *****************************************************************************
// * Metodos BASE                                                              *
// *****************************************************************************

procedure TDataOrigin._AddHeader(aHeader : String);
begin
  fHeaders.Add(aHeader);
end;

procedure TDataOrigin._AddParam(aKey, aValue : String);
begin
  fParams.AddPair(Trim(aKey), Trim(aValue));
end;

procedure TDataOrigin._Prepare;
var
  header : String;
begin
  with RESTCLient.RequestHeaders do
   begin
    // Iniciar cabecera ---------------
    Clear;
    _PrepareCustom;
    for header in fHeaders do Add(header); // Agregar cabeceras extras
  end;
end;

procedure TDataOrigin._PrepareCustom;
begin
  //
end;


// *****************************************************************************
// * METODO CENTRAL                                                            *
// *****************************************************************************

function TDataOrigin._Execute(aURL: String; aGetMethod : Boolean; var aResponse: String) : Boolean;
const
  MAX_RETRIES = 3;
var
  Arguments : String;
  Response  : String;
  Param : String;
  i : Integer;
  RetryCount : Integer;
  RetryAfter: Integer;

  function BuildArguments: String;
  var
    Arg: String;
    Idx: Integer;
  begin
    Result := IfThenStr(fParams.Count > 0, '?', '');
    for Idx := 0 to fParams.Count - 1 do
     begin
      Arg := Trim(fParams.Names[Idx]) + '=' +
             StringReplace(fParams.Values[fParams.Names[Idx]], ' ', '%20', [rfReplaceAll]);
      Result := Result + Arg;
      if Idx < fParams.Count - 1 then
        Result := Result + '&';
     end;

    Result := StringReplace(Result, sLineBreak, '', [rfReplaceAll]);
  end;

begin
  // Limpiar errores anteriores ====================================
  Result := False;
  RetryCount := 1;

  // ******************************************************************
  repeat
    try

      // ================================================
      try
       _Prepare; // Preparar headers y otras configuraciones
       aResponse      := '';
       fInternalError := '';
       fServerCode    := 0;
       fServerMessage := '';

       // .......................................
       if aGetMethod then
        Response := RESTCLient.Get(aURL + BuildArguments)
       else Response := RESTCLient.FormPost(aURL, '');
       // .......................................

      except
       on E: Exception do
        fInternalError := E.Message;
      end;
      // ================================================

      // Evaluar respuesta ---------------------------------
      with RESTCLient do
       begin
        fServerCode    := ResponseStatusCode;
        fServerMessage := ResponseStatusText;
       end;
      aResponse := Response;
      Result := (fServerCode = HTTP_OK);

      if not Result then
       begin
        WriteLN('ERROR: ', fServerCode, ' - ', fServerMessage);
        WriteLN('Response (', fServerCode, '):', Response);
        WriteLN('Internal : ', fInternalError);
       end;

    finally
      // Limpiar recursos en cada iteración
      fHeaders.Clear;
      fParams.Clear;
    end;

    Inc(RetryCount);

  until (Result or (RetryCount >= MAX_RETRIES));

  // Última limpieza ===============================
  //if not Result then aResponse := '';
end;

function TDataOrigin._Get(aURL: String; var aResponse: String) : Boolean;
begin
  Result := _Execute(aURL, True, aResponse);
end;

function TDataOrigin._Post(aURL: String; var aResponse: String) : Boolean;
begin
  Result := _Execute(aURL, False, aResponse);
end;


////////////////////////////////////////////////////////////////////////////////
// Obtener valor
//

function TDataOrigin._GetValueFromJSON(const JSONString: string; aNameValue : String): string;
var
  StartPos, EndPos: Integer;
  BaseText, SearchText : String;
begin
  Result := '';
  BaseText := '"' + aNameValue + '"';

  // Sin espacio -----------------
  SearchText := BaseText + ':';
  StartPos := Pos(SearchText, JSONString);

  // Con espacio -----------------
  if StartPos = 0 then
   begin
    SearchText := BaseText + ' :';
    StartPos := Pos(SearchText, JSONString);
   end;

  // EXtraer ----------------------------------------
  if not(StartPos = 0) then
   begin
    // Mover el inicio justo después de ':'
    StartPos := StartPos + Length(SearchText);

    // Buscar el final del valor (asumiendo que es un string o número sin espacios intermedios)
    EndPos := StartPos;
    while (EndPos <= Length(JSONString)) and not (JSONString[EndPos] in [',', '}', #0]) do
      Inc(EndPos);

    // Extraer el valor
    Result := Trim(Copy(JSONString, StartPos, EndPos - StartPos));

    // Remover posibles comillas al inicio y al final
    if (Length(Result) > 0) and (Result[1] = '"') then  Delete(Result, 1, 1);
    if (Length(Result) > 0) and (Result[Length(Result)] = '"') then Delete(Result, Length(Result), 1);
  end;
end;

end.

