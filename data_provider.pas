unit Data_Provider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Data_Origin, Data_Origin_PPI, Market;

type
  TAssetArray = array of TAsset;

/// <summary>
/// Esta es una clase para agrupar diferentes TDataSource y fusionar sus resultados.
/// Con esta clase se pueden obtener diferentes datos de diferentes cuentas.
/// </summary>

type
  { TDataProvider }

  TDataProvider = class(TDataModule)

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

  private
    fOrigins: array of TDataOrigin; { #todo : Cambiar a Tlist o similar para un manejo más versatil. }
    fActiveOrigin: TDataOrigin;

    function _GetOrigin(Index: Integer):  TDataOrigin;

    // Metodo de busqeuda general ----------------------------
    function _Search( const aSearchType : Integer;
        var aList: TAssetArray; const aSearchTerm: String = '';
        const aTypeFilter: TAssetType = asstUNKNOWN ): Boolean;

  public
    // Origenes ==========================================
    procedure AddOrigin(aSource: TDataOrigin);
    procedure RemoveOrigin(aSource: TDataOrigin);

    property ActiveOrigin : TDataOrigin read fActiveOrigin;
    property Origin[Index: Integer]: TDataOrigin read _GetOrigin;

    procedure SetActiveOrigin(Index: Integer);
    //procedure SetSourceByID (ID : String);

    // BUSQUEDAS ============================================================
    // Con filtro por mercado ------------------------------------------
    function Search(const aSearchTerm : String; var aList : TAssetArray;
        const aMarketFilter : String = '') : Boolean;

    // Con filtro por Tipo y Mercado -----------------------------------
    function Search(const aSearchTerm : String; var aList : TAssetArray;
        const aTypeFilter : TAssetType; const aMarketFilter : String = '') : Boolean;

    // Portfolio -----------------------------------------------------
    function Portfolio(var aList : TAssetArray) : Boolean;

    // DATOS ===============================================================
    function Current( var aData: TAssetData; aAssetID: TAssetID): Boolean;
    function Historic(var aList: TAssetDataArray; const aAssetID: TAssetID; aDays: Integer = 32): Boolean;
  end;

var
  DataProvider: TDataProvider;

implementation


const
  SEARCH_GENERAL   = 0;
  SEARCH_BYTYPE    = 1;
  SEARCH_PORTFOLIO = 2;


{$R *.lfm}

{ TDataProvider }

procedure TDataProvider.DataModuleCreate(Sender: TObject);
begin
  // Limpiar y agregar una fuente inicial
  SetLength(fOrigins, 0);
  AddOrigin(TDataOrigin_PPI.Create); // Por ahora solo PPI.
  fActiveOrigin := fOrigins[0];
end;

procedure TDataProvider.DataModuleDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(fOrigins) do fOrigins[i].Free;
  SetLength(fOrigins, 0);
end;

// =============================================================================
// == ORIGENES                                                                ==
// =============================================================================

procedure TDataProvider.AddOrigin(aSource: TDataOrigin);
begin
  SetLength(fOrigins, Length(fOrigins) + 1);
  fOrigins[High(fOrigins)] := aSource;
end;

procedure TDataProvider.RemoveOrigin(aSource: TDataOrigin);
var
  i, j: Integer;
begin
  for i := 0 to High(fOrigins) do
   if fOrigins[i] = aSource then
    begin
     fOrigins[i].Free; // Liberar origen
     // Desplazar el resto cubriendo el espacio
     for j := i to High(fOrigins) - 1 do
       fOrigins[j] := fOrigins[j + 1];

     SetLength(fOrigins, Length(fOrigins) - 1); // Ajustar el array

     // Ajustar el origen por defecto
     if Length(fOrigins) > 0 then
       fActiveOrigin := fOrigins[0] { #todo : Cambiar esto por el ID y verificar si el remove cambio o no el ActiveOrigin.  }
     else fActiveOrigin := nil;

     Exit;
    end;
end;

function TDataProvider._GetOrigin (Index : Integer) : TDataOrigin;
begin
  if (Index >= 0) and (Index < Length(fOrigins)) then
   Result := fOrigins[index]
  else
   Result := nil;
end;

procedure TDataProvider.SetActiveOrigin(Index: Integer);
var
  NewSource : TDataOrigin;
begin
  NewSource := _GetOrigin(Index);
  if not (NewSource = nil) then fActiveOrigin := NewSource;
end;

// =============================================================================
// == BUSCAR                                                                  ==
// =============================================================================

function TDataProvider._Search( const aSearchType : Integer; var aList: TAssetArray;
    const aSearchTerm: String; const aTypeFilter: TAssetType ): Boolean;
var
  i, j, k: Integer;
  SearchCount : Integer;
  Flag : Boolean;
  tmpList: TAssetArray;
  Exists: Boolean;
begin
  if Length(fOrigins) = 0 then
   begin
    WriteLN(Self.Name + ': No hay origenes definidos.');
    Exit(False);
   end;

  // Inicuialziar --------------------------------------
  SetLength(aList, 0);
  SearchCount := 0;
  Result := False;

  // Recorrer origines de datos =======================
  for i := 0 to High(fOrigins) do
   begin

    // Tipo de busqueda ///////////////////////////////////////////
    case aSearchType of
      SEARCH_BYTYPE : // Buscar filtrado por tipo
       Flag := fOrigins[i].Search( tmpList, aSearchTerm, aTypeFilter);
      SEARCH_PORTFOLIO : // Obtener cartera
       Flag := fOrigins[i].PortFolio(tmpList);
     else // Busqueda general
      Flag := fOrigins[i].Search(tmpList, aSearchTerm);
    end;
    // //////////////////////////////////////////////////////////

    WriteLN('Lista obtenido ' , Length(tmpList), ' ' );
    if Flag then Inc(SearchCount); // Numero de busquedas efectuadas

    if Flag and (Length(tmpList) > 0 ) then // Operacion exitosa? Agregar al listado final
     for j := 0 to High(tmpList) do // Verificar si ya existe ....
      begin
       Exists := False;
       for k := 0 to High(aList) do
        if aList[k].ID = tmpList[j].ID then
         begin
          Exists := True;
          Break;
         end;

       // Si no existe agregar -------------------
       if not Exists then
        begin
         SetLength(aList, Length(aList) + 1);
         aList[High(aList)] := tmpList[j];
        end;
       end;

    Result := not(SearchCount = 0);
   end;
end;

// Con filtro por mercado (GENERAL) ============================================
function TDataProvider.Search(const aSearchTerm : String; var aList : TAssetArray;
    const aMarketFilter : String = '') : Boolean;
begin
  Result := _Search(SEARCH_GENERAL, aList, aSearchTerm, asstUNKNOWN);
end;

// Con filtro por Tipo y Mercado ===============================================
function TDataProvider.Search(const aSearchTerm : String; var aList : TAssetArray;
    const aTypeFilter : TAssetType; const aMarketFilter : String) : Boolean;
begin
  Result := _Search(SEARCH_BYTYPE, aList, aSearchTerm, aTypeFilter);
end;

// En cartra (SEARCH_PORTFOLIO) ================================================
function TDataProvider.Portfolio(var aList : TAssetArray) : Boolean;
begin
  Result := _Search(SEARCH_PORTFOLIO, aList);
end;

// =============================================================================
// == DATOS                                                                   ==
// =============================================================================

function TDataProvider.Current(var aData: TAssetData; aAssetID: TAssetID): Boolean;
begin
  Result := Assigned(fActiveOrigin) and fActiveOrigin.CurrentData(aAssetID, aData);
end;

function TDataProvider.Historic(var aList: TAssetDataArray; const aAssetID: TAssetID;  aDays: Integer): Boolean;
begin
  Result := Assigned(fActiveOrigin) and fActiveOrigin.HistoricData(aAssetID, aList, aDays);
end;

end.

