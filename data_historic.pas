unit Data_Historic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Market;

const
  TOLERANCE_DAYS = 5;
  DEFAULT_PERIOD = 31;

Type
  THistDict = specialize TDictionary<String, TAssetDataArray>;

type

  { TDataHistoric }

  TDataHistoric = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    fData : THistDict;

    function _FindWorkingDay(aDate : TDate) : TDate;
    function _FindWorkingPeriod(aDays : Integer) : Integer;

  public
    function Historic(var aList: TAssetDataArray; const aAssetID: TAssetID; aDays : Integer = DEFAULT_PERIOD ): Boolean;
    function FindPreviousData(var aData : TAssetData; const aAssetID : TAssetID; aDays : Integer) : Boolean;
  end;

var
  DataHistoric: TDataHistoric;

implementation

uses Data_Provider, DateUtils;

{$R *.lfm}

procedure TDataHistoric.DataModuleCreate(Sender: TObject);
begin
  fData := THistDict.Create;
end;

procedure TDataHistoric.DataModuleDestroy(Sender: TObject);
begin
  fData.Free;
end;

// =============================================================================
// == UTILES
// =============================================================================

// Buscar el día habíl más antiguo ------------------------------------------
function TDataHistoric._FindWorkingDay(aDate : TDate) : TDate;
begin
  while (DayOfTheWeek(aDate) in [1, 7]) do // 1 = Domingo, 7 = Sábado
    aDate := IncDay(aDate, -1);

  Result := aDate;
end;

function TDataHistoric._FindWorkingPeriod(aDays : Integer) : Integer;
var
  aDate : TDate;
begin
  aDate := _FindWorkingDay(IncDay(Date, - aDays));
  Result := DaysBetween(Date, aDate);
end;

// =============================================================================
// == DATOS HISTORICOS
// =============================================================================

{ TODO : Implementar un cache y revisión de datos guardados. (file.db) }

function TDataHistoric.Historic(var aList: TAssetDataArray; const aAssetID: TAssetID; aDays : Integer ): Boolean;
var
  ID : String;
  InCache : Boolean;
  DateToFind : TDate;
begin
  // NOTA: Se aseuma que el pedido de datos historicos viene sin saltos
  // (excepto feriodso y fines de semana) y ordenado.
  Result := False;
  ID := aAssetID;

  // Buscar en cache ----------------------
  InCache := fData.TryGetValue(ID, aList);
  // --------------------------------------

  if InCache then
   begin // Ver si el caceh actual tiene el largo pedido
    if aDays = 0 then aDays := 1;
    DateToFind := _FindWorkingDay(IncDay(Now, - Abs(aDays)));

    // La ultima fecha involucra la fecha buscada?
    Result := Trunc(aList[High(aList)].DateTime) <= DateToFind;  { #todo : Algo no esa bine aqui. si la fecha no está en cache no pide datos hasta tenerla. auqnue peude qu eno exista }
   end
  else
   begin // Cargar al cache ----------------------
    Result := DataProvider.Historic(aList, aAssetID, aDays + TOLERANCE_DAYS);
    if Result then fData.AddOrSetValue(ID, aList) // Agregar / Reemplazar en el cache
   end;

  // Borrar la lista si la carga falla
  if not Result then SetLength(aList, 0);
end;

function TDataHistoric.FindPreviousData(var aData : TAssetData; const aAssetID : TAssetID; aDays : Integer) : Boolean;
var
  aDate : TDate;
  aList : TAssetDataArray;
  i : Integer;
begin
  Result := Historic(aList, aAssetID, aDays);
  aDate := IncDay(Date, - aDays); //_FindWorkingDay();

  if Length(aList) > 0 then
   for i := Low(aList) to High(aList) do
    if (Trunc(aDate) - Trunc(aList[I].DateTime)) >= 1 then
     begin
      aData := aList[I];
      Exit(True);
     end;

  //WriteLN( ' No encontrado');
end;

end.

