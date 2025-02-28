unit Market;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TAssetType = (
    asstUNKNOWN,       // Desconocido: Valor por Defecto [0].
    asstSTOCK,         // Acción
    asstCDAR,          // CErtificado de DEposito ARgentino ( C.D.Ar )
    asstBOND,          // BOno
    asstTREASURY_BILL, // Letras
    asstMUTUAL_FUND,   // Fondo Común de Inversión ( F.C.I. )
    asstON,            // Obligación Negociable
    asstSTOCK_US,      // Acción USA
    asstNOBAC,
    asstLEBAC,
    asstLENDING,       // Licitaciones (PPI)
    asstETF,
    asstOPTION,
    asstFUTURE,
    asstFOREIGN_MUTUAL_FUND,
    asstSECURITY_BOND, // Caución
    asstCRIPTO
  );

const
  AsseTypeCategorisable = [asstSTOCK, asstCDAR, asstMUTUAL_FUND, asstON,
                            asstSTOCK_US, asstETF, asstOPTION, asstFUTURE,
                            asstFOREIGN_MUTUAL_FUND];

Type
  TAssetTypeArray = array of TAssetType;

Type
  TCurrencyType = (
    crcyUNKNOWN,
    crcyARS,   // Peso argentino
    crcyUSD,   // Dólar estadounidense
    crcyUSD_MEP,
    crcyUSD_CCL);

{    crcyEUR,   // Euro
    crcyGBP,   // Libra esterlina británica
    crcyJPY,   // Yen japonés
    crcyCAD,   // Dólar canadiense
    crcyAUD,   // Dólar australiano
    crcyCHF,   // Franco suizo
    crcyCNY,   // Yuan chino
    crcyMXN ); // Peso mexicano }

Type
  TCurrencyTypeArray = array of TCurrencyType;

Type
  TSettlementType = (
    sttmUNKNOWN,
    sttmNOW,
    sttmHOURS24,
    sttmHOURS48,
    sttmHOURS72
);

Type
  TSettlementTypeArray = array of TSettlementType;

Type
  TAssetID = record
    Market      : String; // Por ahora, siempre BYMA. (COnsiderar cuando se agreguen cripto)
    TypeAsset   : TAssetType;
    Symbol      : String;
  end;

Type
  TAssetIDArray = array of TAssetID;

operator =  (A, B: TAssetID): Boolean;
operator <> (A, B: TAssetID): Boolean;
operator := (ID : TAssetID): String;

Type
  TAsset = record
    ID          : TAssetID;
    Description : String;
    Currency    : TCurrencyType;
    Settlement  : TSettlementType;
  end;

Type
  TAssetArray = array of TAsset;

const
  AssetSymbol_ValidChar = ['A'..'Z', '0'..'9', '.', ' '];

// DATA ==============================================================
Type
  TAssetPrices = record
    Previous : Real;
    Open     : Real;
    High     : Real;
    Low      : Real;
    Close    : Real;
  end;

Type
  TCandleStick = TAssetPrices;

operator = (A, B: TAssetPrices): Boolean;
function IsSimilar ( A, B : TAssetPrices) : Boolean;

Type
  TAssetData = record
    DateTime : TDatetime;
    Prices   : TAssetPrices;
    Volume   : QWord; // UInt64;
  end;

Type
  TAssetDataArray = array of TAssetData;

 {Type
  TAssetStats = record
    Min : TAssetPrices;
    Max : TAssetPrices;
    Average : TAssetPrices;
  end; }


function AssetInit(aSymbol: String = ''; aTypeAsset: TAssetType = asstUNKNOWN; aMarket : String = '';
                               aDescription: String = ''; aCurrency : TCurrencyType = crcyUNKNOWN;
                               aSettlement : TSettlementType = sttmHOURS24) : TAsset;
function AssetInit(aSymbol: String = ''; aTypeAsset: Integer = 0; aMarket : String = '';
                               aDescription: String = ''; aCurrency : Integer = 0;
                               aSettlement : Integer = 2) : TAsset;

function AssetIDInit(aSymbol: String; aTypeAsset: Integer; aMarket : String) : TAssetID;

function AssetTypeToStr(const aValue : TAssetType; const aLong : Boolean = True) : String;
function CurrencyTypeToStr(const aValue : TCurrencyType; const aLong: Boolean  = True; const aShowVariation : Boolean = True) : String;
function SettlementTypeToStr(const aValue : TSettlementType; const aLong: Boolean = True) : String;

generic function IntToType<T>(const aValue: Integer): T;

////////////////////////////////////////////////////////////////////////////////
// Calculos

function IsPricesValid (Prices : TAssetPrices) : Boolean;

////////////////////////////////////////////////////////////////////////////////
// Array - Utilitarios

procedure SortAssetDataArray (var aArray: TAssetDataArray; ASC : Boolean = False);

implementation

uses Tools, Math;

////////////////////////////////////////////////////////////////////////////////
// Sobrecarga de operadores
//

// TAssetID ---------------------------------
operator = (A, B: TAssetID): Boolean;
begin
  Result := (A.Market    = B.Market) and
            (A.TypeAsset = B.TypeAsset) and
            (A.Symbol    = B.Symbol);
end;

operator <> (A, B: TAssetID): Boolean;
begin
  Result := not (A = B);
end;

operator := (ID: TAssetID): String; // Sobrecarga del operador para conversión implícita
begin
  with ID do Result := Market + '_' + IntToStr(Ord(TypeAsset)) + '_' + Symbol;
end;

// TAssetPrices ---------------------------------
operator =  (A, B: TAssetPrices): Boolean;
begin
  Result := (A.Open = B.Open) and (A.Close = B.Close) and
            (A.High = B.High) and (A.Low = B.Low);
end;

function IsSimilar ( A, B : TAssetPrices) : Boolean;
begin
 Result := (Trunc(A.Open)  = Trunc(B.Open))  and
           (Trunc(A.Close) = Trunc(B.Close)) and
           (Trunc(A.High)  = Trunc(B.High))  and
           (Trunc(A.Low)   = Trunc(B.Low));
end;

////////////////////////////////////////////////////////////////////////7777
// Auxiliares
//

function AssetIDInit(aSymbol: String; aTypeAsset: Integer; aMarket : String) : TAssetID;
begin
  with Result do
   begin
    Market      := aMarket;
    TypeAsset   := TAssetType(aTypeAsset);
    Symbol      := aSymbol;
   end;
end;

function AssetInit(aSymbol: String; aTypeAsset: TAssetType; aMarket : String;
                     aDescription: String; aCurrency : TCurrencyType;
                      aSettlement : TSettlementType = sttmHOURS24) : TAsset;
begin
  with Result do
   begin
     ID := AssetIDInit(aSymbol, Ord(aTypeAsset), aMarket);

     Description    := aDescription;
     Currency       := aCurrency;
     Settlement     := aSettlement;
   end;
end;

function AssetInit(aSymbol: String; aTypeAsset: Integer; aMarket : String;
                     aDescription: String; aCurrency : Integer;
                     aSettlement : Integer) : TAsset;
begin
  Result :=  AssetInit(aSymbol, specialize IntToType<TAssetType>(aTypeAsset),
                      aMarket, aDescription, TCurrencyType(aCurrency), TSettlementType(aSettlement));
end;

////////////////////////////////////////////////////////////////////////////////
// <Type> To String
//

function AssetTypeToStr(const aValue : TAssetType; const aLong : Boolean) : String;
begin
   case aValue of
    asstBOND                : Result := 'Bono';
    asstSTOCK               : Result := IfThenStr(aLong, 'Acción', 'Acc.');
    asstSTOCK_US            : Result := IfThenStr(aLong, 'Acción (U.S.A.)', 'Acc.USA');
    asstCDAR                : Result := IfThenStr(aLong, 'C.D.Ar', 'CDAr');

    asstTREASURY_BILL       : Result := IfThenStr(aLong, 'Letra', 'Let.');
    asstNOBAC               : Result := 'NOBAC';
    asstLEBAC               : Result := 'LEBAC';
    asstLENDING             : Result := IfThenStr(aLong, 'Licitaciones', 'Lic.');

    asstON                  : Result := IfThenStr(aLong, 'Obligación Negociable', 'ON');
    asstSECURITY_BOND       : Result := IfThenStr(aLong, 'Caución', 'Cau.');
    asstOPTION              : Result := IfThenStr(aLong, 'Opción', 'Opc.');
    asstFUTURE              : Result := IfThenStr(aLong, 'Futuro', 'Fut.');

    asstMUTUAL_FUND         : Result := IfThenStr(aLong, 'F.C.I.', 'FCI');
    asstFOREIGN_MUTUAL_FUND : Result := IfThenStr(aLong, 'F.C.I. (Exterior)', 'FCI(ext.)');
    asstETF                 : Result := IfThenStr(aLong, 'E.T.F.', 'ETF');

    asstCRIPTO              : Result := IfThenStr(aLong, 'Criptomoneda', 'Crip.');
   else
    // asstUNKNOWN
    Result := IfThenStr(aLong, 'Tipo Desconocido', '???');
   end;
end;

function CurrencyTypeToStr(const aValue : TCurrencyType; const aLong: Boolean; Const aShowVariation : Boolean = True) : String;
begin
   case aValue of
    crcyARS     : Result := IfThenStr(aLong, 'Peso argentino', 'ARS');
    crcyUSD, crcyUSD_MEP, crcyUSD_CCL  :
     begin
      Result := IfThenStr(aLong, 'Dólar estadounidense', 'USD');
      if aShowVariation then
       begin
        if aValue = crcyUSD_MEP then Result := Result + IfThenStr(aLong, ' (MEP)', '-MEP');
        if aValue = crcyUSD_CCL then Result := Result + IfThenStr(aLong, ' (CCL)', '-CCL');
       end;
     end
{    crcyEUR     : Result := IfThenStr(aLong, 'Euro', 'EUR');
    crcyGBP     : Result := IfThenStr(aLong, 'Libra esterlina británica', 'GBP');
    crcyJPY     : Result := IfThenStr(aLong, 'Yen japonés', 'JPY');
    crcyCAD     : Result := IfThenStr(aLong, 'Dólar canadiense', 'CAD');
    crcyAUD     : Result := IfThenStr(aLong, 'Dólar australiano', 'AUD');
    crcyCHF     : Result := IfThenStr(aLong, 'Franco suizo', 'CHF');
    crcyCNY     : Result := IfThenStr(aLong, 'Yuan chino', 'CNY');
    crcyMXN     : Result := IfThenStr(aLong, 'Peso mexicano', 'MXN'); }
   else
    Result := IfThenStr(aLong, 'Moneda desconocida', '???');
   end;
end;

function SettlementTypeToStr(const aValue : TSettlementType; const aLong: Boolean ) : String;
begin
  case aValue of
   sttmNOW     : Result := IfThenStr(aLong, 'Inmediato', 'Inmed.');
   sttmHOURS24 : Result := IfThenStr(aLong, '24 horas',  '24hs');
   sttmHOURS48 : Result := IfThenStr(aLong, '48 horas',  '48hs');
   sttmHOURS72 : Result := IfThenStr(aLong, '72 horas',  '72hs');
  else
   Result := IfThenStr(aLong, 'Plazo desconocido', '???');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Intger to <Type>

generic function IntToType<T>(const aValue: Integer): T;
begin
  if (aValue >= Ord(Low(T))) and (aValue <= Ord(High(T))) then
    Result := T(aValue)
  else
    Result := Default(T);
end;

{
function IntToCurrencyType (const aValue : Integer) : TCurrencyType;
begin
  if  (aValue >= Ord(Low(TCurrencyType))) or (aValue <= Ord(High(TCurrencyType))) then
    Result := TCurrencyType(aValue)
   else
    Result := crcyUNKNOWN;
end;

function IntToAssetType (const aValue : Integer) : TAssetType;
begin
  if  (aValue >= Ord(Low(TAssetType))) or (aValue <= Ord(High(TAssetType))) then
    Result := TAssetType(aValue)
   else   // No se puede convertir -> Desconocido
    Result := asstUNKNOWN;
end; }

////////////////////////////////////////////////////////////////////////////////
// Calculos

function IsPricesValid (Prices : TAssetPrices) : Boolean;
begin
  with Prices do
   Result := not( (Close <= 0) or (Open <= 0) or (Low <= 0) or (High <= 0) );
end;

procedure SortAssetDataArray (var aArray: TAssetDataArray; ASC : Boolean);
var
 i, j: Integer;
 Temp: TAssetData;
begin
 if Length(aArray) < 2 then Exit; // No hay nada que ordenar si hay 0 o 1 elementos

 for i := 0 to High(aArray) - 1 do
  for j := 0 to High(aArray) - i - 1 do
   // Cambiar la comparación según el valor de Ascendente
   if (ASC and (aArray[j].DateTime > aArray[j + 1].DateTime)) or
      (not ASC and (aArray[j].DateTime < aArray[j + 1].DateTime)) then
    begin
     Temp := aArray[j];
     aArray[j] := aArray[j + 1];
     aArray[j + 1] := Temp;
   end;
end;

end.

