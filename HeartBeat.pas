unit HeartBeat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Market, Graphics, Math;

const
  HB_SIGNAL  = 75;
  HB_CONFIRM = HB_SIGNAL * 2;

  HB_BULL =   HB_SIGNAL;
  HB_BEAR = - HB_SIGNAL;

  HB_BULL_CONFIRM =   HB_CONFIRM;
  HB_BEAR_CONFIRM = - HB_CONFIRM;

  HB_SENSIBILITY_DEFAULT = 1;

Type
  THBRange = (
   hbrngBearZone,       //       -Infinite <--> Zero
   hbrngBearConfirm,    //       -Infinite <--> HB_BEAR_CONFIRM
   hbrngBearish,        // HB_BEAR_CONFIRM <--> HB_BEAR
   hbrngBearishLateral, //         HB_BEAR <--> Zero
   hbrngLateral,        //         HB_BEAR <--> HB_BULL
   hbrngBullishLateral, //            Zero <--> HB_BULL
   hbrngBullish,        //         HB_BULL <--> HB_BULL_CONFIRM
   hbrngBullConfirm,    // HB_BULL_CONFIRM <--> +Infinite
   hbrngBullZone );     //            Zero <--> +Infinite

Type
  THBRangeLimits = record
    MinValue: Double;
    MaxValue: Double;
  end;

const
  HBRange: array[THBRange] of THBRangeLimits = (
    (MinValue: NegInfinity;     MaxValue: ZeroValue),       // hbrngBear
    (MinValue: NegInfinity;     MaxValue: HB_BEAR_CONFIRM), // hbrngBearConfirm
    (MinValue: HB_BEAR_CONFIRM; MaxValue: HB_BEAR),         // hbrngBearish
    (MinValue: HB_BEAR;         MaxValue: ZeroValue),       // hbrngBearishLateral
    (MinValue: HB_BEAR;         MaxValue: HB_BULL),         // hbrngLateral
    (MinValue: ZeroValue;       MaxValue: HB_BULL),         // hbrngBullishLateral
    (MinValue: HB_BULL;         MaxValue: HB_BULL_CONFIRM), // hbrngBullish
    (MinValue: HB_BULL_CONFIRM; MaxValue: Infinity),        // hbrngBullConfirm
    (MinValue: ZeroValue;       MaxValue: Infinity)         // hbrngBull
  );

const
  HB_AssetTypeValid = [
    asstSTOCK, asstCDAR, asstBOND, asstTREASURY_BILL,
    asstON, asstSTOCK_US, asstNOBAC, asstLEBAC, asstCRIPTO ];

Type
  THBData = Record
    Raw    : Real;
    Force  : Real;
  end;

Type
  THBAssetData = record
    Prices : TAssetPrices;
    Volume : Integer;
    HB     : THBData;
  end;

function HeartBeat_IsValidType(aAssetType : TAssetType): Boolean;

// CALCULO DE FUERZAS ======================================================
function HeartBeat_RAW(aClose, aOpen, aHigh, aLow: Double): Real;
function HeartBeat_RAW(aPrices : TAssetPrices): Real;

function HeartBeat_Force (CurrentHBRaw, PreviousHBRaw : Real; aSensibility: Integer = HB_SENSIBILITY_DEFAULT) : Real;
function HeartBeat_Force (CurrentPrices, PreviousPrices : TAssetPrices; aSensibility: Integer = HB_SENSIBILITY_DEFAULT) : Real;

function HeartBeat_Volume (HBForce : Integer; CurrentVolume, PreviousVolume : Integer) : Integer;

// Rango de valores (NO ForceRaw) -----------------------
function HeartBeat_ToRange (const Force : Integer) : THBRange;

implementation

uses TypInfo;

function HeartBeat_IsValidType(aAssetType : TAssetType): Boolean;
begin
  Result := aAssetType in HB_AssetTypeValid;
end;

// *****************************************************************************
// *                                                                           *
// * HEART BEAT : Cruda (Raw)                                                  *
// * - Juego de fuerzas dentro del movimiento de precios en un momento.        *
// *                                                                           *
// *****************************************************************************

function HeartBeat_RAW(aClose, aOpen, aHigh, aLow: Double): Real;
var
  xCenter, xOpen, xLow, xHigh, xClose : Real;
  yCenter, yOpen, yLow, yHigh, yClose : Real;
  fOpen, fLow, fHigh, fClose : Real;
  Distance : Real;
begin
    // Calcular las componentes de la fuerza -----
    Distance := aHigh - aLow;

    // Calcular centro ---------------------------
    xCenter := 0; yCenter := ( aClose + aOpen + aHigh + aLow ) / 4;

    // Calcular fuerzas al centro ----------------
    xOpen  := xCenter - Distance;  yOpen  := yCenter - aOpen;
    xHigh  := xCenter;             yHigh  := yCenter - aHigh;
    xLow   := xCenter;             yLow   := yCenter - aLow * -1; // Dirección contraria
    xClose := xCenter + Distance;  yClose := yCenter - aClose;

    // Calcular vectores -------------------------
    fOpen  := Sqrt( Power( xOpen,  2) + Power( yOpen,  2) );
    fHigh  := Sqrt( Power( xHigh,  2) + Power( yHigh,  2) );
    fLow   := Sqrt( Power( xLow,   2) + Power( yLow,   2) );
    fClose := Sqrt( Power( xClose, 2) + Power( yClose, 2) );

    // Sumar las fuerzas -------------------------
    Result := fOpen + fLow + fHigh + fClose;
end;

function HeartBeat_RAW(aPrices : TAssetPrices): Real;
begin
  with aPrices do Result := HeartBeat_Raw(Close, Open, High, Low);
end;

// *****************************************************************************
// * HEART BEAT : Fuerza (Comparada)                                           *
// * - Razón entre las fuerza RWA de la actual ronda y la anterior.            *
// *****************************************************************************

function HeartBeat_Force(CurrentHBRaw, PreviousHBRaw : Real; aSensibility: Integer) : Real;
var
  HBForce : Real;
begin
  HBForce := CurrentHBRaw / PreviousHBRaw; // Valor comparado
  HBForce := (HBForce - 1) * 1250; // --> ( ( Result * 50 ) - 50 ) * 25; // Valores para ajustar la escala
  Result  := HBForce * aSensibility;
end;

function HeartBeat_Force (CurrentPrices, PreviousPrices : TAssetPrices; aSensibility: Integer) : Real;
begin
  Result := HeartBeat_Force(HeartBeat_RAW(CurrentPrices), HeartBeat_RAW(PreviousPrices), aSensibility);
end;

// *****************************************************************************
// * HEART BEAT : Fuerza corregida por razón de Volumen                        *
// *****************************************************************************

function HeartBeat_Volume ( HBForce : Integer;  CurrentVolume, PreviousVolume : Integer) : Integer;
begin
  Result := Trunc( HBForce * Abs(CurrentVolume / PreviousVolume) );
end;

function HeartBeat_ToRange (const Force : Integer) : THBRange;
begin
  { #todo : Modificar con el nuevo array }
  if Force <= HB_BEAR_CONFIRM  then Result := hbrngBearConfirm    else
   if Force <= HB_BEAR          then Result := hbrngBearish        else
    if Force <= 0                then Result := hbrngBearishLateral else
     if Force <= HB_BULL          then Result := hbrngBullishLateral else
      if Force <= HB_BULL_CONFIRM  then Result := hbrngBullish        else
       Result := hbrngBullConfirm;
end;


end.

