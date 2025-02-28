unit Tools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, TypInfo, Graphics;

// STRING ======================================================================

function IsEmptyString(const AString: String): Boolean;

function Pluralize(const aCount:  Integer; aPluralForm: String = 's'; aSingularForm : String = ''): String;
function Pluralize(const aPlural: Boolean; aPluralForm: String = 's'; aSingularForm : String = ''): String;

function RemoveEnclosingQuotes(const S: string): string;

function IfThenInt(Value: Boolean; const ValueTrue: Integer; const ValueFalse: Integer): Integer;
function IfThenStr(Value: Boolean; const ValueTrue: String;  const ValueFalse: string = ''): String;

function FormatValue(aValue: Double): string;

// If ==========================================================================

function MinValue(a, b: Real): Real;
function MaxValue(a, b: Real): Real;

function CheckRange(Min, Max, Value : Real): Real;

// Files =======================================================================

function FileIsReadable(const FileName: TFileName): Boolean;

// Date Time ===================================================================

function FixISO8601ToDate(const DateTimeString: string): TDateTime;

// Memoria =====================================================================

function GetMemoryUsed: LongInt;
function GetMemoryUsedFPC: LongInt;

// Calculos ====================================================================

generic function PercentChange<T>(aLastValue, aCurrentValue: T): Double;

// Colores =====================================================================

function GetLuminance(aR, aG, aB: Byte): Double;
function GetLuminanceFromColor(Color: TColor): Double;

function GetContrastRatio(R1, G1, B1, R2, G2, B2: Byte): Double;
function GetContrastRatioFromColors(Color1, Color2: TColor): Double;

// DEPURACION ==================================================================

function GetEnumNameValue(EnumType: PTypeInfo; Value: Integer): string;

implementation

uses Math;

///////////////////////////////////////////////////////////////////////7777
// STRING's

function IsEmptyString(const AString: string): Boolean;
begin
  Result := (Trim(AString) = '');
end;

function RemoveEnclosingQuotes(const S: string): string;
begin
  if (Length(S) > 1) and (S[1] = '"') and (S[Length(S)] = '"') then
    Result := Copy(S, 2, Length(S) - 2)
  else
    Result := S;
end;

function FormatValue(aValue: Double): string;
const
  Units : array[0..4] of string = ('', 'K', 'M', 'B', 'T');
var
  Magnitud: Integer;
begin
  Magnitud := 0;

  // Ajusta la magnitud y divide el valor según la magnitud
  while (aValue >= 1000) and (Magnitud < High(Units)) do
   begin
    aValue := aValue / 1000;
    Inc(Magnitud);
   end;

  // Formatea el valor con 2 decimales y agrega la unidad correspondiente
  Result := FormatFloat('0.##', aValue) + Units[Magnitud];
end;

function Pluralize(const aCount: Integer; aPluralForm: String = 's'; aSingularForm : String = ''): String;
begin
  Result := Pluralize(aCount > 1, aPluralForm, aSingularForm);
end;

function Pluralize(const aPlural: Boolean; aPluralForm: String = 's'; aSingularForm : String = '' ): String;
begin
  Result := ifThenStr(aPlural, aPluralForm, aSingularForm);
end;

function IfThenInt(Value: Boolean; const ValueTrue: Integer; const ValueFalse: Integer): Integer;
begin
  Result := specialize IfThen<Integer>(Value, ValueTrue, ValueFalse);
end;

function IfThenStr(Value: Boolean; const ValueTrue: string; const ValueFalse: string = ''): String;
begin
  if (Value) then
    Result := ValueTrue
  else
    Result := ValueFalse;
end;

////////////////////////////////////////////////////////////////////////////////
// MATH

function MinValue(a, b: Real): Real;
begin
  if a < b then Result := a else Result := b;
end;

function MaxValue(a, b: Real): Real;
begin
  if a > b then Result := a else Result := b;
end;

function CheckRange(Min, Max, Value : Real): Real;
var
  AMin , aMax : Real;
begin
  // Check range -------------------------
  aMin := Trunc(MinValue(Min, Max));
  aMax := Trunc(MaxValue(Min, Max));

  // Check value -------------------------
  Result := Value;
  if Value < aMin then Result := Value;
  if Value > aMax then Result := Value;
end;

////////////////////////////////////////////////////////////////////////////////
// FILE

function FileIsReadable(const FileName: TFileName): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  try
   FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
   try
    Result := FS.Size > 0;
   finally
    FS.Free;
   end;
  except
   on E: EFOpenError do Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// DATE TIME

function FixISO8601ToDate(const DateTimeString: string): TDateTime;
var
  dotIndex : Integer;
  tmp : STring;
begin
  tmp := DateTimeString;
  dotIndex := Pos('.', tmp); // Busca la posición del punto
  if dotIndex > 0 then tmp := Copy(tmp, 1, dotIndex - 1); // Elimina la parte de los milisegundos si se encuentra un punto

  Result := ISO8601ToDate(tmp);
end;

////////////////////////////////////////////////////////////////////////////////
// Memoria

function GetMemoryUsedFPC: LongInt;
var
  HeapStatus: TFPCHeapStatus;
begin
  HeapStatus := GetFPCHeapStatus;
  Result := HeapStatus.MaxHeapUsed div 1024; // Retorna la memoria usada en KB
end;

function GetMemoryUsed: LongInt;
var
  StatusFile: TextFile;
  Line: string;
  Value: LongInt;
begin
  AssignFile(StatusFile, '/proc/self/status');
  Reset(StatusFile);

  try
   while not EOF(StatusFile) do
    begin
     ReadLN(StatusFile, Line);
     if Pos('VmRSS:', Line) = 1 then
      begin
       // VmRSS: Resident set size (the portion of a process's memory held in RAM)
       Line := Trim(Copy(Line, Pos(':', Line) + 1, Length(Line)));
       Line := Trim(Copy(Line, 1, Pos(' ', Line) - 1)); // Extract the numerical value
       if TryStrToInt(Line, Value) then Result := Value; // Value is in KB
       Exit;
      end;
    end;
  finally
    CloseFile(StatusFile);
  end;

  Result := -1; // Return -1 if VmRSS not found
end;

////////////////////////////////////////////////////////////////////////////////
// CALCULOS
//

generic function PercentChange<T>(aLastValue, aCurrentValue: T): Double;
var
  Divisor : Real;
begin
  // Si el valor anterior es 0, retornamos 0 como resultado
  if not(aLastValue = 0) then
   begin
    Divisor := aLastValue * IfThenInt(aLastValue < 0, -1, 1); // Abs no soporta UInt64
    Result := (aCurrentValue - aLastValue) / Divisor * 100.0;
   end else Result := 0.0;
end;

////////////////////////////////////////////////////////////////////////////////
// COLORES
//

function GetLuminance(aR, aG, aB: Byte): Double;
var
  R, G, B : Real;
begin
  // Convertir RGB a valores entre 0 y 1
  R := aR / 255;
  G := aG / 255;
  B := aB / 255;

  // Aplicar la fórmula para la luminosidad relativa
  // Usamos la fórmula estándar de la W3C para la luminosidad
  if R <= 0.03928 then
    R := R / 12.92
  else
    R := Power((R + 0.055) / 1.055, 2.4);

  if G <= 0.03928 then
    G := G / 12.92
  else
    G := Power((G + 0.055) / 1.055, 2.4);

  if B <= 0.03928 then
    B := B / 12.92
  else
    B := Power((B + 0.055) / 1.055, 2.4);

  // Calcular la luminosidad relativa
  Result := 0.2126 * R + 0.7152 * G + 0.0722 * B;
end;

function GetLuminanceFromColor(Color: TColor): Double;
begin
  Result := GetLuminance(Red(Color), Green(Color), Blue(Color));
end;

function GetContrastRatio(R1, G1, B1, R2, G2, B2: Byte): Double;
var
  L1, L2: Double;
begin
  // Calcular luminosidad de los dos colores
  L1 := GetLuminance(R1, G1, B1);
  L2 := GetLuminance(R2, G2, B2);

  // Asegurar que L1 sea el color más claro
  if L1 < L2 then
    Result := (L2 + 0.05) / (L1 + 0.05)
  else
    Result := (L1 + 0.05) / (L2 + 0.05);
end;

function GetContrastRatioFromColors(Color1, Color2: TColor): Double;
begin
  Result := GetContrastRatio(Red(Color1), Green(Color1), Blue(Color1),
                             Red(Color2), Green(Color2), Blue(Color2));
end;


////////////////////////////////////////////////////////////////////////////////
// DEBUG
//

function GetEnumNameValue(EnumType: PTypeInfo; Value: Integer): string;
begin
  Result := GetEnumName(EnumType, Value);
end;

end.

