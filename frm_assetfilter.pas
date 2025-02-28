unit frm_assetfilter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, EditBtn, ExtCtrls, StdCtrls, Buttons, Types;

Type
  TPriceRange = record
    Text   : String;
    ValueA : Real;
    ValueB : Real;
  end;

type

  { TfrmAssetsFilter }

  TfrmAssetsFilter = class(TFrame)
    btnResetFilters: TSpeedButton;
    chkHBConfirm: TCheckBox;
    cmbCategory: TComboBox;
    cmbCurrencyType: TComboBox;
    cmbFilterHBRange: TComboBox;
    cmbFilterType: TComboBox;
    cmbPriceRange: TComboBox;
    edtFilterSymbol: TEditButton;
    Panel1: TPanel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    stTextQuickfilter: TStaticText;
    procedure cmbFilterTypeDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure edtFilterSymbolButtonClick(Sender: TObject);
    procedure edtFilterSymbolChange(Sender: TObject);
  private
    fPricesRangeArray: array of TPriceRange;

    procedure _CreateComboItems;
    procedure _CreatePricesRangesItems;
    procedure _CreateCategoryItems;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses HeartBeat, Config, Market, Graphics, LCLType;

{$R *.lfm}

constructor TfrmAssetsFilter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  _CreateComboItems;

  //btnResetFiltersClick(nil); // Resetear boton de filtro.
end;


procedure TfrmAssetsFilter._CreateComboItems;
var
  I : Integer;
  AssetType : TAssetType;
  CurrencyType : TCurrencyType;
begin
  // Tipo de activo -------------------------------------------
  with cmbFilterType.Items do
   begin
    Clear; AddPair('Todos los tipos', '-1'); I := 0;
    for AssetType in TAssetType do
     begin
      if HeartBeat_IsValidType(AssetType) then AddPair(AssetTypeToStr(AssetType), IntTostr(Ord(AssetType)));
      Inc(I);
     end;
   end;

  // Tipo de moneda --------------------------------------------
  with cmbCurrencyType.Items do
    begin
     Clear; AddPair('Todas las monedas', '-1'); I := 0;
     for CurrencyType in TCurrencyType do
      begin
       AddPair(CurrencyTypeToStr(CurrencyType), IntTostr(Ord(CurrencyType)));
       Inc(I);
      end;
     Move(1, Count - 1);
    end;

  // Precios ---------------------------------------------------
  _CreatePricesRangesItems;
  _CreateCategoryItems;
end;

procedure TfrmAssetsFilter._CreateCategoryItems;
var
  I : Integer;
begin
  // Categorias ----------------------------------
  with cmbCategory do
   begin
    Clear;
    Items.AddPair('Todas las categorías', '-1');

    with Configuration.Categories do
     for I := 0 to Count - 1 do
      Items.AddPair(Values[Names[I]], Names[I]);

    ItemIndex := 0;
   end;
end;

procedure TfrmAssetsFilter._CreatePricesRangesItems;

  procedure LoadPricesRanges(aText : String; aMax : real; aMin : Real = 0);
  var
    Count : Integer;
  begin
   Count := Length(fPricesRangeArray);
   SetLength(fPricesRangeArray, Count + 1);

   // Cargar array --------------------------
   with fPricesRangeArray[Count] do
    begin
     Text := aText;
     ValueA := aMin;
     ValueB := aMax;
    end;

   // Cargar item TODOS -----------------------------
   cmbPriceRange.Items.AddPair(aText, '-1');
  end;

begin
  cmbPriceRange.Items.Clear;
  LoadPricesRanges('Todos los valores', -1, -1);

  LoadPricesRanges('Menos de 1k',   1000);
  LoadPricesRanges('Menos de 5k',   5000);
  LoadPricesRanges('Menos de 10k', 10000);
  LoadPricesRanges('Menos de 15k', 15000);
  LoadPricesRanges('Menos de 20k', 20000);

  LoadPricesRanges( 'De 5k a 10k', 10000,  5000);
  LoadPricesRanges('De 10k a 15k', 15000, 10000);
  LoadPricesRanges('De 15k a 20k', 20000, 15000);
  LoadPricesRanges('De 20k a 25k', 25000, 20000);
end;

// *****************************************************************************
// ** DIBUJAR                                                                 **
// *****************************************************************************

procedure TfrmAssetsFilter.cmbFilterTypeDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Txt : String;
  X, Y: Integer;
begin
  with (Control as TComboBox), Canvas do
   begin
    if odSelected in State then
     begin
      Brush.Color := clHighlight;
      Font.Color  := clHighlightText;
     end;

     // Rellenar el fondo del ítem
     FillRect(ARect);

     // Calcular las coordenadas para centrar el texto
     Txt := Items.Names[Index];
     X := ARect.Left + (ARect.Width  - TextWidth (Txt)) div 2;
     Y := ARect.Top  + (ARect.Height - TextHeight(Txt)) div 2;
     TextOut(X, Y, Txt); // Dibujar el texto centrado
  end;
end;

procedure TfrmAssetsFilter.edtFilterSymbolButtonClick(Sender: TObject);
begin

end;

procedure TfrmAssetsFilter.edtFilterSymbolChange(Sender: TObject);
begin

end;

end.

