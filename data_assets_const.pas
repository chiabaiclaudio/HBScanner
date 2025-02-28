unit Data_Assets_Const;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


const
  // GROUPS ===============================
  GRP_ASSET  = 'ASSET_';
  GRP_MARK   = 'MARK_';
  GRP_PRICE  = 'PRICE_';
  GRP_VOLUME = 'VOLUME_';
  GRP_HB     = 'HB_';
  GRP_DERIV  = 'DERIV_';
  GRP_UPDATE = 'UPDATE_';
  GRP_SCORE  = 'SCORE_';

  GRP_ARRAY : array of String = (
    GRP_ASSET, GRP_MARK, GRP_PRICE, GRP_VOLUME,
    GRP_HB, GRP_DERIV, GRP_UPDATE, GRP_SCORE );

  // ID ===================================
  FLD_ASSET_ID         = GRP_ASSET + 'ID';  // ID Interno
  FLD_ASSET_MARKET     = GRP_ASSET + 'MARKET';
  FLD_ASSET_TYPE       = GRP_ASSET + 'TYPE';
  FLD_ASSET_SETTLEMENT = GRP_ASSET + 'SETTLEMENT';
  FLD_ASSET_SYMBOL     = GRP_ASSET + 'SYMBOL';
  FLD_ASSET_CURRENCY   = GRP_ASSET + 'CURRENCY';

  // Descripción --------------------------
  FLD_ASSET_DESCRIPTION = GRP_ASSET + 'DESCRIPTION';
  FLD_ASSET_NOTE        = GRP_ASSET + 'NOTE';

  // Marcadores ===========================
  FLD_MARK_PORTFOLIO = GRP_MARK + 'PORTFOLIO';
  FLD_MARK_BLACKLIST = GRP_MARK + 'BLACKLIST';
  FLD_MARK_FAVORITE  = GRP_MARK + 'FAVORITE';
  FLD_MARK_CATEGORY  = GRP_MARK + 'CATEGORY'; FLD_MARK_CATEGORY_NOVALUE = -1;
  FLD_MARK_ZONE      = GRP_MARK + 'ZONE';     FLD_MARK_ZONE_NOVALUE = FLD_MARK_CATEGORY_NOVALUE;

  // Datos ================================
  // Precios ------------------------------
  FLD_PRICE_CLOSE_LAST = GRP_PRICE + 'CLOSE_LAST'; // Precio anterior
  FLD_PRICE_CLOSE      = GRP_PRICE + 'CLOSE'; // Precio actual
  FLD_PRICE_OPEN       = GRP_PRICE + 'OPEN';
  FLD_PRICE_LOW        = GRP_PRICE + 'LOW';
  FLD_PRICE_HIGH       = GRP_PRICE + 'HIGH';

  // Cambio de precios --------------------
  FLD_PRICE_CHANGE_CC         = GRP_PRICE + 'CHANGE_CC';
  FLD_PRICE_CHANGE_OC         = GRP_PRICE + 'CHANGE_OC';
  FLD_PRICE_CHANGE_THREE_DAY  = GRP_PRICE + 'CHANGE_3DAY';
  FLD_PRICE_CHANGE_WEEK       = GRP_PRICE + 'CHANGE_WEEK';
  FLD_PRICE_CHANGE_HALF_MONTH = GRP_PRICE + 'CHANGE_HALF_MONTH';
  FLD_PRICE_CHANGE_MONTH      = GRP_PRICE + 'CHANGE_MONTH';
  //FLD_PRICE_CHANGE_MONTH_3    = 'CHANGE_MONTH_3'; // Para cuando este implementado
  //FLD_PRICE_CHANGE_MONTH_6    = 'CHANGE_MONTH_6'; // el archiVO de HISTORICOS

  // Volumne ------------------------------
  FLD_VOLUME        = GRP_VOLUME + 'ACTUAL';
  FLD_VOLUME_LAST   = GRP_VOLUME + 'LAST';
  FLD_VOLUME_CHANGE = GRP_VOLUME + 'CHANGE';

  FLD_VOLUME_ORDER        = GRP_VOLUME + 'ORDER'; // VOLUME / PRICE
  FLD_VOLUME_ORDER_LAST   = GRP_VOLUME + 'ORDER_LAST';
  FLD_VOLUME_ORDER_CHANGE = GRP_VOLUME + 'ORDER_CHANGE';

  // Heart Beat ---------------------------
  FLD_HB_RAW    = GRP_HB + 'RAW';    // HB cruda
  FLD_HB_BASE   = GRP_HB + 'BASE';   // HB * Sensibilidad ( = 1 )
  FLD_HB_FORCE  = GRP_HB + 'FORCE';  // HB_BASE * Sensibilidad
  FLD_HB_VOLUME = GRP_HB + 'VOLUME'; // HB_FORCE ajustado por volumen
  FLD_HB_SPREAD_VOL = GRP_HB + 'SPREAD_VOL';

  FLD_HB_SENSIBILITY = GRP_HB + 'SENSIBILITY';

  // Derivados ----------------------------
  FLD_DERIV_SMA     = GRP_DERIV + 'SMA';
  FLD_DERIV_EMA     = GRP_DERIV + 'EMA';
  FLD_DERIV_WMA     = GRP_DERIV + 'WMA';
  FLD_DERIV_SWMA    = GRP_DERIV + 'SWMA';
  FLD_DERIV_SMA_WMA = GRP_DERIV + 'SMA_WMA';

  // Señales ------------------------------
  FLD_SCORE      = GRP_SCORE + 'MAIN';
  FLD_SCORE_BULL = GRP_SCORE + 'BULL';
  FLD_SCORE_BEAR = GRP_SCORE + 'BEAR';
  FLD_SCORE_DESCRIPTION = GRP_SCORE + 'DESCRIPTION';

  // Actualizacion ========================
  FLD_UPDATE_TIME = GRP_UPDATE + 'TIME';
  FLD_UPDATE_CODE = GRP_UPDATE + 'CODE'; FLD_UPDATE_CODE_NOVALUE = -1;
  FLD_UPDATE_MSG  = GRP_UPDATE + 'MSG';

  // --------------------------------------
  FIELDS_EXPORT : array of String = (
    FLD_ASSET_ID, FLD_ASSET_MARKET, FLD_ASSET_TYPE,
    FLD_ASSET_SETTLEMENT, FLD_ASSET_SYMBOL, FLD_ASSET_CURRENCY,
    FLD_ASSET_DESCRIPTION, FLD_ASSET_NOTE,
    FLD_MARK_PORTFOLIO, FLD_MARK_BLACKLIST, FLD_MARK_FAVORITE,
    FLD_MARK_CATEGORY, FLD_MARK_ZONE,
    FLD_HB_SENSIBILITY );

implementation

end.

