BEGIN;

/* V_TRADE */
--DROP VIEW "1".V_TRADE;
CREATE VIEW "1".V_TRADE
AS
select
    t.*
from
    t_trade t 
;

/* V_ACCOUNT */
--DROP VIEW "1".V_ACCOUNT;
CREATE VIEW "1".V_ACCOUNT
AS
select
    a.*
from
    t_account a 
;

/* V_RATE */
--DROP VIEW "1".V_RATE;
CREATE VIEW "1".V_RATE
AS
select
    r.*
from
    t_rate r 
;

/* V_CURRENCY */
--DROP VIEW "1".V_CURRENCY;
CREATE VIEW "1".V_CURRENCY
AS
select
    c.*
from
    t_currency c 
;

/* V_CURRENCY_EXCHANGE */
--DROP VIEW "1".V_CURRENCY_EXCHANGE;
CREATE VIEW "1".V_CURRENCY_EXCHANGE
AS
select
    ce.*
from
    t_currency_exchange ce
;

/* V_COMMODITY */
--DROP VIEW "1".V_COMMODITY;
CREATE VIEW "1".V_COMMODITY
AS
select
    sn.*
from
    t_commodity sn
;

/* V_MARKET */
--DROP VIEW "1".V_MARKET;
CREATE VIEW "1".V_MARKET
AS
select
    m.*
from
    t_market m
;

/* V_DRAWDOWN */
--DROP VIEW "1".V_DRAWDOWN;
CREATE VIEW "1".V_DRAWDOWN
AS
select
    d.*
from
    t_drawdown d 
;

/* V_PARAMETER */
--DROP VIEW "1".V_PARAMETER;
CREATE VIEW "1".V_PARAMETER
AS
select
    p.*
from
    t_parameter p 
;

/* V_REP_CHECK_TOTAL */
-- TODO: get the total from the pool table and update it with each trade?
--DROP VIEW "1".V_REP_CHECK_TOTAL;
/*CREATE VIEW "1".V_REP_CHECK_TOTAL
AS
select
    *
    --a.name as account_name,
    --sum(f.amount) as account_total
from
    t_finance f
    --inner join t_account a on f.account_id = a.account_id
--group by
--    a.name
;
*/

/* V_POOL */
--DROP VIEW "1".V_POOL;
CREATE VIEW "1".V_POOL
AS
select
    p.*
from
    t_pool p 
;

/* V_ACCOUNT_NAME */
--DROP VIEW "1".V_ACCOUNT_NAME;
CREATE VIEW "1".V_ACCOUNT_NAME
AS
select
    a.account_id as account_id
    , coalesce(a.name || ':', '') as name
from 
    T_ACCOUNT a
where
    a.active = 1
;

/* V_EXPECTANCY */
--DROP VIEW "1".V_EXPECTANCY;
CREATE VIEW "1".V_EXPECTANCY
AS
select
    1--sum(t.r_multiple)/count(1) as expectancy
from 
    T_TRADE t
where
    t.active = 1
;

/* V_EXPORT_LEDGER */
--DROP VIEW "1".V_EXPORT_LEDGER;
/*CREATE VIEW "1".V_EXPORT_LEDGER
AS
select
    --f.date --TODO: format?
    ,f.comment
    ,f.amount_debit
    ,f.amount_credit
    --,c.code
    --TODO: finish 
    *
from
    t_finance f
    --inner join t_currency_exchange ce on ce.currency_exchange_id = f.currency_exchange_id
    --inner join t_currency c on f.currency_id_to = c.currency_id_to
;*/

/* V_TRADE_JOURNAL */
--DROP VIEW "1".V_TRADE_JOURNAL;
CREATE VIEW "1".V_TRADE_JOURNAL
AS
select
    t.trade_id/*,
    t.active,
    m.name as market
    , c.name as commodity
    , t.date_buy
    , t.date_sell
    , t.long_flag
    , t.price_buy
    , t.price_sell
    , t.shares_buy
    , t.shares_sell
    , t.commission_buy
    , t.commission_sell
    , t.tax_buy
    , t.tax_sell
    --, t.cost_buy
    --, t.cost_sell
    , t.risk_input
    , t.risk_input_percent
    , t.risk_initial
    , t.risk_initial_percent
    , t.risk_actual
    , t.risk_actual_percent
    , t.stoploss
    , t.stoploss_orig
    , t.profit_loss
    , t.profit_loss_orig
    , t.profit_loss_total
    , t.profit_loss_total_percent
    , t.r_multiple
    , t.win_flag
    --, t.wins
    --, t.wins_percent
    , t.amount_buy
    , t.amount_sell
    , t.amount_buy_simple
    , t.amount_sell_simple
    , d.drawdown_current
    , d.drawdown_max
    , t.price_buy_orig
    , t.price_sell_orig
    --, u_buy.code as currency_from_buy
    --, 'EUR' as currency_to_buy
    --, u_sell.code as currency_from_sell
    --, 'EUR' as currency_to_sell
    --, e_buy.exchange_rate as exchange_rate_buy
    --, e_sell.exchange_rate as exchange_rate_sell
    , t.id_buy
    , t.id_sell*/
from
    t_trade t
    inner join t_market m on t.market_id = m.market_id
    inner join t_commodity c on t.commodity_id = c.commodity_id
    inner join t_drawdown d on t.drawdown_id = d.drawdown_id
    --left join t_finance f_buy on t.id_buy = f_buy.finance_id
    --inner join t_currency_exchange e_buy on f_buy.currency_exchange_id = e_buy.currency_exchange_id
    --inner join t_currency u_buy on e_buy.currency_from_id = u_buy.currency_id
    --left join t_finance f_sell on t.id_sell = f_sell.finance_id
    --inner join t_currency_exchange e_sell on f_sell.currency_exchange_id = e_sell.currency_exchange_id
    --inner join t_currency u_sell on e_sell.currency_from_id = u_sell.currency_id
;

/* V_COMMODITY_TYPE */
--DROP VIEW "1".V_COMMODITY_TYPE;
CREATE VIEW "1".V_COMMODITY_TYPE
AS
select
    ct.*
from
    t_commodity_type ct 
;

/* V_CFD_GENERAL */
--DROP VIEW "1".V_CFD_GENERAL;
CREATE VIEW "1".V_CFD_GENERAL
AS
select
    cfd.*
from
    t_cfd_general cfd 
;

COMMIT;
