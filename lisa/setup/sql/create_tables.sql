BEGIN;

/* finance */
CREATE TABLE "1".T_ACCOUNT
(
    account_id serial not null,
    name varchar(4000) not null,
    description varchar(4000) not null,
    active int not null default 1,
    date_created timestamp not null default '1900-01-01',
    date_modified timestamp not null default '1900-01-01',
    constraint pk_account_id primary key(account_id)
);

CREATE TABLE "1".T_POOL
(
    pool_id serial not null,
    account_id int not null,
    total decimal(18,6) not null default 0.0,
    invested decimal(18,6) not null default 0.0,
    cash decimal(18,6) not null default 0.0,
    active int not null default 1,
    date_created timestamp not null default '1900-01-01',
    date_modified timestamp not null default '1900-01-01',
    constraint fk_account_id foreign key(account_id) references T_ACCOUNT(account_id)
);

CREATE TABLE "1".T_MARKET
(
    market_id int not null,
    code varchar(50) not null,
    name varchar(30) not null,
    country char(3) not null,
    active int not null default 1,
    date_created timestamp not null default '1900-01-01',
    date_modified timestamp not null default '1900-01-01',
    constraint pk_market_id primary key(market_id),
    unique(market_id),
    unique(code)
);

CREATE TABLE "1".T_CURRENCY
(
    currency_id int not null,
    code varchar(3) not null default '',
    description varchar(256) not null default '',
    constraint pk_currency_id primary key(currency_id),
    unique(currency_id)
);

CREATE TABLE "1".T_CURRENCY_EXCHANGE
(
    currency_exchange_id serial not null,
    currency_from_id int not null default -1,
    currency_to_id int not null default -1,
    exchange_rate decimal(18,6) not null default 0.0,
    date_created timestamp not null default '1900-01-01',
    date_modified timestamp not null default '1900-01-01',
    constraint pk_currency_exchange_id primary key(currency_exchange_id),
    constraint fk_currency_from_id foreign key(currency_from_id) references T_CURRENCY(currency_id),
    constraint fk_currency_to_id foreign key(currency_to_id) references T_CURRENCY(currency_id)
);

CREATE TABLE "1".T_CFD_GENERAL
(
    cfd_general_id serial not null,
    name varchar(50) not null,
    market_id int not null default -1,
    currency_id int not null default 1,
    tick decimal(18, 6) not null default 1.0,
    tick_value decimal(18, 6) not null default 1.0,
    order_min decimal(18, 6) not null default -1.0,
    order_max decimal(18, 6) not null default -1.0,
    margin_day_proc decimal(18, 6) not null default -1.0,
    margin_night_proc decimal(18, 6) not null default -1.0,
    date_created timestamp not null default '1900-01-01',
    date_modified timestamp not null default '1900-01-01',
    constraint fk_market_id foreign key(market_id) references T_MARKET(market_id),
    constraint fk_currency_id foreign key(currency_id) references T_CURRENCY(currency_id),
    constraint pk_cfd_general_id primary key(cfd_general_id)
);

CREATE TABLE "1".T_COMMODITY_TYPE
(
    commodity_type_id serial not null,
    name varchar(50) not null,
    description varchar(256) not null default '',
    active int not null default 1,
    date_created timestamp not null default '1900-01-01',
    date_modified timestamp not null default '1900-01-01',
    constraint pk_commodity_type_id primary key(commodity_type_id)
);

CREATE TABLE "1".T_COMMODITY
(
    commodity_id serial not null,
    name varchar(50) not null,
    description varchar(256) not null default '',
    commodity_type_id int not null default 1,
    cfd_general_id int not null default -1,
    active int not null default 1,
    date_created timestamp not null default '1900-01-01',
    date_modified timestamp not null default '1900-01-01',
    constraint pk_commodity_id primary key(commodity_id),
    constraint fk_cfd_general_id foreign key(cfd_general_id) references T_CFD_GENERAL(cfd_general_id),
    constraint fk_commodity_type_id foreign key(commodity_type_id) references T_COMMODITY_TYPE(commodity_type_id)
);

CREATE TABLE "1".T_PARAMETER
(
    parameter_id int not null,
    name varchar(50) not null,
    value varchar(512) not null,
    description varchar(256) not null,
    constraint pk_parameter_id primary key(parameter_id),
    unique(parameter_id)
);

-- when automatic is checked, the gui should calculate it on the fly.
-- Then, when writing the records to T_RATE, the automatically calculated values
-- should be put in T_RATE.
-- This table thus contains commission and tax values, which might
CREATE TABLE "1".T_RATE
(
    rate_id serial not null,
    commission decimal(18, 6) not null default 0.0,
    tax decimal(18, 6) not null default 0.0,
    automatic_flag int not null default -1,
    date_created timestamp not null default '1900-01-01',
    date_modified timestamp not null default '1900-01-01',
    constraint pk_rate_id primary key(rate_id)
);

/* main finance table */
CREATE TABLE "1".T_DRAWDOWN
(
    drawdown_id serial not null,
    drawdown_current int not null default 0,
    drawdown_max int not null default 0,
    date_created timestamp not null default '1900-01-01',
    date_modified timestamp not null default '1900-01-01',
    constraint pk_drawdown_id primary key(drawdown_id),
    unique(drawdown_id)
);

CREATE TABLE "1".T_TRADE_CALCULATED
(
    trade_calculated_id serial not null,
    price_buy decimal(18,6) not null default 0.0,
    price_sell decimal(18,6) not null default 0.0,
    amount_buy decimal(18,6) not null default 0.0,
    amount_sell decimal(18,6) not null default 0.0,
    amount_buy_simple decimal(18,6) not null default 0.0,
    amount_sell_simple decimal(18,6) not null default 0.0,
    risk_input decimal(18,6) not null default 0.0,
    risk_initial decimal(18,6) not null default 0.0,
    risk_initial_percent decimal(18,6) not null default 0.0,
    risk_actual decimal(18,6) not null default 0.0,
    risk_actual_percent decimal(18,6) not null default 0.0,
    cost_total decimal(18,6) not null default 0.0,
    cost_other decimal(18,6) not null default 0.0,
    stoploss decimal(18,6) not null default 0.0,
    stoploss_orig decimal(18,6) not null default 0.0,
    profit_loss decimal(18,6) not null default 0.0,
    profit_loss_orig decimal(18,6) not null default 0.0,
    profit_loss_total decimal(18,6) not null default 0.0,
    profit_loss_total_percent decimal(18,6) not null default 0.0,
    r_multiple decimal(18,6) not null default 0.0,
    constraint pk_trade_calculated_id primary key(trade_calculated_id)
);

CREATE TABLE "1".T_TRADE_POOL
(
    trade_pool_id serial not null,
    pool_at_start decimal(18,6) not null default 0.0,
    pool_override decimal(18,6) not null default 0.0,
    date_created timestamp not null default '1900-01-01',
    date_modified timestamp not null default '1900-01-01',
    constraint pk_trade_pool_id primary key(trade_pool_id)
);

  
-- TODO: add orig fields and other missing ones, cfr. ods
CREATE TABLE "1".T_TRADE
(
    trade_id serial not null,
    trade_calculated_id int not null default -1,
    market_id int not null,
    commodity_id int not null,
    date_buy timestamp not null default '1900-01-01',
    year_buy int not null default 1900,
    month_buy int not null default 1,
    day_buy int not null default 1,
    date_sell timestamp not null default '1900-01-01',
    year_sell int not null default 1900,
    month_sell int not null default 1,
    day_sell int not null default 1,
    is_long int not null default -1,
    price_buy_orig decimal(18,6) not null default 0.0,
    price_sell_orig decimal(18,6) not null default 0.0,
    shares_buy int not null default 0,
    shares_sell int not null default 0,
    comment varchar(256) not null default '',
    currency_exchange_id_buy int not null default -1,
    currency_exchange_id_sell int not null default -1,
    rate_id_buy int not null default -1,
    rate_id_sell int not null default -1,
    risk_input_percent decimal(18,6) not null default 0.0,
    is_winner int not null default -1,
    drawdown_id int not null default -1,
    trade_pool_id int not null default -1,
    date_expiration timestamp not null default '1900-01-01',
    is_expired int not null default -1,
    active int not null default 1,
    reconciled int not null default -1,
    date_created timestamp not null default '1900-01-01',
    date_modified timestamp not null default '1900-01-01',
    --constraint fk_account_from_id foreign key(account_from_id) references T_ACCOUNT(account_id);
    --constraint fk_account_to_id foreign key(account_to_id) references T_ACCOUNT(account_id);
    constraint pk_trade_id primary key(trade_id),
    constraint fk_commodity_id foreign key(commodity_id) references T_COMMODITY(commodity_id),
    constraint fk_rate_id_buy foreign key(rate_id_buy) references T_RATE(rate_id),
    constraint fk_rate_id_sell foreign key(rate_id_sell) references T_RATE(rate_id),
    constraint fk_currency_exchange_id_buy foreign key(currency_exchange_id_buy) references T_CURRENCY_EXCHANGE(currency_exchange_id),
    constraint fk_currency_exchange_id_sell foreign key(currency_exchange_id_sell) references T_CURRENCY_EXCHANGE(currency_exchange_id),
    constraint fk_trade_calculated_id foreign key(trade_calculated_id) references T_TRADE_CALCULATED(trade_calculated_id),
    constraint fk_drawdown_id foreign key(drawdown_id) references T_DRAWDOWN(drawdown_id),
    constraint fk_trade_pool_id foreign key(trade_pool_id) references T_TRADE_POOL(trade_pool_id)
);

CREATE TABLE "1".T_VERSION
(
    version_id int not null,
    version varchar(100) not null default '',
    version_info varchar(100) not null default '',
    date_created timestamp not null default '1900-01-01',
    date_modified timestamp not null default '1900-01-01'
);

COMMIT;
