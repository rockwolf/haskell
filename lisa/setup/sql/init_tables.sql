BEGIN;
/* markets */
INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(1, '', '', '', 1, current_date, current_date);

/* t_account */
INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('income', 'income', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('expenses', 'expenses', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets', 'assets', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('liabilities', 'liabilities', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('equity', 'equity', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:prepaid_expenses', 'prepaid_expenses', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('liabilities:prepaid_income', 'prepaid_income', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:accounts_receivable', 'accounts_receivable', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('liabilities:accounts_payable', 'accounts_payable', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:current_assets', 'current_assets', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:reimbursements', 'reimbursements', 1, current_date, current_date);

/* currencies */
INSERT INTO T_CURRENCY(currency_id, code, description)
values(1, 'EUR', 'Euro');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(2, 'USD', 'United States Dollar');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(3, 'CHF', 'Swiss Frank');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(4, 'CAD', 'Canadian Dollar');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(5, 'JPY', 'Japanese Yen');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(6, 'NZD', 'New Zealand Dollar');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(7, 'AUD', 'Australian Dollar');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(8, 'GBP', 'British Pound');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(9, 'HKD', 'Hong Kong Dollar');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(10, 'DKK', 'Danish Krone');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(11, 'PLN', 'Polish Zloty');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(12, 'MXN', 'Mexican Peso');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(13, 'SEK', 'Swedish Krona');

INSERT INTO T_CURRENCY(currency_id, code, description)
values(14, 'RUB', 'Russian Ruble');

/* commodity type */
INSERT INTO T_COMMODITY_TYPE(name, description, active, date_created, date_modified)
values('', '', 1, current_date, current_date);

/* commodity general */
INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
values('', 1, 1, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, current_date, current_date);

/* commodity names */
INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('', '', 1, 1, 1, current_date, current_date);

/* t_rate */
INSERT INTO T_RATE(commission, tax, automatic_flag, date_created, date_modified)
values(-1.0, -1.0, 0, current_date, current_date);

/* t_parameter */
-- None

/* t_version */
INSERT INTO T_VERSION
values(1, '2.0', 'Lisa with trading options.', current_date, current_date);

COMMIT;
