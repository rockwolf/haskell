/* MARKETS */
-- Add market names
-- NOTE: see list of country codes at:
-- http://www.iso.org/iso/country_codes/iso_3166_code_lists/country_names_and_code_elements.htm

-- GENERAL
INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(2, 'ams', 'Amsterdam stock exchange AEX25', 'NL', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(3, 'ebr', 'Brussels stock exchange BEL20', 'BE', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(4, 'etr', 'Frankfurt Xetra DAX30', 'DE', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(5, 'epa', 'Paris Stock Exchange CAC40', 'FR', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(6, 'other', 'Other', '', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(7, 'eli', 'Lisbon Stock Exchange', 'PT', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(8, 'lse', 'London Stock Exchange', 'UK', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(9, 'ise', 'Irish Stock Exchange (Dublin)', 'IE', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(10, 'mil', 'Milan Stock Exchange', 'IT', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(11, 'bma', 'Bolsa de Madrid', 'ES', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(12, 'vse', 'Vienna Stock Exchange', 'CH', 1, current_date, current_date);

-- CFD's
INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(13, 'cfd .gold', 'CFD - World Spot Gold', 'US', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(14, 'cfd .silver', 'CFD - World Spot Silver', 'US', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(15, 'cfd oil', 'CFD - Brent and WTI oil', 'US', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(16, 'cfd other non-share', 'CFD - other non-share', '', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(17, 'cfd BE', 'CFD - Belgium', 'BE', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(18, 'cfd FR', 'CFD - France', 'FR', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(19, 'cfd DE', 'CFD - Germany', 'DE', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(20, 'cfd UK', 'CFD - United Kingdom', 'UK', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(21, 'cfd DK', 'CFD - Denmark', 'DK', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(22, 'cfd FI', 'CFD - Finland', 'FI', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(23, 'cfd IT', 'CFD - Italy', 'IT', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(24, 'cfd NL', 'CFD - Netherlands', 'NL', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(25, 'cfd NO', 'CFD - Norway', 'NO', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(26, 'cfd PT', 'CFD - Portugal', 'PT', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(27, 'cfd SE', 'CFD - Sweden', 'SE', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(28, 'cfd CH', 'CFD - Switzerland', 'CH', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(29, 'cfd ES', 'CFD - Spain', 'ES', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(30, 'cfd other share', 'CFD - other share', '', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(31, 'cfd AU', 'CFD - Australia', 'AU', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(32, 'cfd AT', 'CFD - Austria', 'AT', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(33, 'cfd CN', 'CFD - China', 'CN', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(34, 'cfd PL', 'CFD - Poland', 'PL', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(35, 'cfd SG', 'CFD - Singapore', 'SG', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(36, 'nyse', 'Ney York Stock Exchange', 'US', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(37, 'nasdaq', 'Nasdaq', 'US', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(38, 'otc bb & pinksheets', 'OTC BB & pinksheets', 'US', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(39, 'amex', 'American Exchange', 'US', 1, current_date, current_date);

INSERT INTO T_MARKET(market_id, code, name, country, active, date_created, date_modified)
values(40, 'other us', 'Other US markets', 'US', 1, current_date, current_date);

/* T_COMMODITY_TYPE */
INSERT INTO T_COMMODITY_TYPE(name, description, active, date_created, date_modified)
values('cfd', 'Contracts for difference', 1, current_date, current_date);

/* T_CFD_GENERAL */
-- commodities (futures)
INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Brent Crude Oil Futures', 16, 2, 1.0, 1.0, 1.0, 100.0, 1.0, 2.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Carbon Emissions', 16, 1, 1.0, 1.0, 1.0, 250.0, 10.0, 20.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Cattle Live Futures', 16, 2, 0.01, 1.0, 1.0, 500.0, 2.0, 4.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Corn Futures', 16, 2, 1.0, 1.0, 1.0, 1000.0, 3.0, 6.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Gold Futures', 16, 2, 0.1, 1.0, 1.0, 500.0, 0.5, 1.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Heating Oil', 16, 2, 0.01, 1.0, 1.0, 100.0, 10.0, 20.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('High Grade Copper', 16, 2, 0.05, 1.0, 1.0, 500.0, 3.0, 6.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('London Cocoa Futures', 16, 8, 1.0, 1.0, 1.0, 1000.0, 5.0, 10.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('London Coffee Futures', 16, 2, 1.0, 1.0, 1.0, 50.0, 5.0, 10.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('London Gas Oil Futures', 16, 2, 25.0, 1.0, 1.0, 250.0, 4.0, 8.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('London Sugar Futures', 16, 2, 0.1, 1.0, 1.0, 500.0, 2.0, 4.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('London Wheat futures', 16, 8, 0.01, 1.0, 1.0, 100.0, 8.0, 16.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Milling Wheat Futures', 16, 1, 1.0, 1.0, 1.0, 1000.0, 3.0, 6.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Mini Silver Futures', 16, 2, 1.0, 1.0, 1.0, 25.0, 1.0, 2.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Natural Gas', 16, 2, 0.001, 1.0, 1.0, 100.0, 10.0, 20.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Orange Juice', 16, 2, 0.01, 1.0, 1.0, 100.0, 3.0, 6.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Palladium Futures', 16, 2, 0.1, 1.0, 1.0, 50.0, 5.0, 10.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Platinum Futures', 16, 2, 0.1, 1.0, 1.0, 50.0, 5.0, 10.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Rapeseed Futures', 16, 1, 1.0, 1.0, 1.0, 1000.0, 3.0, 6.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Silver futures', 16, 2, 0.1, 1.0, 1.0, 50.0, 1.0, 2.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('US Cocoa', 16, 2, 1.0, 1.0, 1.0, 100.0, 3.0, 6.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('US Coffee C', 16, 2, 0.01, 1.0, 1.0, 100.0, 3.0, 6.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('US Corn Futures', 16, 2, 0.25, 1.0, 1.0, 1500.0, 8.0, 16.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('US Cotton No. 2', 16, 2, 0.01, 1.0, 1.0, 100.0, 3.0, 6.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('US Soybean Oil Futures', 16, 2, 0.01, 1.0, 1.0, 250.0, 8.0, 16.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('US Soybeans Futures', 16, 2, 0.25, 1.0, 1.0, 1000.0, 8.0, 16.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('US Sugar 11', 16, 2, 0.01, 1.0, 1.0, 100.0, 8.0, 16.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('US Wheat futures', 16, 2, 0.25, 1.0, 1.0, 250.0, 8.0, 16.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('WTI Crude Oil Futures', 16, 2, 0.01, 1.0, 1.0, 100.0, 1.0, 2.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Spot Brent Crude Oil', 16, 2, 1.0, 1.0, 1.0, 100.0, 1.0, 2.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Spot Gold', 16, 2, 0.1, 1.0, 1.0, 500.0, 0.5, 1.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Spot Mini Gold', 16, 2, 0.1, 0.1, 1.0, 50.0, 0.5, 1.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Spot Mini Silver', 16, 2, 0.1, 0.1, 1.0, 25.0, 1.0, 2.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Spot Silver', 16, 2, 0.1, 1.0, 1.0, 250.0, 1.0, 2.0, current_date, current_date);

INSERT INTO T_CFD_GENERAL(name, market_id, currency_id, tick, tick_value, order_min, order_max, margin_day_proc, margin_night_proc, date_created, date_modified)
VALUES('Spot WTI Light Crude Oil', 16, 2, 0.01, 1.0, 1.0, 100.0, 1.0, 2.0, current_date, current_date);

/* STOCKS/COMMODITIES */
--TODO: add 1 to every cfd_general_id and commodity_type_id! I forgot there is a default record in each table!
-- CFD's
--- softs
INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('CCZ3.cfd', 'US COCOA, US Dollar DecYY', 2, 22, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('CTZ3.cfd', 'US Cotton No.2, US Dollar/100 DecYY', 2, 25, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('KCZ3.cfd', 'US Coffee C, US Dollar/100 DecYY', 2, 23, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LCCU3.cfd', 'London Cocoa, Pound Sterling SepYY', 2, 9, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LCCZ3.cfd', 'London Cocoa, Pound Sterling DecYY', 2, 9, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LEV3.cfd', 'Live Cattle (per 0.01), US Dollar/100 OctYY', 2, 4, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LRCU3.cfd', 'London Coffee, US Dollar SepYY', 2, 10, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LRCX3.cfd', 'London Coffee, US Dollar NovYY', 2, 10, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LSUV3.cfd', 'London Sugar, US Dollar OctYY', 2, 12, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LSUZ3.cfd', 'London Sugar, US Dollar DecYY', 2, 12, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LWBX3.cfd', 'London Wheat (per 0.01), Pound Sterling NovYY', 2, 13, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('OJU3.cfd', 'Orange Juice, US Dollar/100 SepYY', 2, 17, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('OJX3.cfd', 'Orange Juice, US Dollar/100 NovYY', 2, 17, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('SBV3.cfd', 'US Sugar No11, US Dollar/100 OctYY', 2, 28, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('ZVU3.cfd', 'US Corn, US Dollar/100 SepYY', 2, 5, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('ZCZ3.cfd', 'US Corn, US Dollar/100 DecYY', 2, 5, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('ZLU3.cfd', 'US Soybean Oil, US Dollar/100 SepYY', 2, 26, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('ZLV3.cfd', 'US Soybean Oil, US Dollar/100 OctYY', 2, 26, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('ZSU3.cfd', 'US Soybeans, US Dollar/100 SepYY', 2, 27, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('ZSX3.cfd', 'US Soybeans, US Dollar/100 NovYY', 2, 27, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('ZWU3.cfd', 'US Wheat, US Dollar/100 SepYY', 2, 29, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('ZWZ3.cfd', 'US Wheat, US Dollar/100 DecYY', 2, 29, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('SBH4.cfd', 'US Sugar No11, US Dollar/100 MarYY', 2, 28, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LCCH4.cfd', 'London Cocoa Futures, Pound Sterling MarYY', 2, 9, 1, current_date, current_date);

-- oil
INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('.BRENT.cfd', 'SPOT Brent Crude Oil, US Dollar/100', 2, 31, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('.WTI.cfd', 'SPOT WTI Light Crude Oil, US Dollar', 2, 36, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('CLV3.cfd', 'WTI Crude Oil, US Dollar OctYY', 2, 30, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('CLX3.cfd', 'WTI Crude Oil, US Dollar NovYY', 2, 30, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('HOU3.cfd', 'Heating Oil, US Dollar NovYY', 2, 7, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('HOV3.cfd', 'Heating Oil, US Dollar OctYY', 2, 7, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LCOV3.cfd', 'Brent Crude Oil, US Dollar/100 OctYY', 2, 2, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LCOX3.cfd', 'Brent Crude Oil, US Dollar/100 NovYY', 2, 2, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LGOU3.cfd', 'London Gas Oil (per 25), US Dollar/100 SepYY', 2, 12, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('LGOV3.cfd', 'London Gas Oil (per 25), US Dollar/100 OctYY', 2, 12, 1, current_date, current_date);

-- metals
INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('.GOLD.cfd', 'Spot Gold, US Dollar', 2, 32, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('.MGOLD.cfd ', 'MINI Spot Gold, US Dollar', 2, 33, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('.MSILVER.cfd', 'Spot Mini Silver, US Dollar/100', 2, 34, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('.SILVER.cfd', 'Spot Silver, US Dollar/100', 2, 35, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('GCZ3.cfd', 'Gold, US Dollar DecYY', 2, 6, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('HGU3.cfd ', 'High Grade Copper (per 0.05), US Dollar SepYY', 2, 8, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('HGZ3.cfd', 'High Grade Copper (per 0.05), US Dollar DecYY', 2, 8, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('MINISIU3.cfd', 'Mini Silver, US Dollar/100 SepYY', 2, 15, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('MINISIZ3.cfd ', 'Mini Silver, US Dollar/100 DecYY', 2, 15, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('PAU3.cfd', 'Palladium, US Dollar SepYY', 2, 18, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('PAZ3.cfd', 'Palladium, US Dollar DecYY', 2, 18, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('PLV3.cfd', 'Platinum, US Dollar OctYY', 2, 19, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('SIU3.cfd', 'Silver, US Dollar/100 SepYY', 2, 21, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('SIZ3.cfd', 'Silver, US Dollar/100 DecYY', 2, 21, 1, current_date, current_date);

-- indices
/*INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('.DE30.cfd', 'Germany 30 cash, Euro', 1, X, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('.ES35.cfd', 'Spain 35 cash, Euro', 1, X, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('.F40.cfd ', 'France 40 cash, Euro', 1, X, 1, current_date, current_date);

INSERT INTO T_COMMODITY(name, description, commodity_type_id, cfd_general_id, active, date_created, date_modified)
values('.N25.cfd', 'Netherlands 25 cash, Euro', 1, 1, X, current_date, current_date);*/

/* ACCOUNTS */
-- my accounts
--TODO: fix the names, it should be assets:current_assets:commodities: --> Check in gnucash!
INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:current_assets:belf00', 'belf00', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:current_assets:belf01', 'belf01', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:current_assets:belf02', 'belf02', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:current_assets:binb00', 'binb00', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:current_assets:cash', 'cash', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:current_assets:payp00', 'payp00', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:current_assets:unib00', 'unib00', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:current_assets:whsi00', 'whsi00', 1, current_date, current_date);

-- CFD's
--- softs
INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):CCZ3.cfd', 'CCZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):CCZ3.cfd', 'CCZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):CTZ3.cfd', 'CTZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):KCZ3.cfd', 'KCZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):LCCU3.cfd', 'LCCU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):LEV3.cfd', 'LEV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):LRCU3.cfd', 'LRCU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):LRCX3.cfd', 'LRCX3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):LSUV3.cfd', 'LSUV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):LWBX3.cfd', 'LWBX3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):OJU3.cfd', 'OJU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):OJX3.cfd', 'OJX3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):ZVU3.cfd', 'ZVU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):ZCZ3.cfd', 'ZCZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):ZLU3.cfd', 'ZLU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):ZLV3.cfd', 'ZLV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):ZSU3.cfd', 'ZSU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):ZSX3.cfd', 'ZSX3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):ZWU3.cfd', 'ZWU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (long):ZWZ3.cfd', 'ZWZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):CCZ3.cfd', 'CCZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):CCZ3.cfd', 'CCZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):CTZ3.cfd', 'CTZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):KCZ3.cfd', 'KCZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):LCCU3.cfd', 'LCCU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):LEV3.cfd', 'LEV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):LRCU3.cfd', 'LRCU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):LRCX3.cfd', 'LRCX3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):LSUV3.cfd', 'LSUV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):LWBX3.cfd', 'LWBX3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):OJU3.cfd', 'OJU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):OJX3.cfd', 'OJX3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):ZVU3.cfd', 'ZVU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):ZCZ3.cfd', 'ZCZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):ZLU3.cfd', 'ZLU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):ZLV3.cfd', 'ZLV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):ZSU3.cfd', 'ZSU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):ZSX3.cfd', 'ZSX3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):ZWU3.cfd', 'ZWU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:softs (short):ZWZ3.cfd', 'ZWZ3.cfd', 1, current_date, current_date);

--- oil
INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (long):.BRENT.cfd', '.BRENT.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (long):.WTI.cfd', '.WTI.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (long):CLV3.cfd', 'CLV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (long):CLX3.cfd', 'CLX3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (long):HOU3.cfd', 'HOU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (long):HOV3.cfd', 'HOV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (long):LCOV3.cfd', 'LCOV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (long):LCOX3.cfd', 'LCOX3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (long):LGOU3.cfd', 'LGOU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (long):LGOV3.cfd', 'LGOV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (short):.BRENT.cfd', '.BRENT.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (short):.WTI.cfd', '.WTI.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (short):CLV3.cfd', 'CLV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (short):CLX3.cfd', 'CLX3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (short):HOU3.cfd', 'HOU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (short):HOV3.cfd', 'HOV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (short):LCOV3.cfd', 'LCOV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (short):LCOX3.cfd', 'LCOX3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (short):LGOU3.cfd', 'LGOU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:oil (short):LGOV3.cfd', 'LGOV3.cfd', 1, current_date, current_date);

--- metals
INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):.GOLD.cfd', '.GOLD.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):.MGOLD.cfd', '.MGOLD.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):.MSILVER.cfd', '.MSILVER.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):.SILVER.cfd', '.SILVER.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):GCZ3.cfd', 'GCZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):HGU3.cfd', 'HGU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):HGZ3.cfd', 'HGZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):MINISIU3.cfd', 'MINISIU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):MINISIZ3.cfd', 'MINISIZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):PAU3.cfd', 'PAU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):PAZ3.cfd', 'PAZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):PLV3.cfd', 'PLV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):SIU3.cfd', 'SIU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (long):SIV3.cfd', 'SIV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):.GOLD.cfd', '.GOLD.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):.MGOLD.cfd', '.MGOLD.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):.MSILVER.cfd', '.MSILVER.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):.SILVER.cfd', '.SILVER.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):GCZ3.cfd', 'GCZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):HGU3.cfd', 'HGU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):HGZ3.cfd', 'HGZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):MINISIU3.cfd', 'MINISIU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):MINISIZ3.cfd', 'MINISIZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):PAU3.cfd', 'PAU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):PAZ3.cfd', 'PAZ3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):PLV3.cfd', 'PLV3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):SIU3.cfd', 'SIU3.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:metals (short):SIV3.cfd', 'SIV3.cfd', 1, current_date, current_date);

--- indices
INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:indices (long):.DE30.cfd', '.DE30.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:indices (long):.ES35.cfd', '.ES35.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:indices (long):.F40.cfd', '.F40.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:indices (long):.N25.cfd', '.N25.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:indices (short):.DE30.cfd', '.DE30.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:indices (short):.ES35.cfd', '.ES35.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:indices (short):.F40.cfd', '.F40.cfd', 1, current_date, current_date);

INSERT INTO T_ACCOUNT(name, description, active, date_created, date_modified)
values('assets:cfd:indices (short):.N25.cfd', '.N25.cfd', 1, current_date, current_date);

-- Add specific parameters
INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(1, 'pool_margin', '0.25', 'margin to leave of pool');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(2, 'risk', '2.0', 'percent risk of pool we are willing to take initially');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(3, 'default_currency_from', '2', 'default index for the currency_from combobox');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(4, 'default_currency_to', '1', 'default index for the currency_to combobox');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(5, 'default_exchange_rate', '1.0', 'default exchange rate');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(6, 'default_account_from', '19', 'default index for the account_from combobox');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(7, 'default_account_to', '39', 'default index for the account_to combobox');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(8, 'importdir', 'import', 'default index for the account_to combobox');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(9, 'exportdir', 'export', 'default index for the account_to combobox');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(10, 'log', '0', '1 = use logging, 0 = don''t use logging');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(11, 'log_file', 'log/lisa.log', 'log file');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(12, 'default_commission', '3.0', 'default commission');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(13, 'default_tax', '0.0', 'default tax');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(14, 'default_market', '16', 'default market');

INSERT INTO T_PARAMETER(parameter_id, name, value, description)
values(15, 'default_commodity', '1', 'default commodity, depends on the market so 1 is recommended here');
