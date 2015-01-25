BEGIN;

/* V_COMMODITY_INFO */
--DROP VIEW "1".V_COMMODITY_INFO;
CREATE VIEW "1".V_COMMODITY_INFO
AS
select
    c.commodity_id
    , c.name as commodity_name
    , c.description as commodity_description
    , ct.name as commodity_type
    , ct.description as commodity_type_description
    , cfd.name as cfd_name
    , m.market_id
    , m.name as market
    , m.code as market_code
    , m.country as market_country
from
    t_commodity c
    inner join t_commodity_type ct on c.commodity_type_id = ct.commodity_type_id
    left join t_cfd_general cfd on c.cfd_general_id = cfd.cfd_general_id
    inner join t_market m on cfd.market_id = m.market_id
where c.active = 1
;

COMMIT;
