<apply template="base">

<ifLoggedIn>
    <h1>WHSI</h1>
    <p>
        <a href="http://www.whselfinvest.com/nl/expiring_markets.php">Expiring markets</a><br>
        <a href="http://www.whselfinvest.com/nl/CFD_Market_Information_Sheets.php?sheet=3">Commodity info</a>
    </p>
    <h1>Finance</h1>
    <p><a href="https://www.homebank.recordbank.be/cwsoft/policyenforcer/pages/loginB2C.jsf?language=nl">Record@Home</a></p>
    <h1>FOD</h1>
    <p><a href="http://www.myminfin.be">Taxes and mandates - minfin</a></p>
</ifLoggedIn>

<ifLoggedOut>
    <apply template="_login"/>
</ifLoggedOut>

</apply>
