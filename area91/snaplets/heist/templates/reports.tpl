<apply template="base">

<ifLoggedIn>
    <!-- TODO: make this more dynamic -->
    <h1>Reports - 2014</h1>
    <h2>Income vs expenses</h2>
    <p><img src="/img/report_income_vs_expenses.png" type="image/png" alt="Income vs expenses" height="200px" width="200px"/><p>
</ifLoggedIn>

<ifLoggedOut>
    <apply template="_login"/>
</ifLoggedOut>

</apply>
