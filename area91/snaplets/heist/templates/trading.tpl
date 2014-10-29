<apply template="base">

<ifLoggedIn>
    <h1>Trading</h1>
    <!-- Note: also add the flow-chart from dia. -->
    <h2>Step 1: ...</h2>
    <p>TBD<p>
    <h2>Step 2</h2>
    <p>TBD<p>
    <h2>Step 2</h2>
    <p>TBD<p>
</ifLoggedIn>

<ifLoggedOut>
    <apply template="_login"/>
</ifLoggedOut>

</apply>
