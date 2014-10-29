<html>
<head>
    <title>area91</title>
    <link rel="icon" href="/favicon.png" type="image/png">
    <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
    <link rel="copyright" href="copyright.html">
    <link rel="icon" type="image/png" href="img/favicon.png" />
    <meta name="description" content="">
    <meta name="generator" content="">
    <meta name="author" content="Andy Nagels">
    <meta name="copyright" content="&copy; 2014&ndash;eternity Andy Nagels">
</head>
<body>
    <div class="header">
    <h1 class="headerTitle">
        <a href="/"><img src="/img/biohazard.png" type="image/png" alt="area91" height="50px" width="50px"/></a><span class="headerSubtitle">...</span><ifLoggedIn>&nbsp;<span class="small">You're logged in as <span class="green"><loggedInUser/></span></span></ifLoggedIn>
    </h1>
    </div>
    <div id="side-bar">
    <ul id="menu_sub">
        <li>
            <a href="/" title="home">home</a>
        </li>
        <ifLoggedIn>
        <li>
            <a href="/trading" title="trading">trading</a>
        </li>
        <li>
            <a href="/reports" title="reports">reports</a>
        </li>
        <li>
            <a href="/links" title="links">links</a>
        </li>
        <li>
            <a href="/new_user" title="new_user">new_user</a>
        </li>
        <li>
            <a href="/logout" title="logout">logout</a>
        </li>
        </ifLoggedIn>
    </ul>
    </div>
    <div id="main">
      <apply-content/>
    </div>
</body>
</html>
