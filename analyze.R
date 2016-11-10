# Analyze exit poll for 2016 general presidential election

# Questions:
# What characterizes Trump supporters?
# What motivates Trump supporters?


library(io);


cconds <- qread("ctable-prop_class-conditionals.rds");
fconds <- qread("ctable-prop_feature-conditionals.rds");

# Are Trump supporters predominately white?
cconds["race"]
# According to the 2010 census, the general US population consists 
# of 72.4% white Americans.
# (http://www.census.gov/prod/cen2010/briefs/c2010br-02.pdf)

# Are Trump supporters predominately male?
cconds["gender"]

# Are women more likely to vote for Clinton?
fconds["gender"]

# Are Trump supporters predominately 'uneducated'?
cconds["education"]

# Are Trump female white supporters predominately 'uneducated'?
cconds["education among whites by sex"]

# Are Trump supporters predominately Republicans?
cconds["party id"]
cconds["party by gender"]

# Are Trump supporters predominately poor people?
cconds["income"]

# Is midddle class more likely to vote for Trump?
fconds["income"]

# Are Trump supporters mostly married?
cconds["marital status"]

# Are Trump supporters mostly conservatives?
cconds["ideology"]

# Are Trump supporters predominately religious?
cconds["religion"]
cconds["how often do you attend religious services?"]

# Are Trump supporters predominately Evangelical Christians?
cconds["white born-again or evangelical christian?"]

# Do Trump supporters predominately live in rural areas?
cconds["area type"]

# Which issues do Trump supporters care about?
cconds["most important issue facing the country"]

# Which candidate qualities matter the most to Trump supporters?
cconds["which candidate quality mattered most?"]

# Do Trump supporters want the wall?
cconds["view of u.s. wall along the entire mexican border"]

# What do Trump supporters think of international trade?
cconds["effect of international trade"]

# What do Trump supporters think of the ongoing war?
cconds["how is the fight against isis going?"]

# What do Trump supporters think of Obamacare?
cconds["view on obamacare"]

# What do Trump supporters think of Obama?
cconds["opinion of barack obama as president"]

# What do Trump supporters think of Hillary?
cconds["opinion of hillary clinton"]

# What do Trump supporters think of Trump?
cconds["opinion of donald trump"]

# Does Trump's treatment of women bother Trump supporters?
cconds["does donald trump's treatment of women bother you:"]

# Do Trump supporters believe Trump is qualified to be president?
cconds["is trump qualified to serve as president?"]

cconds["in your vote, were supreme court appointments:"]

cconds["does the country's criminal justice system:"]

cconds["feelings about the federal government"]

cconds["opinion of government"]

cconds["is hillary clinton honest and trustworthy?"]

cconds["is donald trump honest and trustworthy?"]

cconds["is clinton qualified to serve as president?"]

cconds["who is qualified to serve as president?"]

cconds["which candidate has the right temperament?"]

cconds["which candidate is honest?"]

cconds["who would better handle the economy?"]

cconds["who would better handle foreign policy?"]

cconds["who would be a better commander in chief?"]

cconds["opinion of the democratic party"]

cconds["opinion of the republican party"]

cconds["condition of national economy"]

cconds["financial situation compared to four years ago:"]

cconds["direction of the country"]

cconds["life for the next generation of americans will be:"]

cconds["importance of debates to your vote"]

cconds["were debates a factor in your vote?"]

