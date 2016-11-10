%\VignetteEngine{knitr::knitr}

# Analysis of exit poll for 2016 general presidential election

**Questions**:
  * What characterizes Trump supporters?
  * What motivates Trump supporters?
  * What do Trump supporters think of the candidates?


```r
library(io);
```

```
## Loading required package: filenamer
```

```r
library(printr);

cconds <- qread("ctable-prop_class-conditionals.rds");
fconds <- qread("ctable-prop_feature-conditionals.rds");
counts <- qread("ctable-counts.rds");
```


```r
# calculate statistical significance of contingency tables,
# considering only the "clinton" and "trump" voter classes
p.values <- unlist(lapply(counts,
	function(ct) {
		x <- ct[, c("trump", "clinton")];
		x <- x[complete.cases(x), ];
		chisq.test(x)$p.value
	}
));

fdr <- 0.01;

# adjust for multiple hypothesis testing
q.values <- p.adjust(p.values, method="BY");
which(q.values >= fdr)
```

```
## were debates a factor in your vote? 
##                                  88
```

```r
# number f expected false positives
sum(q.values < fdr) * fdr
```

```
## [1] 0.89
```
All of the contingency values are statistically significant at the nominal
false discovery rate of 0.1, with the exception of the survey question, "Were
debates a factor in your vote?" At this false disovery rate level, less than
one false positive result is expected. This result indicates that the trend
observed in any of the remaining contingency tables is unlikely to be due
purely to chance.  Since the large sample size created tight bounds on the
uncertainty of the proportions reported in the contingency tables, any
differences in proportions can be trivially considered statistically
significant. Therefore, the primary objective in the analysis is to report the
magntitude of the proportions or differences in proportions.


# What characterizes Trump supporters?

## Are Trump supporters predominately white?

```r
cconds[["race"]]
```



|                |     white|     black|    latino|     asian| other race|
|:---------------|---------:|---------:|---------:|---------:|----------:|
|clinton         | 0.5408511| 0.2205106| 0.1492766| 0.0542979|  0.0350638|
|trump           | 0.8634827| 0.0204559| 0.0678686| 0.0246165|  0.0235763|
|other/no answer | 0.6872000| 0.0944000| 0.1296000| 0.0472000|  0.0416000|
86% of Trump supporters are white. According to the 2010 census, the general 
US population consists of 72.4% white Americans.
(http://www.census.gov/prod/cen2010/briefs/c2010br-02.pdf)

## Are Trump supporters predominately male?

```r
cconds[["gender"]]
```



|                |      male|    female|
|:---------------|---------:|---------:|
|clinton         | 0.4120659| 0.5879341|
|trump           | 0.5380571| 0.4619429|
|other/no answer | 0.5809367| 0.4190633|
54% of Trump supporters are men.

## Are women more likely to vote for Clinton?

```r
fconds[["gender"]]
```



|       | total| clinton| trump| other/no answer|
|:------|-----:|-------:|-----:|---------------:|
|male   |  0.48|    0.41|  0.53|            0.06|
|female |  0.52|    0.54|  0.42|            0.04|

```r
gender <- as.matrix(counts[["gender"]][c("female", "male"), c("clinton", "trump")]);
gender
```



|       | clinton| trump|
|:------|-------:|-----:|
|female |    6890|  5359|
|male   |    4829|  6242|

```r
fisher.test(gender)
```

```
## 
## 	Fisher's Exact Test for Count Data
## 
## data:  gender
## p-value < 2.2e-16
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  1.577400 1.750906
## sample estimates:
## odds ratio 
##   1.661886
```
Compared to males, female voters are 66% more likely to vote for Clinton.

## Are Trump supporters predominately 'uneducated'?

```r
cconds[["education"]]
```



|                | high school or less| some college| college graduate| postgraduate|
|:---------------|-------------------:|------------:|----------------:|------------:|
|clinton         |           0.1688610|    0.2867578|        0.3267646|    0.2176166|
|trump           |           0.1958619|    0.3549509|        0.3071373|    0.1420499|
|other/no answer |           0.1402536|    0.3114105|        0.3732171|    0.1751189|
45% of Trump supporters have a college or postgraduate degree.

## Are Trump female white supporters predominately 'uneducated'?

```r
cconds[["education among whites by sex"]]
```



|                | white college-grad women| white non-college women| white college-grad men| white non-college men| non-whites|
|:---------------|------------------------:|-----------------------:|----------------------:|---------------------:|----------:|
|clinton         |                0.2126051|               0.1204451|              0.1381976|             0.0814576|  0.4472947|
|trump           |                0.1912848|               0.2240319|              0.1950966|             0.2601577|  0.1294291|
|other/no answer |                0.1606557|               0.1368852|              0.2393443|             0.1713115|  0.2918033|

```r
trump.white.females.ed <- counts[["education among whites by sex"]][1:2, "trump"];
trump.white.females.ed / sum(trump.white.females.ed)
```

```
## white college-grad women  white non-college women 
##                0.4605757                0.5394243
```
46% of white, female Trump supporters have a college degree.


## Are Trump supporters predominately Republicans?

```r
cconds[["party id"]]
```



|                | democrats| republicans| independents|
|:---------------|---------:|-----------:|------------:|
|clinton         | 0.6823172|   0.0478804|    0.2698024|
|trump           | 0.0695023|   0.6199064|    0.3105912|
|other/no answer | 0.1534570|   0.2048904|    0.6416526|

```r
cconds[["party by gender"]]
```



|                | democratic men| democratic women| republican men| republican women| independent men| independent women|
|:---------------|--------------:|----------------:|--------------:|----------------:|---------------:|-----------------:|
|clinton         |      0.2534566|        0.4309102|      0.0212062|        0.0266350|       0.1308847|         0.1369073|
|trump           |      0.0295330|        0.0388049|      0.3222871|        0.2999657|       0.1826065|         0.1268029|
|other/no answer |      0.0765230|        0.0839525|      0.1240713|        0.0876672|       0.3722140|         0.2555721|

## Are Trump supporters predominately poor people?

```r
cconds[["income"]]
```



|                | under $30,000| $30k-$49,999| $50k-$99,999| $100k-$199,999| $200k-$249,999| $250,000 or more|
|:---------------|-------------:|------------:|------------:|--------------:|--------------:|----------------:|
|clinton         |     0.1841886|    0.1981006|    0.2914862|      0.2305898|      0.0392369|        0.0563979|
|trump           |     0.1488769|    0.1704684|    0.3310987|      0.2461257|      0.0418771|        0.0615532|
|other/no answer |     0.1936483|    0.2525174|    0.2354764|      0.2277304|      0.0224632|        0.0681642|

## Is the midddle class more likely to vote for Trump?

```r
fconds[["income"]]
```



|                 | total| clinton| trump| other/no answer|
|:----------------|-----:|-------:|-----:|---------------:|
|under $30,000    |  0.17|    0.53|  0.41|            0.06|
|$30k-$49,999     |  0.19|    0.51|  0.42|            0.07|
|$50k-$99,999     |  0.31|    0.46|  0.50|            0.04|
|$100k-$199,999   |  0.24|    0.47|  0.48|            0.05|
|$200k-$249,999   |  0.04|    0.48|  0.49|            0.03|
|$250,000 or more |  0.06|    0.46|  0.48|            0.06|

```r
middle.class <- rbind(
	middle_class = counts[["income"]][3, c("trump", "clinton")],
	not_middle_class = colSums(counts[["income"]][-3, c("trump", "clinton")])
);

middle.class
```



|                 | trump| clinton|
|:----------------|-----:|-------:|
|middle_class     |  3803|    3499|
|not_middle_class |  7683|    8505|

```r
fisher.test(middle.class)
```

```
## 
## 	Fisher's Exact Test for Count Data
## 
## data:  middle.class
## p-value = 5.982e-11
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  1.137991 1.272117
## sample estimates:
## odds ratio 
##   1.203152
```
Compared to people in other income brackets, those earning between 
$50,000 and $100,000 per year are 20% more likely to vote for Trump;
33% of Trump supporters are in this income bracket.

## Are Trump supporters mostly married?

```r
cconds[["marital status"]]
```



|                |   married| unmarried|
|:---------------|---------:|---------:|
|clinton         | 0.5191312| 0.4808688|
|trump           | 0.6582301| 0.3417699|
|other/no answer | 0.4410853| 0.5589147|

## Are Trump supporters mostly conservatives?

```r
cconds[["ideology"]]
```



|                |   liberal|  moderate| conservative|
|:---------------|---------:|---------:|------------:|
|clinton         | 0.4610686| 0.4281167|    0.1108148|
|trump           | 0.0553964| 0.3406269|    0.6039767|
|other/no answer | 0.2741589| 0.4795991|    0.2462419|

## Are Trump supporters predominately religious?

```r
cconds[["religion"]]
```



|                | protestant|  catholic|    mormon| other christian|    jewish| muslim| other religion| no religion|
|:---------------|----------:|---------:|---------:|---------------:|---------:|------:|--------------:|-----------:|
|clinton         |  0.2111839| 0.2188523| 0.0052559|       0.2181630| 0.0450629|     NA|      0.0858177|   0.2156643|
|trump           |  0.3312776| 0.2446037| 0.0124177|       0.2699392| 0.0147512|     NA|      0.0472539|   0.0797566|
|other/no answer |  0.2132905| 0.1811361| 0.0364416|       0.1264737| 0.0396570|     NA|      0.1661308|   0.2368703|

```r
cconds[["how often do you attend religious services?"]]
```



|                | weekly or more|   monthly| few times a year|     never|
|:---------------|--------------:|---------:|----------------:|---------:|
|clinton         |      0.2743056| 0.1529472|        0.2892954| 0.2834519|
|trump           |      0.3950854| 0.1676542|        0.2914779| 0.1457825|
|other/no answer |      0.2583732| 0.1562998|        0.2838915| 0.3014354|

## Are Trump supporters predominately Evangelical Christians?

```r
cconds[["white born-again or evangelical christian?"]]
```



|                |       yes|        no|
|:---------------|---------:|---------:|
|clinton         | 0.0870121| 0.9129879|
|trump           | 0.4484943| 0.5515057|
|other/no answer | 0.1492188| 0.8507812|

## Do Trump supporters predominately live in rural areas?

```r
cconds[["area type"]]
```



|                | urban area| suburban area| rural area|
|:---------------|----------:|-------------:|----------:|
|clinton         |  0.4188936|     0.4604255|  0.1206809|
|trump           |  0.2535162|     0.5219656|  0.2245181|
|other/no answer |  0.3947991|     0.4736013|  0.1315997|

# What motivates Trump supporters?

## Which issues do Trump supporters care about?

```r
cconds[["most important issue facing the country"]]
```



|                | foreign policy| immigration|   economy| terrorism|
|:---------------|--------------:|-----------:|---------:|---------:|
|clinton         |      0.1694855|   0.0904100| 0.5875321| 0.1525724|
|trump           |      0.0986005|   0.1855689| 0.4870047| 0.2288259|
|other/no answer |      0.1513471|   0.1014263| 0.6069731| 0.1402536|

## What do Trump supporters think of international trade?

```r
cconds[["effect of international trade"]]
```



|                | creates u.s. jobs| takes away u.s. jobs| does not affect jobs|
|:---------------|-----------------:|--------------------:|--------------------:|
|clinton         |         0.5291458|            0.3073297|            0.1635244|
|trump           |         0.3029150|            0.6218901|            0.0751949|
|other/no answer |         0.4818966|            0.3551724|            0.1629310|

## What do Trump supporters think of the ongoing war?

```r
cconds[["how is the fight against isis going?"]]
```



|                | very well| somewhat well| somewhat badly| very badly|
|:---------------|---------:|-------------:|--------------:|----------:|
|clinton         | 0.1177523|     0.5819842|      0.2392696|  0.0609940|
|trump           | 0.0145487|     0.1903907|      0.3454872|  0.4495734|
|other/no answer | 0.0451761|     0.4058193|      0.3683002|  0.1807044|

## What do Trump supporters think of Obamacare?

```r
cconds[["view on obamacare"]]
```



|                | did not go far enough| was about right| went too far|
|:---------------|---------------------:|---------------:|------------:|
|clinton         |             0.5285833|       0.3334254|    0.1379913|
|trump           |             0.1168533|       0.0389805|    0.8441662|
|other/no answer |             0.2653430|       0.3185921|    0.4160650|

## Do Trump supporters want the wall?

```r
cconds[["view of u.s. wall along the entire mexican border"]]
```



|                |   support|    oppose|
|:---------------|---------:|---------:|
|clinton         | 0.0908270| 0.9091730|
|trump           | 0.7934703| 0.2065297|
|other/no answer | 0.3022556| 0.6977444|
79% of Trump supporters support the construction of the wall.

## How has the financial situation of Trump supporters changed compared to four years ago?

```r
cconds[["financial situation compared to four years ago:"]]
```



|                | better today| worse today| about the same|
|:---------------|------------:|-----------:|--------------:|
|clinton         |    0.4819150|   0.1107982|      0.4072868|
|trump           |    0.1570433|   0.4447122|      0.3982446|
|other/no answer |    0.2324159|   0.1521407|      0.6154434|
44% of Trump supporters are financially worse today compared to four years
ago.

## What do Trump supporters think of the federal government?

```r
cconds[["feelings about the federal government"]]
```



|                | enthusiastic| satisfied| dissatisfied|     angry|
|:---------------|------------:|---------:|------------:|---------:|
|clinton         |    0.0834423| 0.3851251|    0.4428459| 0.0885866|
|trump           |    0.0216814| 0.1042478|    0.4894690| 0.3846018|
|other/no answer |    0.0195618| 0.2300469|    0.5297340| 0.2206573|
87% of Trump supporters are dissatisified or angry with the federal
government.

## What do Trump supporters think of the direction of the country? 

```r
cconds[["direction of the country"]]
```



|                | right direction| wrong track|
|:---------------|---------------:|-----------:|
|clinton         |       0.6570784|   0.3429216|
|trump           |       0.0581427|   0.9418573|
|other/no answer |       0.1506977|   0.8493023|
94% of Trump supporters believe the direction is on the wrong track.


# What do Trump supporters think of the candidates?

## Which candidate qualities matter the most to Trump supporters?

```r
cconds[["which candidate quality mattered most?"]]
```



|                | cares about me| can bring change| right experience| good judgment|
|:---------------|--------------:|----------------:|----------------:|-------------:|
|clinton         |      0.1880726|        0.1180409|        0.4085624|     0.2853242|
|trump           |      0.1179703|        0.7274226|        0.0377358|     0.1168712|
|other/no answer |      0.2478386|        0.2756964|        0.0989433|     0.3775216|
73% of Trump supporters view the ability to bring change as the most important
qualitity in the presidential candidate.


## What do Trump supporters think of Obama?

```r
cconds[["opinion of barack obama as president"]]
```



|                | strongly approve| somewhat approve| somewhat disapprove| strongly disapprove|
|:---------------|----------------:|----------------:|-------------------:|-------------------:|
|clinton         |        0.6531926|        0.2905939|           0.0353587|           0.0208548|
|trump           |        0.0287158|        0.0912878|           0.1982629|           0.6817336|
|other/no answer |        0.1460775|        0.4427412|           0.2651037|           0.1460775|

## What do Trump supporters think of Hillary?

```r
cconds[["opinion of hillary clinton"]]
```



|                | favorable| unfavorable|
|:---------------|---------:|-----------:|
|clinton         | 0.8755335|   0.1244665|
|trump           | 0.0289570|   0.9710430|
|other/no answer | 0.1888112|   0.8111888|

## What do Trump supporters think of Trump?

```r
cconds[["opinion of donald trump"]]
```



|                | favorable| unfavorable|
|:---------------|---------:|-----------:|
|clinton         | 0.0318558|   0.9681442|
|trump           | 0.8004699|   0.1995301|
|other/no answer | 0.0731707|   0.9268293|
Although 20% of Trump supporters view Trump unfavorably, they nevertheless
voted for him.

## Do Trump supporters believe Trump is qualified to be president?

```r
cconds[["is trump qualified to serve as president?"]]
```



|                |       yes|        no|
|:---------------|---------:|---------:|
|clinton         | 0.0326763| 0.9673237|
|trump           | 0.7678493| 0.2321507|
|other/no answer | 0.1528348| 0.8471652|
23% of Trump supporters do not believe Trump to be qualified to be president.

## Do Trump supporters believe Trump has the temperament to be president?

```r
cconds[["does trump have the temperament to be president?"]]
```



|                |       yes|        no|
|:---------------|---------:|---------:|
|clinton         | 0.0371139| 0.9628861|
|trump           | 0.7230631| 0.2769369|
|other/no answer | 0.0650038| 0.9349962|
28% of Trump suppporters do not believe Trump has the temperatment to be
president.

## Which candidates do Trump supporter believe has the qualification or temperament to be president?

```r
cconds[["who is qualified to serve as president?"]]
```



|                |  both are| only clinton is| only trump is| neither is|
|:---------------|---------:|---------------:|-------------:|----------:|
|clinton         | 0.0237613|       0.9237877|     0.0071284|  0.0453225|
|trump           | 0.0748845|       0.0197019|     0.6988057|  0.2066080|
|other/no answer | 0.0808581|       0.4653465|            NA|  0.4537954|

```r
cconds[["which candidate has the right temperament?"]]
```



|                |   both do| only clinton does| only trump does| neither does|
|:---------------|---------:|-----------------:|---------------:|------------:|
|clinton         | 0.0240240|         0.9284427|       0.0121836|    0.0353496|
|trump           | 0.1016417|         0.0539159|       0.6256392|    0.2188033|
|other/no answer | 0.0474277|         0.4831190|              NA|    0.4694534|
21% of Trump supporters believe neither candidate is qualified.
22% of Trump supporters believe neither candidate has the right temperament.

## Which candidate do Trump supporters believe to be honest?

```r
cconds[["which candidate is honest?"]]
```



|                | both are| only clinton is| only trump is| neither is|
|:---------------|--------:|---------------:|-------------:|----------:|
|clinton         |       NA|       0.7326869|     0.0069527|  0.2603604|
|trump           |       NA|       0.0077288|     0.6941056|  0.2981656|
|other/no answer |       NA|       0.1794688|     0.0545585|  0.7659727|

```r
honest <- rowSums(counts[["which candidate is honest?"]], na.rm=TRUE);
honest["neither is"] / sum(honest)
```

```
## neither is 
##  0.3085028
```
About 30% of voters believe neither candidate is honest.

## Does Trump's treatment of women bother Trump supporters?

```r
cconds[["does donald trump's treatment of women bother you:"]]
```



|                |     a lot|      some|  not much| not at all|
|:---------------|---------:|---------:|---------:|----------:|
|clinton         | 0.8656691| 0.0792382| 0.0216800|  0.0334127|
|trump           | 0.1198898| 0.3270530| 0.2494668|  0.3035905|
|other/no answer | 0.5768025| 0.2304075| 0.1003135|  0.0924765|


# Other questions


```r
cconds[["in your vote, were supreme court appointments:"]]
```



|                | the most important factor| an important factor| a minor factor| not a factor at all|
|:---------------|-------------------------:|-------------------:|--------------:|-------------------:|
|clinton         |                 0.1857909|           0.4970544|      0.1510595|           0.1660951|
|trump           |                 0.2583706|           0.4956132|      0.1260519|           0.1199642|
|other/no answer |                 0.1231136|           0.4678316|      0.2454329|           0.1636219|

```r
cconds[["does the country's criminal justice system:"]]
```



|                | treat all fairly| treat blacks unfairly|
|:---------------|----------------:|---------------------:|
|clinton         |        0.2249513|             0.7750487|
|trump           |        0.7508414|             0.2491586|
|other/no answer |        0.2778265|             0.7221735|

```r
cconds[["opinion of government"]]
```



|                | government should do more| government doing too much|
|:---------------|-------------------------:|-------------------------:|
|clinton         |                 0.7603052|                 0.2396948|
|trump           |                 0.2133509|                 0.7866491|
|other/no answer |                 0.3752122|                 0.6247878|

```r
cconds[["is hillary clinton honest and trustworthy?"]]
```



|                |       yes|        no|
|:---------------|---------:|---------:|
|clinton         | 0.7349739| 0.2650261|
|trump           | 0.0312943| 0.9687057|
|other/no answer | 0.1444898| 0.8555102|

```r
cconds[["is donald trump honest and trustworthy?"]]
```



|                |       yes|        no|
|:---------------|---------:|---------:|
|clinton         | 0.0355888| 0.9644112|
|trump           | 0.7010224| 0.2989776|
|other/no answer | 0.0614568| 0.9385432|

```r
cconds[["is clinton qualified to serve as president?"]]
```



|                |       yes|        no|
|:---------------|---------:|---------:|
|clinton         | 0.9500433| 0.0499567|
|trump           | 0.0995923| 0.9004077|
|other/no answer | 0.5251029| 0.4748971|

```r
cconds[["who would better handle the economy?"]]
```



|                |   clinton|     trump|
|:---------------|---------:|---------:|
|clinton         | 0.9677592| 0.0322408|
|trump           | 0.0098993| 0.9901007|
|other/no answer | 0.4842857| 0.5157143|

```r
cconds[["who would better handle foreign policy?"]]
```



|                |   clinton|     trump|
|:---------------|---------:|---------:|
|clinton         | 0.9811338| 0.0188662|
|trump           | 0.0810198| 0.9189802|
|other/no answer | 0.8088768| 0.1911232|

```r
cconds[["who would be a better commander in chief?"]]
```



|                |   clinton|     trump|
|:---------------|---------:|---------:|
|clinton         | 0.9901007| 0.0098993|
|trump           | 0.0216685| 0.9783315|
|other/no answer | 0.5865854| 0.4134146|

```r
cconds[["opinion of the democratic party"]]
```



|                | favorable| unfavorable|
|:---------------|---------:|-----------:|
|clinton         | 0.9036509|   0.0963491|
|trump           | 0.0828323|   0.9171677|
|other/no answer | 0.3900169|   0.6099831|

```r
cconds[["opinion of the republican party"]]
```



|                | favorable| unfavorable|
|:---------------|---------:|-----------:|
|clinton         | 0.0988016|   0.9011984|
|trump           | 0.7577199|   0.2422801|
|other/no answer | 0.2372881|   0.7627119|

```r
cconds[["condition of national economy"]]
```



|                | excellent|      good|  not good|      poor|
|:---------------|---------:|---------:|---------:|---------:|
|clinton         | 0.0533112| 0.5369514| 0.3422912| 0.0674461|
|trump           | 0.0104796| 0.1365897| 0.4913854| 0.3615453|
|other/no answer | 0.0052830| 0.3056604| 0.4558491| 0.2332075|

```r
cconds[["life for the next generation of americans will be:"]]
```



|                | better than today| worse than today| about the same|
|:---------------|-----------------:|----------------:|--------------:|
|clinton         |         0.4816147|        0.2324912|      0.2858941|
|trump           |         0.3135508|        0.4776879|      0.2087612|
|other/no answer |         0.2295359|        0.4227848|      0.3476793|

```r
cconds[["importance of debates to your vote"]]
```



|                | most important factor| an important factor| a minor factor| not a factor|
|:---------------|---------------------:|-------------------:|--------------:|------------:|
|clinton         |             0.3019954|           0.4326682|      0.1600928|    0.1052436|
|trump           |             0.2616700|           0.3742256|      0.2440450|    0.1200593|
|other/no answer |             0.1542169|           0.4493976|      0.1686747|    0.2277108|

```r
# differences between Clinton and Trump supporters in this contingency table 
# are not considered statistically significant
cconds[["were debates a factor in your vote?"]]
```



|                |       yes|        no|
|:---------------|---------:|---------:|
|clinton         | 0.8929178| 0.1070822|
|trump           | 0.8796782| 0.1203218|
|other/no answer | 0.7616646| 0.2383354|

