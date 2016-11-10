%\VignetteEngine{knitr::knitr}

# Analyze exit poll for 2016 general presidential election

*Questions*:
  * What characterizes Trump supporters?
  * What motivates Trump supporters?


```r
library(io);
```

```
## Loading required package: filenamer
```

```r
cconds <- qread("ctable-prop_class-conditionals.rds");
fconds <- qread("ctable-prop_feature-conditionals.rds");
counts <- qread("ctable-counts.rds");
```

## Are Trump supporters predominately white?

```r
cconds["race"]
```

```
## $race
##                     white      black    latino      asian other race
## clinton         0.5408511 0.22051064 0.1492766 0.05429787 0.03506383
## trump           0.8634827 0.02045592 0.0678686 0.02461645 0.02357632
## other/no answer 0.6872000 0.09440000 0.1296000 0.04720000 0.04160000
```
86% of Trump supporters are white. According to the 2010 census, the general 
US population consists of 72.4% white Americans.
(http://www.census.gov/prod/cen2010/briefs/c2010br-02.pdf)

## Are Trump supporters predominately male?

```r
cconds["gender"]
```

```
## $gender
##                      male    female
## clinton         0.4120659 0.5879341
## trump           0.5380571 0.4619429
## other/no answer 0.5809367 0.4190633
```
53.8% of Trump supporteres are men.


## Are women more likely to vote for Clinton?

```r
fconds["gender"]
```

```
## $gender
##        total clinton trump other/no answer
## male    0.48    0.41  0.53            0.06
## female  0.52    0.54  0.42            0.04
```

```r
counts["gender"]
```

```
## $gender
##        clinton trump other/no answer
## male      4829  6242             707
## female    6890  5359             510
```

```r
fisher.test(as.matrix(counts[["gender"]][, c("trump", "clinton")]))
```

```
## 
## 	Fisher's Exact Test for Count Data
## 
## data:  as.matrix(counts[["gender"]][, c("trump", "clinton")])
## p-value < 2.2e-16
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  1.577400 1.750906
## sample estimates:
## odds ratio 
##   1.661886
```
Female voters are 66% more likely to vote for Trump.


## Are Trump supporters predominately 'uneducated'?

```r
cconds["education"]
```

```
## $education
##                 high school or less some college college graduate
## clinton                   0.1688610    0.2867578        0.3267646
## trump                     0.1958619    0.3549509        0.3071373
## other/no answer           0.1402536    0.3114105        0.3732171
##                 postgraduate
## clinton            0.2176166
## trump              0.1420499
## other/no answer    0.1751189
```
45% of Trump supporters have a college or postgraduate degree.

## Are Trump female white supporters predominately 'uneducated'?

```r
cconds["education among whites by sex"]
```

```
## $`education among whites by sex`
##                 white college-grad women white non-college women
## clinton                        0.2126051               0.1204451
## trump                          0.1912848               0.2240319
## other/no answer                0.1606557               0.1368852
##                 white college-grad men white non-college men non-whites
## clinton                      0.1381976            0.08145757  0.4472947
## trump                        0.1950966            0.26015767  0.1294291
## other/no answer              0.2393443            0.17131148  0.2918033
```

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
cconds["party id"]
```

```
## $`party id`
##                  democrats republicans independents
## clinton         0.68231718  0.04788043    0.2698024
## trump           0.06950234  0.61990642    0.3105912
## other/no answer 0.15345700  0.20489039    0.6416526
```

```r
cconds["party by gender"]
```

```
## $`party by gender`
##                 democratic men democratic women republican men
## clinton             0.25345661       0.43091017     0.02120621
## trump               0.02953297       0.03880495     0.32228709
## other/no answer     0.07652303       0.08395245     0.12407132
##                 republican women independent men independent women
## clinton               0.02663500       0.1308847         0.1369073
## trump                 0.29996566       0.1826065         0.1268029
## other/no answer       0.08766716       0.3722140         0.2555721
```

## Are Trump supporters predominately poor people?

```r
cconds["income"]
```

```
## $income
##                 under $30,000 $30k-$49,999 $50k-$99,999 $100k-$199,999
## clinton             0.1841886    0.1981006    0.2914862      0.2305898
## trump               0.1488769    0.1704684    0.3310987      0.2461257
## other/no answer     0.1936483    0.2525174    0.2354764      0.2277304
##                 $200k-$249,999 $250,000 or more
## clinton             0.03923692       0.05639787
## trump               0.04187707       0.06155320
## other/no answer     0.02246321       0.06816421
```

## Is midddle class more likely to vote for Trump?

```r
fconds["income"]
```

```
## $income
##                  total clinton trump other/no answer
## under $30,000     0.17    0.53  0.41            0.06
## $30k-$49,999      0.19    0.51  0.42            0.07
## $50k-$99,999      0.31    0.46  0.50            0.04
## $100k-$199,999    0.24    0.47  0.48            0.05
## $200k-$249,999    0.04    0.48  0.49            0.03
## $250,000 or more  0.06    0.46  0.48            0.06
```

```r
middle.class <- rbind(
	middle_class = counts[["income"]][3, c("trump", "clinton")],
	not_middle_class = colSums(counts[["income"]][-3, c("trump", "clinton")])
);

middle.class
```

```
##                  trump clinton
## middle_class      3803    3499
## not_middle_class  7683    8505
```

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
Those earning between \$50,000 and \$100,000 per year are 20% more likely to
vote for Trump.


## Are Trump supporters mostly married?

```r
cconds["marital status"]
```

```
## $`marital status`
##                   married unmarried
## clinton         0.5191312 0.4808688
## trump           0.6582301 0.3417699
## other/no answer 0.4410853 0.5589147
```

## Are Trump supporters mostly conservatives?

```r
cconds["ideology"]
```

```
## $ideology
##                    liberal  moderate conservative
## clinton         0.46106857 0.4281167    0.1108148
## trump           0.05539637 0.3406269    0.6039767
## other/no answer 0.27415891 0.4795991    0.2462419
```

## Are Trump supporters predominately religious?

```r
cconds["religion"]
```

```
## $religion
##                 protestant  catholic      mormon other christian
## clinton          0.2111839 0.2188523 0.005255902       0.2181630
## trump            0.3312776 0.2446037 0.012417701       0.2699392
## other/no answer  0.2132905 0.1811361 0.036441586       0.1264737
##                     jewish muslim other religion no religion
## clinton         0.04506290     NA     0.08581768  0.21566431
## trump           0.01475123     NA     0.04725394  0.07975665
## other/no answer 0.03965702     NA     0.16613076  0.23687031
```

```r
cconds["how often do you attend religious services?"]
```

```
## $`how often do you attend religious services?`
##                 weekly or more   monthly few times a year     never
## clinton              0.2743056 0.1529472        0.2892954 0.2834519
## trump                0.3950854 0.1676542        0.2914779 0.1457825
## other/no answer      0.2583732 0.1562998        0.2838915 0.3014354
```

## Are Trump supporters predominately Evangelical Christians?

```r
cconds["white born-again or evangelical christian?"]
```

```
## $`white born-again or evangelical christian?`
##                       yes        no
## clinton         0.0870121 0.9129879
## trump           0.4484943 0.5515057
## other/no answer 0.1492188 0.8507813
```

## Do Trump supporters predominately live in rural areas?

```r
cconds["area type"]
```

```
## $`area type`
##                 urban area suburban area rural area
## clinton          0.4188936     0.4604255  0.1206809
## trump            0.2535162     0.5219656  0.2245181
## other/no answer  0.3947991     0.4736013  0.1315997
```


## Which issues do Trump supporters care about?

```r
cconds["most important issue facing the country"]
```

```
## $`most important issue facing the country`
##                 foreign policy immigration   economy terrorism
## clinton             0.16948552  0.09040999 0.5875321 0.1525724
## trump               0.09860051  0.18556888 0.4870047 0.2288259
## other/no answer     0.15134707  0.10142631 0.6069731 0.1402536
```

## What do Trump supporters think of international trade?

```r
cconds["effect of international trade"]
```

```
## $`effect of international trade`
##                 creates u.s. jobs takes away u.s. jobs
## clinton                 0.5291458            0.3073297
## trump                   0.3029150            0.6218901
## other/no answer         0.4818966            0.3551724
##                 does not affect jobs
## clinton                   0.16352443
## trump                     0.07519495
## other/no answer           0.16293103
```

## What do Trump supporters think of the ongoing war?

```r
cconds["how is the fight against isis going?"]
```

```
## $`how is the fight against isis going?`
##                  very well somewhat well somewhat badly very badly
## clinton         0.11775226     0.5819842      0.2392696 0.06099398
## trump           0.01454872     0.1903907      0.3454872 0.44957342
## other/no answer 0.04517611     0.4058193      0.3683002 0.18070444
```

## What do Trump supporters think of Obamacare?

```r
cconds["view on obamacare"]
```

```
## $`view on obamacare`
##                 did not go far enough was about right went too far
## clinton                     0.5285833      0.33342539    0.1379913
## trump                       0.1168533      0.03898051    0.8441662
## other/no answer             0.2653430      0.31859206    0.4160650
```

## Do Trump supporters want the wall?

```r
cconds["view of u.s. wall along the entire mexican border"]
```

```
## $`view of u.s. wall along the entire mexican border`
##                    support    oppose
## clinton         0.09082701 0.9091730
## trump           0.79347029 0.2065297
## other/no answer 0.30225564 0.6977444
```
79% of Trump supporters support the construction of the wall.

## How has the financial situation of Trump supporters changed compared to
## four years ago?

```r
cconds["financial situation compared to four years ago:"]
```

```
## $`financial situation compared to four years ago:`
##                 better today worse today about the same
## clinton            0.4819150   0.1107982      0.4072868
## trump              0.1570433   0.4447122      0.3982446
## other/no answer    0.2324159   0.1521407      0.6154434
```
44% of Trump supporters are financially worse today compared to four years
ago.


## Which candidate qualities matter the most to Trump supporters?

```r
cconds["which candidate quality mattered most?"]
```

```
## $`which candidate quality mattered most?`
##                 cares about me can bring change right experience
## clinton              0.1880726        0.1180409       0.40856237
## trump                0.1179703        0.7274226       0.03773585
## other/no answer      0.2478386        0.2756964       0.09894332
##                 good judgment
## clinton             0.2853242
## trump               0.1168712
## other/no answer     0.3775216
```
73% of Trump supporters want change.

## What do Trump supporters think of Obama?

```r
cconds["opinion of barack obama as president"]
```

```
## $`opinion of barack obama as president`
##                 strongly approve somewhat approve somewhat disapprove
## clinton               0.65319258       0.29059389          0.03535874
## trump                 0.02871577       0.09128778          0.19826287
## other/no answer       0.14607755       0.44274121          0.26510370
##                 strongly disapprove
## clinton                  0.02085479
## trump                    0.68173358
## other/no answer          0.14607755
```

## What do Trump supporters think of Hillary?

```r
cconds["opinion of hillary clinton"]
```

```
## $`opinion of hillary clinton`
##                  favorable unfavorable
## clinton         0.87553355   0.1244665
## trump           0.02895701   0.9710430
## other/no answer 0.18881119   0.8111888
```

## What do Trump supporters think of Trump?

```r
cconds["opinion of donald trump"]
```

```
## $`opinion of donald trump`
##                  favorable unfavorable
## clinton         0.03185584   0.9681442
## trump           0.80046991   0.1995301
## other/no answer 0.07317073   0.9268293
```

## Does Trump's treatment of women bother Trump supporters?

```r
cconds["does donald trump's treatment of women bother you:"]
```

```
## $`does donald trump's treatment of women bother you:`
##                     a lot       some   not much not at all
## clinton         0.8656691 0.07923822 0.02167999 0.03341268
## trump           0.1198898 0.32705297 0.24946676 0.30359047
## other/no answer 0.5768025 0.23040752 0.10031348 0.09247649
```

## Do Trump supporters believe Trump is qualified to be president?

```r
cconds["is trump qualified to serve as president?"]
```

```
## $`is trump qualified to serve as president?`
##                       yes        no
## clinton         0.0326763 0.9673237
## trump           0.7678493 0.2321507
## other/no answer 0.1528348 0.8471652
```
23% of Trump supporters do not believe Trump to be qualified to be president.

## Do Trump supporters believe Trump has the temperament to be president?

```r
cconds["does trump have the temperament to be president?"]
```

```
## $`does trump have the temperament to be president?`
##                        yes        no
## clinton         0.03711394 0.9628861
## trump           0.72306314 0.2769369
## other/no answer 0.06500378 0.9349962
```
28% of Trump suppporters do not believe Trump has the temperatment to be
president.

## Which candidates do Trump supporter believe has the qualification or temperament
## to be president?

```r
cconds["who is qualified to serve as president?"]
```

```
## $`who is qualified to serve as president?`
##                   both are only clinton is only trump is neither is
## clinton         0.02376133      0.92378773   0.007128399 0.04532254
## trump           0.07488449      0.01970186   0.698805684 0.20660797
## other/no answer 0.08085809      0.46534653            NA 0.45379538
```

```r
cconds["which candidate has the right temperament?"]
```

```
## $`which candidate has the right temperament?`
##                    both do only clinton does only trump does neither does
## clinton         0.02402402        0.92844273      0.01218361   0.03534964
## trump           0.10164170        0.05391585      0.62563919   0.21880327
## other/no answer 0.04742765        0.48311897              NA   0.46945338
```
21% of Trump supporters believe neither candidate is qualified.
22% of Trump supporters believe neither candidate has the right temperament.


```r
cconds["which candidate is honest?"]
```

```
## $`which candidate is honest?`
##                 both are only clinton is only trump is neither is
## clinton               NA     0.732686854   0.006952703  0.2603604
## trump                 NA     0.007728839   0.694105596  0.2981656
## other/no answer       NA     0.179468772   0.054558507  0.7659727
```

```r
honest <- rowSums(counts[["which candidate is honest?"]], na.rm=TRUE);
honest["neither is"] / sum(honest)
```

```
## neither is 
##  0.3085028
```
About 30% of voters believe neither candidate is honest.



```r
cconds["in your vote, were supreme court appointments:"]
```

```
## $`in your vote, were supreme court appointments:`
##                 the most important factor an important factor
## clinton                         0.1857909           0.4970544
## trump                           0.2583706           0.4956132
## other/no answer                 0.1231136           0.4678316
##                 a minor factor not a factor at all
## clinton              0.1510595           0.1660951
## trump                0.1260519           0.1199642
## other/no answer      0.2454329           0.1636219
```

```r
cconds["does the country's criminal justice system:"]
```

```
## $`does the country's criminal justice system:`
##                 treat all fairly treat blacks unfairly
## clinton                0.2249513             0.7750487
## trump                  0.7508414             0.2491586
## other/no answer        0.2778265             0.7221735
```

```r
cconds["feelings about the federal government"]
```

```
## $`feelings about the federal government`
##                 enthusiastic satisfied dissatisfied      angry
## clinton           0.08344232 0.3851251    0.4428459 0.08858662
## trump             0.02168142 0.1042478    0.4894690 0.38460177
## other/no answer   0.01956182 0.2300469    0.5297340 0.22065728
```

```r
cconds["opinion of government"]
```

```
## $`opinion of government`
##                 government should do more government doing too much
## clinton                         0.7603052                 0.2396948
## trump                           0.2133509                 0.7866491
## other/no answer                 0.3752122                 0.6247878
```

```r
cconds["is hillary clinton honest and trustworthy?"]
```

```
## $`is hillary clinton honest and trustworthy?`
##                        yes        no
## clinton         0.73497389 0.2650261
## trump           0.03129433 0.9687057
## other/no answer 0.14448980 0.8555102
```

```r
cconds["is donald trump honest and trustworthy?"]
```

```
## $`is donald trump honest and trustworthy?`
##                        yes        no
## clinton         0.03558875 0.9644112
## trump           0.70102238 0.2989776
## other/no answer 0.06145675 0.9385432
```

```r
cconds["is clinton qualified to serve as president?"]
```

```
## $`is clinton qualified to serve as president?`
##                        yes         no
## clinton         0.95004329 0.04995671
## trump           0.09959226 0.90040774
## other/no answer 0.52510288 0.47489712
```

```r
cconds["who would better handle the economy?"]
```

```
## $`who would better handle the economy?`
##                     clinton      trump
## clinton         0.967759221 0.03224078
## trump           0.009899255 0.99010074
## other/no answer 0.484285714 0.51571429
```

```r
cconds["who would better handle foreign policy?"]
```

```
## $`who would better handle foreign policy?`
##                    clinton      trump
## clinton         0.98113376 0.01886624
## trump           0.08101978 0.91898022
## other/no answer 0.80887681 0.19112319
```

```r
cconds["who would be a better commander in chief?"]
```

```
## $`who would be a better commander in chief?`
##                    clinton       trump
## clinton         0.99010074 0.009899255
## trump           0.02166847 0.978331528
## other/no answer 0.58658537 0.413414634
```

```r
cconds["opinion of the democratic party"]
```

```
## $`opinion of the democratic party`
##                  favorable unfavorable
## clinton         0.90365093  0.09634907
## trump           0.08283227  0.91716773
## other/no answer 0.39001692  0.60998308
```

```r
cconds["opinion of the republican party"]
```

```
## $`opinion of the republican party`
##                  favorable unfavorable
## clinton         0.09880157   0.9011984
## trump           0.75771993   0.2422801
## other/no answer 0.23728814   0.7627119
```

```r
cconds["condition of national economy"]
```

```
## $`condition of national economy`
##                   excellent      good  not good       poor
## clinton         0.053311229 0.5369514 0.3422912 0.06744612
## trump           0.010479574 0.1365897 0.4913854 0.36154529
## other/no answer 0.005283019 0.3056604 0.4558491 0.23320755
```

```r
cconds["direction of the country"]
```

```
## $`direction of the country`
##                 right direction wrong track
## clinton              0.65707845   0.3429216
## trump                0.05814266   0.9418573
## other/no answer      0.15069767   0.8493023
```

```r
cconds["life for the next generation of americans will be:"]
```

```
## $`life for the next generation of americans will be:`
##                 better than today worse than today about the same
## clinton                 0.4816147        0.2324912      0.2858941
## trump                   0.3135508        0.4776879      0.2087612
## other/no answer         0.2295359        0.4227848      0.3476793
```

```r
cconds["importance of debates to your vote"]
```

```
## $`importance of debates to your vote`
##                 most important factor an important factor a minor factor
## clinton                     0.3019954           0.4326682      0.1600928
## trump                       0.2616700           0.3742256      0.2440450
## other/no answer             0.1542169           0.4493976      0.1686747
##                 not a factor
## clinton            0.1052436
## trump              0.1200593
## other/no answer    0.2277108
```

```r
cconds["were debates a factor in your vote?"]
```

```
## $`were debates a factor in your vote?`
##                       yes        no
## clinton         0.8929178 0.1070822
## trump           0.8796782 0.1203218
## other/no answer 0.7616646 0.2383354
```

