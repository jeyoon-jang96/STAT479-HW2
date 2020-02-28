HW2
================
Jane Jang
2/27/2020

``` r
library(blscrapeR)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------- tidyverse 1.3.0 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v stringr 1.4.0
    ## v tidyr   1.0.2     v forcats 0.4.0
    ## v readr   1.3.1

    ## -- Conflicts ------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Loaded glmnet 3.0-2

``` r
library(Matrix)
```

``` r
BLS <- get_bls_county(date_mth = "December 2018")
BLS <- BLS %>%
  filter(fips_state == 55)
```

``` r
bridges <- read.csv("2018AllRecordsDelimitedAllStates.csv")
```

    ## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec = dec, :
    ## EOF within quoted string

``` r
bridges <- bridges %>%
  filter(STATE_CODE_001 == 55)
```

``` r
summary(bridges$STATE_CODE_001)
```

    ##     01     02     04     05     10   1014     12     13     15     16 165.11 
    ##      0      0      0      0      0      0      0      0      0      0      0 
    ##     17     18     19     20     21     22     23     24     25     26     27 
    ##      0      0      0      0      0      0      0      0      0      0      0 
    ##     28     29     30     31     32     33     34     35     36     37     38 
    ##      0      0      0      0      0      0      0      0      0      0      0 
    ##     39     40     41     42     49     53     54     55      G 
    ##      0      0      0      0      0      0      0   7071      0

``` r
summary(bridges$COUNTY_CODE_003)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       1      33      53      56      73     135

``` r
bridge <- bridges %>%
  filter(!(STATE_CODE_001 %in% c('G' , '1014', '164.11'))) %>%
  filter(!(is.na(COUNTY_CODE_003)))

#Confirm Data Cleaning
summary(bridges$STATE_CODE_001)
```

    ##     01     02     04     05     10   1014     12     13     15     16 165.11 
    ##      0      0      0      0      0      0      0      0      0      0      0 
    ##     17     18     19     20     21     22     23     24     25     26     27 
    ##      0      0      0      0      0      0      0      0      0      0      0 
    ##     28     29     30     31     32     33     34     35     36     37     38 
    ##      0      0      0      0      0      0      0      0      0      0      0 
    ##     39     40     41     42     49     53     54     55      G 
    ##      0      0      0      0      0      0      0   7071      0

``` r
summary(bridges$COUNTY_CODE_003)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       1      33      53      56      73     135

``` r
#Rename Bridge Data Column
bridge <- bridge %>%
  rename(fips_state = STATE_CODE_001, fips_county = COUNTY_CODE_003)
```

``` r
data <-merge(bridge,BLS,by=c("fips_state","fips_county"))
str(data)
```

    ## 'data.frame':    1214 obs. of  145 variables:
    ##  $ fips_state             : Factor w/ 42 levels "01","02","04",..: 41 41 41 41 41 41 41 41 41 41 ...
    ##  $ fips_county            : int  101 101 101 101 101 101 101 101 101 101 ...
    ##  $ STRUCTURE_NUMBER_008   : Factor w/ 294581 levels "","           0034",..: 276906 276907 276918 276919 276920 276908 276909 276930 276931 276932 ...
    ##  $ RECORD_TYPE_005A       : Factor w/ 30 levels "","1","2","37.13",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ ROUTE_PREFIX_005B      : int  5 5 5 5 5 5 5 5 4 4 ...
    ##  $ SERVICE_LEVEL_005C     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ ROUTE_NUMBER_005D      : Factor w/ 20700 levels "","0-59A","0000_",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ DIRECTION_005E         : int  2 2 2 1 1 2 2 2 2 2 ...
    ##  $ HIGHWAY_DISTRICT_002   : Factor w/ 110 levels ""," 0"," 1"," 2",..: 10 10 10 10 10 10 10 10 10 10 ...
    ##  $ PLACE_CODE_004         : num  11950 11950 66375 57700 66000 ...
    ##  $ FEATURES_DESC_006A     : Factor w/ 95465 levels "","''700' CREEK             '",..: 67392 67392 67392 57137 67392 67393 67393 10391 24713 74349 ...
    ##  $ CRITICAL_FACILITY_006B : Factor w/ 4 levels "","'0.1M N JCT USH 14        '",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ FACILITY_CARRIED_007   : Factor w/ 122215 levels "","''","''A' AVE.          '",..: 64092 62940 62282 64380 63481 64092 62927 62264 30017 30108 ...
    ##  $ LOCATION_009           : Factor w/ 267190 levels "","''","''0.5 N JCT 5207'         '",..: 119319 54796 97689 48375 70793 60123 97549 60299 167456 97553 ...
    ##  $ MIN_VERT_CLR_010       : Factor w/ 974 levels "","'1.77 MI W OF MD 450      '",..: 974 974 974 974 974 974 974 974 974 974 ...
    ##  $ KILOPOINT_011          : Factor w/ 53145 levels "","'\"D\" LINE DIKE '",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ BASE_HWY_NETWORK_012   : Factor w/ 8 levels "","'BEAR RIVER MBR           '",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ LRS_INV_ROUTE_013A     : Factor w/ 16476 levels "","_000000202",..: 5 5 5 5 5 5 5 5 5 5 ...
    ##  $ SUBROUTE_NO_013B       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ LAT_016                : num  42494255 42470514 42503683 42455272 42432490 ...
    ##  $ LONG_017               : num  87524091 87515362 87583345 87463925 87475610 ...
    ##  $ DETOUR_KILOS_019       : int  8 9 3 3 1 9 8 6 4 6 ...
    ##  $ TOLL_020               : int  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ MAINTENANCE_021        : int  4 4 3 4 4 3 3 3 2 2 ...
    ##  $ OWNER_022              : int  4 4 3 4 4 3 3 3 2 2 ...
    ##  $ FUNCTIONAL_CLASS_026   : int  9 9 16 19 14 9 9 9 7 7 ...
    ##  $ YEAR_BUILT_027         : int  1962 1960 1975 1920 1936 1965 1965 1960 1939 1970 ...
    ##  $ TRAFFIC_LANES_ON_028A  : int  2 2 2 2 2 2 2 2 2 2 ...
    ##  $ TRAFFIC_LANES_UND_028B : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ADT_029                : num  1294 4056 340 587 11700 ...
    ##  $ YEAR_ADT_030           : num  2015 2015 2015 2015 2017 ...
    ##  $ DESIGN_LOAD_031        : Factor w/ 17 levels "","0","1","2",..: 14 14 8 9 8 8 8 4 4 4 ...
    ##  $ APPR_WIDTH_MT_032      : Factor w/ 629 levels "","0","0.1","0.3",..: 609 609 609 588 48 609 616 609 33 616 ...
    ##  $ MEDIAN_CODE_033        : Factor w/ 9 levels "","0","1","10.7",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ DEGREES_SKEW_034       : Factor w/ 95 levels "","0","1","10",..: 37 2 7 2 2 2 2 2 37 2 ...
    ##  $ STRUCTURE_FLARED_035   : Factor w/ 7 levels "","0","1","10",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ RAILINGS_036A          : Factor w/ 4 levels "","0","1","N": 3 3 2 2 2 3 3 2 2 2 ...
    ##  $ TRANSITIONS_036B       : Factor w/ 5 levels "","0","1","5",..: 3 3 5 5 2 2 2 5 2 5 ...
    ##  $ APPR_RAIL_036C         : Factor w/ 5 levels "","0","1","5",..: 3 3 2 2 2 2 2 2 2 2 ...
    ##  $ APPR_RAIL_END_036D     : Factor w/ 5 levels "","0","1","5",..: 3 3 2 2 2 2 2 3 2 2 ...
    ##  $ HISTORY_037            : Factor w/ 8 levels "","0","1","2",..: 7 7 7 7 7 7 7 7 7 7 ...
    ##  $ NAVIGATION_038         : Factor w/ 6 levels "","0","1","5",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ NAV_VERT_CLR_MT_039    : Factor w/ 267 levels "","0","0.3","0.6",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ NAV_HORR_CLR_MT_040    : Factor w/ 577 levels "","0","0.3","0.7",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ OPEN_CLOSED_POSTED_041 : Factor w/ 13 levels "","0","1","3",..: 6 6 6 6 6 6 6 6 6 6 ...
    ##  $ SERVICE_ON_042A        : Factor w/ 15 levels "","0","01","1",..: 10 10 4 4 10 4 4 4 4 4 ...
    ##  $ SERVICE_UND_042B       : int  5 5 5 5 5 5 5 5 5 5 ...
    ##  $ STRUCTURE_KIND_043A    : Factor w/ 14 levels "","0","00","1",..: 10 9 9 4 4 9 9 13 7 7 ...
    ##  $ STRUCTURE_TYPE_043B    : int  2 2 2 2 11 4 4 19 19 19 ...
    ##  $ APPR_KIND_044A         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ APPR_TYPE_044B         : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ MAIN_UNIT_SPANS_045    : num  3 2 2 1 3 2 2 2 1 2 ...
    ##  $ APPR_SPANS_046         : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ HORR_CLR_MT_047        : num  9.1 9.1 9.1 8.5 12.2 9.1 9.8 9.1 11 9.8 ...
    ##  $ MAX_SPAN_LEN_MT_048    : num  18.3 18.7 18.9 9.8 25 13.9 13.8 3.8 15.2 3 ...
    ##  $ STRUCTURE_LEN_MT_049   : num  56.1 38.2 38.4 10.4 58.5 28.4 28.4 9.1 15.2 6.9 ...
    ##  $ LEFT_CURB_MT_050A      : num  1.4 1.2 0 2.5 2.4 0 0 0 0 0 ...
    ##  $ RIGHT_CURB_MT_050B     : num  1.4 1.2 0 2.5 2.4 0 0 0 0 0 ...
    ##  $ ROADWAY_WIDTH_MT_051   : Factor w/ 709 levels "","0","0.2","0.4",..: 688 688 688 666 47 690 690 2 2 2 ...
    ##  $ DECK_WIDTH_MT_052      : Factor w/ 904 levels "","0","0.2","0.3",..: 867 867 866 96 167 863 863 2 2 2 ...
    ##  $ VERT_CLR_OVER_MT_053   : Factor w/ 500 levels "","0","0.3","0.4",..: 499 499 499 499 499 499 499 499 499 499 ...
    ##  $ VERT_CLR_UND_REF_054A  : Factor w/ 7 levels "","0","4.6","99.99",..: 6 6 6 6 6 6 6 6 6 6 ...
    ##  $ VERT_CLR_UND_054B      : Factor w/ 1027 levels "","0","0.03",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ LAT_UND_REF_055A       : Factor w/ 8 levels "","0","32.69",..: 7 7 7 7 7 7 7 7 7 7 ...
    ##  $ LAT_UND_MT_055B        : Factor w/ 268 levels "","0","0.1","0.2",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ LEFT_LAT_UND_MT_056    : Factor w/ 296 levels "","0","0.1","0.2",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ DECK_COND_058          : Factor w/ 12 levels "","0","1","2",..: 9 9 7 6 8 7 7 12 12 12 ...
    ##  $ SUPERSTRUCTURE_COND_059: Factor w/ 12 levels "","0","1","2",..: 10 10 9 6 7 8 8 12 12 12 ...
    ##  $ SUBSTRUCTURE_COND_060  : Factor w/ 12 levels "","0","1","2",..: 9 9 9 7 8 9 8 12 12 12 ...
    ##  $ CHANNEL_COND_061       : Factor w/ 14 levels "","0","1","2",..: 11 12 12 11 11 7 12 10 10 7 ...
    ##  $ CULVERT_COND_062       : Factor w/ 14 levels "","0","1","2",..: 14 14 14 14 14 14 14 12 10 10 ...
    ##  $ OPR_RATING_METH_063    : Factor w/ 20 levels "","0","1","12.7",..: 3 3 3 3 3 2 2 2 2 2 ...
    ##  $ OPERATING_RATING_064   : num  70.2 78.4 60.4 65.3 45.7 34.3 35.9 53.9 53.9 53.9 ...
    ##  $ INV_RATING_METH_065    : Factor w/ 20 levels "","0","1","12.7",..: 3 3 3 3 3 2 2 2 2 2 ...
    ##  $ INVENTORY_RATING_066   : Factor w/ 967 levels "","0","0.1","0.3",..: 313 329 189 355 205 141 141 173 173 173 ...
    ##  $ STRUCTURAL_EVAL_067    : Factor w/ 15 levels "","*","0","2",..: 12 12 11 8 10 10 11 11 11 11 ...
    ##  $ DECK_GEOMETRY_EVAL_068 : Factor w/ 12 levels "","*","0","2",..: 7 6 8 7 7 8 8 12 12 12 ...
    ##  $ UNDCLRENCE_EVAL_069    : Factor w/ 12 levels "","0","2","3",..: 12 12 12 12 12 12 12 12 12 12 ...
    ##  $ POSTING_EVAL_070       : Factor w/ 11 levels "","0","1","2",..: 7 7 7 7 7 7 7 7 7 7 ...
    ##  $ WATERWAY_EVAL_071      : Factor w/ 11 levels "","0","2","3",..: 9 9 9 9 9 8 9 9 9 9 ...
    ##  $ APPR_ROAD_EVAL_072     : Factor w/ 11 levels "","0","2","3",..: 9 9 7 9 9 9 9 9 9 9 ...
    ##  $ WORK_PROPOSED_075A     : int  NA NA NA 31 NA NA NA NA NA NA ...
    ##  $ WORK_DONE_BY_075B      : int  NA NA NA 2 NA NA NA NA NA NA ...
    ##  $ IMP_LEN_MT_076         : Factor w/ 3530 levels "","0","0.1","0.3",..: 2 2 2 492 2 2 2 2 2 2 ...
    ##  $ DATE_OF_INSPECT_090    : Factor w/ 175 levels "","1001","1005",..: 171 171 14 15 15 14 14 171 14 14 ...
    ##  $ INSPECT_FREQ_MONTHS_091: Factor w/ 31 levels "","1 ","10","11",..: 18 18 18 5 18 18 18 18 18 18 ...
    ##  $ FRACTURE_092A          : Factor w/ 15 levels "","24","N  ",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ UNDWATER_LOOK_SEE_092B : Factor w/ 26 levels "","1016","N  ",..: 3 3 3 3 26 3 3 3 3 3 ...
    ##  $ SPEC_INSPECT_092C      : Factor w/ 31 levels "","24","N  ",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ FRACTURE_LAST_DATE_093A: Factor w/ 121 levels "","0","0100",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ UNDWATER_LAST_DATE_093B: Factor w/ 117 levels "","0","0100",..: 1 1 1 1 62 1 1 1 1 1 ...
    ##  $ SPEC_LAST_DATE_093C    : Factor w/ 120 levels "","0","0100",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ BRIDGE_IMP_COST_094    : int  0 0 0 158 0 0 0 0 0 0 ...
    ##  $ ROADWAY_IMP_COST_095   : int  0 0 0 15 0 0 0 0 0 0 ...
    ##  $ TOTAL_IMP_COST_096     : int  0 0 0 238 0 0 0 0 0 0 ...
    ##  $ YEAR_OF_IMP_097        : int  2017 2017 2017 2017 2017 2017 2017 2017 2017 2017 ...
    ##  $ OTHER_STATE_CODE_098A  : Factor w/ 51 levels "","0","014","049",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ OTHER_STATE_PCNT_098B  : Factor w/ 71 levels "","-1","0","1",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ OTHR_STATE_STRUC_NO_099: Factor w/ 606 levels "","-              ",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##   [list output truncated]

``` r
#Change some colnames structure (applied to some factors which has more than 900 factors)
data$APPR_WIDTH_MT_032<- as.numeric(data$APPR_WIDTH_MT_032 )
data$ROADWAY_WIDTH_MT_051 <- as.numeric(data$ROADWAY_WIDTH_MT_051)
data$DECK_WIDTH_MT_052 <- as.numeric(data$DECK_WIDTH_MT_052)
data$VERT_CLR_OVER_MT_053 <- as.numeric(data$VERT_CLR_OVER_MT_053)
data$VERT_CLR_UND_054B <- as.numeric(data$VERT_CLR_UND_054B)
data$INVENTORY_RATING_066 <- as.numeric(data$INVENTORY_RATING_066)
#change the structure of fips_county to factor variable
data$fips_county <- as.factor(data$fips_county)
```

``` r
finaldata1 <- data %>%
  select_if(is.numeric)%>%
  drop_na() %>%
  select(-unemployed_rate)
```

``` r
finaldata2 <-data %>%
  select_if(is.numeric)%>%
  drop_na() %>%
  select(-unemployed)
```

``` r
x_var <- model.matrix(unemployed~.,finaldata1)[,-1]
y_var <- finaldata1$unemployed
lambda_seq<- 10^seq(2,-2, by= -.1)

set.seed(12345)
train = sample(1:nrow(x_var), nrow(x_var)/2)
test = (-train)
y_test = y_var[test]

cv_output <-cv.glmnet(x_var[train,], y_var[train], alpha=1, lamda= lamda_seq)
```

    ## Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
    ## fold

``` r
best_lam <- cv_output$lambda.min

lasso_best <- glmnet(x_var[train,], y_var[train], alpha = 1, lambda = best_lam)

pred <- predict(lasso_best, s=best_lam, newx = x_var[test,])

mod <- cbind (y_var[test], pred)

head(mod)
```

    ##                 1
    ## 15  3193 2657.490
    ## 273 2444 2325.656
    ## 285 2444 2325.656
    ## 286 2444 2325.656
    ## 299 2444 2325.656
    ## 306 2444 2325.656

``` r
tail(mod)
```

    ##                   1
    ## 748   353  431.3632
    ## 829  1400 1763.3807
    ## 924   400  513.1093
    ## 982   529  641.7362
    ## 1147  748  876.0174
    ## 1191  748  876.0174

``` r
coef(lasso_best)
```

    ## 50 x 1 sparse Matrix of class "dgCMatrix"
    ##                                 s0
    ## (Intercept)            239.9494207
    ## ROUTE_PREFIX_005B        .        
    ## SERVICE_LEVEL_005C       .        
    ## DIRECTION_005E           .        
    ## PLACE_CODE_004           .        
    ## SUBROUTE_NO_013B         .        
    ## LAT_016                  .        
    ## LONG_017                 .        
    ## DETOUR_KILOS_019         .        
    ## TOLL_020                 .        
    ## MAINTENANCE_021          .        
    ## OWNER_022                .        
    ## FUNCTIONAL_CLASS_026     .        
    ## YEAR_BUILT_027           .        
    ## TRAFFIC_LANES_ON_028A    .        
    ## TRAFFIC_LANES_UND_028B   .        
    ## ADT_029                  .        
    ## YEAR_ADT_030             .        
    ## APPR_WIDTH_MT_032        .        
    ## SERVICE_UND_042B         .        
    ## STRUCTURE_TYPE_043B      .        
    ## APPR_KIND_044A           .        
    ## APPR_TYPE_044B           .        
    ## MAIN_UNIT_SPANS_045      .        
    ## APPR_SPANS_046           .        
    ## HORR_CLR_MT_047          .        
    ## MAX_SPAN_LEN_MT_048      .        
    ## STRUCTURE_LEN_MT_049     .        
    ## LEFT_CURB_MT_050A        .        
    ## RIGHT_CURB_MT_050B       .        
    ## ROADWAY_WIDTH_MT_051     .        
    ## DECK_WIDTH_MT_052        .        
    ## VERT_CLR_OVER_MT_053     .        
    ## VERT_CLR_UND_054B        .        
    ## OPERATING_RATING_064     .        
    ## INVENTORY_RATING_066     .        
    ## WORK_PROPOSED_075A       .        
    ## WORK_DONE_BY_075B        .        
    ## BRIDGE_IMP_COST_094      .        
    ## ROADWAY_IMP_COST_095     .        
    ## TOTAL_IMP_COST_096       .        
    ## YEAR_OF_IMP_097          .        
    ## FEDERAL_LANDS_105        .        
    ## YEAR_RECONSTRUCTED_106   .        
    ## PROJ_SUFFIX              .        
    ## NBI_TYPE_OF_IMP          .        
    ## DTL_TYPE_OF_IMP          .        
    ## CAT29                    .        
    ## labor_force              0.0246223
    ## employed                 .

\#The first linear model is to predict the number of employed. To figure
out important factors to predict the number of employed, lasso
regression method was used. The labor force and deck width was
statitically significant variables to predict the nubmer of labor force.

``` r
x_var2 <- model.matrix(unemployed_rate~.,finaldata2)[,-1]
y_var2 <- finaldata2$unemployed_rate
lambda_seq<- 10^seq(2,-2, by= -.1)

set.seed(12345)
train2 = sample(1:nrow(x_var2), nrow(x_var2)/2)
test2 = (-train2)
y_test2 = y_var2[test2]

cv_output2 <-cv.glmnet(x_var2[train2,], y_var2[train2], alpha=1, lamda= lamda_seq)
```

    ## Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
    ## fold

``` r
best_lam2 <- cv_output2$lambda.min

lasso_best2 <- glmnet(x_var2[train2,], y_var2[train2], alpha = 1, lambda = best_lam2)

pred2 <- predict(lasso_best2, s=best_lam2, newx = x_var2[test,])

mod2 <- cbind (y_var2[test2], pred2)

head(mod2)
```

    ##                1
    ## 15  3.3 2.718152
    ## 273 2.9 3.010373
    ## 285 2.9 2.879458
    ## 286 2.9 2.860289
    ## 299 2.9 2.871150
    ## 306 2.9 2.979714

``` r
tail(mod2)
```

    ##                 1
    ## 748  4.5 4.124983
    ## 829  2.3 2.795290
    ## 924  3.6 3.697604
    ## 982  3.2 3.712356
    ## 1147 2.9 3.260439
    ## 1191 2.9 3.204080

``` r
coef(lasso_best2)
```

    ## 50 x 1 sparse Matrix of class "dgCMatrix"
    ##                                   s0
    ## (Intercept)            -2.023294e+01
    ## ROUTE_PREFIX_005B       .           
    ## SERVICE_LEVEL_005C      .           
    ## DIRECTION_005E          .           
    ## PLACE_CODE_004          .           
    ## SUBROUTE_NO_013B        .           
    ## LAT_016                 1.822123e-07
    ## LONG_017                1.742465e-07
    ## DETOUR_KILOS_019        1.557691e-03
    ## TOLL_020                .           
    ## MAINTENANCE_021         .           
    ## OWNER_022               .           
    ## FUNCTIONAL_CLASS_026    .           
    ## YEAR_BUILT_027          .           
    ## TRAFFIC_LANES_ON_028A   .           
    ## TRAFFIC_LANES_UND_028B  .           
    ## ADT_029                 .           
    ## YEAR_ADT_030            .           
    ## APPR_WIDTH_MT_032       .           
    ## SERVICE_UND_042B        .           
    ## STRUCTURE_TYPE_043B     .           
    ## APPR_KIND_044A          .           
    ## APPR_TYPE_044B          .           
    ## MAIN_UNIT_SPANS_045     .           
    ## APPR_SPANS_046          .           
    ## HORR_CLR_MT_047         .           
    ## MAX_SPAN_LEN_MT_048     .           
    ## STRUCTURE_LEN_MT_049    .           
    ## LEFT_CURB_MT_050A       .           
    ## RIGHT_CURB_MT_050B      .           
    ## ROADWAY_WIDTH_MT_051    .           
    ## DECK_WIDTH_MT_052       .           
    ## VERT_CLR_OVER_MT_053    .           
    ## VERT_CLR_UND_054B       .           
    ## OPERATING_RATING_064    .           
    ## INVENTORY_RATING_066   -1.668089e-04
    ## WORK_PROPOSED_075A      .           
    ## WORK_DONE_BY_075B       .           
    ## BRIDGE_IMP_COST_094     .           
    ## ROADWAY_IMP_COST_095    .           
    ## TOTAL_IMP_COST_096      .           
    ## YEAR_OF_IMP_097         .           
    ## FEDERAL_LANDS_105       .           
    ## YEAR_RECONSTRUCTED_106  .           
    ## PROJ_SUFFIX             .           
    ## NBI_TYPE_OF_IMP         .           
    ## DTL_TYPE_OF_IMP         .           
    ## CAT29                   .           
    ## labor_force             .           
    ## employed                .

\#The second lasso regression is to predict the unemployment rate. To
prevent correlation between the number of unempolyed and the
unemployment rate, the number of unemployed is deleted in the dataset.
Interestingly, the lantitude and longtide of bridge was statistically
significant variables to predict the unemployment rate.

``` r
November <-get_bls_county(date_mth = "November 2018")
November <- November %>%
  filter(fips_state == 55)
```

``` r
November <- November %>%
  rename( Nov_unemployed = unemployed, Nov_unemplyed_rate=unemployed_rate)

Newdata <- merge(data, November, by =c("fips_state", "fips_county"))
```

``` r
Newdata$APPR_WIDTH_MT_032<- as.numeric(Newdata$APPR_WIDTH_MT_032 )
Newdata$ROADWAY_WIDTH_MT_051 <- as.numeric(Newdata$ROADWAY_WIDTH_MT_051)
Newdata$DECK_WIDTH_MT_052 <- as.numeric(Newdata$DECK_WIDTH_MT_052)
Newdata$VERT_CLR_OVER_MT_053 <- as.numeric(Newdata$VERT_CLR_OVER_MT_053)
Newdata$VERT_CLR_UND_054B <- as.numeric(Newdata$VERT_CLR_UND_054B)
Newdata$INVENTORY_RATING_066 <- as.numeric(Newdata$INVENTORY_RATING_066)
#change the structure of fips_county to factor variable
Newdata$fips_county <- as.factor(Newdata$fips_county)

#Save only columns which has numeric values
Newdata <- Newdata %>%
  select(-unemployed) %>%
  drop_na() %>%
  select_if(is.numeric)
```

``` r
x_var3 <- model.matrix(unemployed_rate~.,Newdata)[,-1]
y_var3 <- Newdata$unemployed_rate
lambda_seq<- 10^seq(2,-2, by= -.1)

set.seed(12345)
train3 = sample(1:nrow(x_var3), nrow(x_var3)/2)
test3 = (-train3)
y_test3 = y_var3[test3]

cv_output3 <-cv.glmnet(x_var3[train3,], y_var3[train3], alpha=1, lamda= lamda_seq)
```

    ## Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
    ## fold

``` r
best_lam3 <- cv_output3$lambda.min

lasso_best3 <- glmnet(x_var3[train3,], y_var3[train3], alpha = 1, lambda = best_lam3)

pred3 <- predict(lasso_best3, s=best_lam3, newx = x_var3[test,])

mod3 <- cbind (y_var3[test3], pred3)

head(mod3)
```

    ##                1
    ## 15  3.3 2.634061
    ## 273 2.9 2.959829
    ## 285 2.9 2.896217
    ## 286 2.9 2.912580
    ## 299 2.9 2.934323
    ## 306 2.9 2.989298

``` r
tail(mod3)
```

    ##                 1
    ## 748  4.5 4.944604
    ## 829  2.3 2.438124
    ## 924  3.6 3.391164
    ## 982  3.2 3.203332
    ## 1147 2.9 3.016019
    ## 1191 2.9 2.871215

``` r
coef(lasso_best3)
```

    ## 54 x 1 sparse Matrix of class "dgCMatrix"
    ##                                   s0
    ## (Intercept)            -2.207674e+01
    ## ROUTE_PREFIX_005B       .           
    ## SERVICE_LEVEL_005C      .           
    ## DIRECTION_005E          .           
    ## PLACE_CODE_004          .           
    ## SUBROUTE_NO_013B        .           
    ## LAT_016                 3.301216e-07
    ## LONG_017                1.018410e-07
    ## DETOUR_KILOS_019        2.572006e-03
    ## TOLL_020                .           
    ## MAINTENANCE_021         .           
    ## OWNER_022               .           
    ## FUNCTIONAL_CLASS_026    .           
    ## YEAR_BUILT_027          .           
    ## TRAFFIC_LANES_ON_028A   .           
    ## TRAFFIC_LANES_UND_028B  .           
    ## ADT_029                 .           
    ## YEAR_ADT_030            .           
    ## APPR_WIDTH_MT_032       .           
    ## SERVICE_UND_042B        .           
    ## STRUCTURE_TYPE_043B     .           
    ## APPR_KIND_044A          .           
    ## APPR_TYPE_044B          .           
    ## MAIN_UNIT_SPANS_045     .           
    ## APPR_SPANS_046          .           
    ## HORR_CLR_MT_047         .           
    ## MAX_SPAN_LEN_MT_048     .           
    ## STRUCTURE_LEN_MT_049   -6.024347e-04
    ## LEFT_CURB_MT_050A      -6.840126e-03
    ## RIGHT_CURB_MT_050B      .           
    ## ROADWAY_WIDTH_MT_051    .           
    ## DECK_WIDTH_MT_052       .           
    ## VERT_CLR_OVER_MT_053    .           
    ## VERT_CLR_UND_054B       .           
    ## OPERATING_RATING_064    .           
    ## INVENTORY_RATING_066   -2.383269e-05
    ## WORK_PROPOSED_075A      .           
    ## WORK_DONE_BY_075B       .           
    ## BRIDGE_IMP_COST_094     .           
    ## ROADWAY_IMP_COST_095    .           
    ## TOTAL_IMP_COST_096      .           
    ## YEAR_OF_IMP_097         .           
    ## FEDERAL_LANDS_105       .           
    ## YEAR_RECONSTRUCTED_106  .           
    ## PROJ_SUFFIX             .           
    ## NBI_TYPE_OF_IMP         .           
    ## DTL_TYPE_OF_IMP         .           
    ## CAT29                   .           
    ## labor_force.x           .           
    ## employed.x              .           
    ## labor_force.y           .           
    ## employed.y              .           
    ## Nov_unemployed          .           
    ## Nov_unemplyed_rate      6.105465e-01

\#The third regression is to predict the unemployment rate by adding the
unemployed number andr rate from the previous month. Certainly, the
unemployed rate in November was one of important variables. Also,
latitude, longtitude, detour length, length of maximum span, and
structure length were statistically significant variables to predict the
unemployed rate in December.
