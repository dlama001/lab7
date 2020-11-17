# lab7
lab7
dawa lama
11/10/2020
load("C:/Users/singh/OneDrive/Desktop/project new/lab7/NHIS_2014.RData")
load("NHIS_2014.Rdata")
Predicting Health Insurance Status Predicting the People that have health insurance coverage and do not have any health insurance coverage available by using different model.

IN this lab, I will estimate a variety of models to try to predict if a person has health insurance. Start with logit like last week then some different models: Random Forest, Support Vector Machines, and Elastic Net (which is not optimal for a 0/1 dependent but it works for a demonstration) via data set NHIS. Here I am going to include the age between 18 to 75.

I run the code and than gae me result as below.

I found age between 25-75 56842= Cover/who has a health insurance(83.33% out of 100% ) 11375= No of people not cover by health insurance(16.67%)

data_use1$earn_lastyr <- as.factor(data_use1$ERNYR_P)
levels(data_use1$earn_lastyr) <- c("0","$01-$4999","$5000-$9999","$10000-$14999","$15000-$19999","$20000-$24999","$25000-$34999","$35000-$44999","$45000-$54999","$55000-$64999","$65000-$74999","$75000 and over",NA,NA,NA)
First decide on how you’re defining your subgroup (all adults? Within certain age? Other?) then find some basic statistics – what fraction are not covered? (Later go back to look at simple stats for subgroups to see if there are sharp differences.)

dat2 <- subset(data_use1, (AGE_P >= 25) & (AGE_P <= 75))
covprop <- dat2$NOTCOV
table(covprop)
## covprop
##     0     1 
## 58046 10171
I set the subset age between age 25-75, which gave the result below-

I run a logit regression.As below.And which gave the resilt below.

model_logit1 <- glm(NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + RaceOther  
                    + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
                    + married + widowed + divorc_sep + veteran_stat + REGION + region_born,
                    family = binomial, data = dat2)
summary(model_logit1)
## 
## Call:
## glm(formula = NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + 
##     RaceOther + Hispanic + educ_hs + educ_smcoll + educ_as + 
##     educ_bach + educ_adv + married + widowed + divorc_sep + veteran_stat + 
##     REGION + region_born, family = binomial, data = dat2)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9337  -0.5632  -0.3577  -0.1912   3.2671  
## 
## Coefficients:
##                                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -2.305e+00  1.661e-01 -13.877  < 2e-16 ***
## AGE_P                          9.955e-02  7.514e-03  13.249  < 2e-16 ***
## I(AGE_P^2)                    -1.546e-03  8.271e-05 -18.693  < 2e-16 ***
## female                        -2.978e-01  2.428e-02 -12.263  < 2e-16 ***
## AfAm                          -1.950e-01  3.654e-02  -5.337 9.45e-08 ***
## Asian                         -1.374e-01  8.513e-02  -1.614 0.106630    
## RaceOther                      4.586e-01  7.348e-02   6.241 4.34e-10 ***
## Hispanic                       2.206e-01  4.071e-02   5.419 6.00e-08 ***
## educ_hs                       -2.796e-01  3.290e-02  -8.500  < 2e-16 ***
## educ_smcoll                   -5.803e-01  3.896e-02 -14.894  < 2e-16 ***
## educ_as                       -8.063e-01  4.571e-02 -17.641  < 2e-16 ***
## educ_bach                     -1.474e+00  4.626e-02 -31.862  < 2e-16 ***
## educ_adv                      -2.133e+00  7.285e-02 -29.283  < 2e-16 ***
## married                       -7.384e-01  2.845e-02 -25.952  < 2e-16 ***
## widowed                       -1.229e-01  8.570e-02  -1.434 0.151594    
## divorc_sep                    -9.045e-02  3.942e-02  -2.295 0.021751 *  
## veteran_stat                  -6.335e-01  6.227e-02 -10.174  < 2e-16 ***
## REGIONMidwest                  2.569e-01  4.441e-02   5.783 7.32e-09 ***
## REGIONSouth                    6.551e-01  3.831e-02  17.098  < 2e-16 ***
## REGIONWest                     2.526e-01  4.050e-02   6.238 4.43e-10 ***
## region_bornMex Cent Am Caribb  1.086e+00  4.382e-02  24.790  < 2e-16 ***
## region_bornS Am                1.017e+00  9.001e-02  11.298  < 2e-16 ***
## region_bornEur                 3.636e-01  1.104e-01   3.293 0.000990 ***
## region_bornformer USSR         9.738e-01  2.249e-01   4.329 1.50e-05 ***
## region_bornAfrica              8.091e-01  1.163e-01   6.956 3.49e-12 ***
## region_bornMidE                5.960e-01  1.866e-01   3.194 0.001401 ** 
## region_bornIndia subc          6.902e-01  1.366e-01   5.053 4.35e-07 ***
## region_bornAsia                8.421e-01  1.252e-01   6.724 1.77e-11 ***
## region_bornSE Asia             3.909e-01  1.136e-01   3.442 0.000577 ***
## region_bornElsewhere           3.350e-01  1.702e-01   1.968 0.049084 *  
## region_bornunknown             9.030e-03  1.993e-01   0.045 0.963865    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 57458  on 68216  degrees of freedom
## Residual deviance: 46665  on 68186  degrees of freedom
## AIC: 46727
## 
## Number of Fisher Scoring iterations: 6
First I am going to change the data_use1 and give name dat2, and I run the code as below.

d_region <- data.frame(model.matrix(~ dat2$REGION))
d_region_born <- data.frame(model.matrix(~ factor(dat2$region_born)))  # snips any with zero in the subgroup
dat_for_analysis_sub <- data.frame(
  dat2$NOTCOV,
  dat2$AGE_P,
  dat2$female,
  dat2$AfAm,
  dat2$Asian,
  dat2$RaceOther,
  dat2$Hispanic,
  dat2$educ_hs,
  dat2$educ_smcoll,
  dat2$educ_as,
  dat2$educ_bach,
  dat2$educ_adv,
  dat2$married,
  dat2$widowed,
  dat2$divorc_sep,
  d_region[,2:4],
  d_region_born[,2:12]) # need [] since model.matrix includes intercept term
names(dat_for_analysis_sub) <- c("NOTCOV",
                                 "Age",
                                 "female",
                                 "AfAm",
                                 "Asian",
                                 "RaceOther",
                                 "Hispanic",
                                 "educ_hs",
                                 "educ_smcoll",
                                 "educ_as",
                                 "educ_bach",
                                 "educ_adv",
                                 "married",
                                 "widowed",
                                 "divorc_sep",
                                 "Region.Midwest",
                                 "Region.South",
                                 "Region.West",
                                 "born.Mex.CentAm.Carib",
                                 "born.S.Am",
                                 "born.Eur",
                                 "born.f.USSR",
                                 "born.Africa",
                                 "born.MidE",
                                 "born.India.subc",
                                 "born.Asia",
                                 "born.SE.Asia",
                                 "born.elsewhere",
                                 "born.unknown")
Now I am going to create standardize analysis, which is blow.As question said to try the max value 0.75, I have try with 0.6. Summary(restrict_1) has gave me result as below, where we can see 56482 is cover by health Insurance and 11375 is not cover by health isurance.

require("standardize")
## Loading required package: standardize
set.seed(654321)
NN <- length(dat_for_analysis_sub$NOTCOV)
restrict_1 <- as.logical(round(runif(NN,min=0,max=0.6))) # use fraction as training data
#runif(<0.6)
#we can bump this down depending on how fast our computer is
#round a number from the uniform distribution, a number between 1 and 0.6 (logical function 0 wi)
summary(restrict_1)
##    Mode   FALSE    TRUE 
## logical   56842   11375
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)
sobj <- standardize(NOTCOV ~ Age + female + AfAm + Asian + RaceOther + Hispanic + 
                      educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv + 
                      married + widowed + divorc_sep + 
                      Region.Midwest + Region.South + Region.West + 
                      born.Mex.CentAm.Carib + born.S.Am + born.Eur + born.f.USSR + 
                      born.Africa + born.MidE + born.India.subc + born.Asia + 
                      born.SE.Asia + born.elsewhere + born.unknown, dat_train, family = binomial)
s_dat_test <- predict(sobj, dat_test)
summary(sobj$data)
##      NOTCOV              Age.V1        female   AfAm     Asian     RaceOther
##  Min.   :0.0000   Min.   :-1.6411563   1:6029   1:1541   1:  836   1:  194  
##  1st Qu.:0.0000   1st Qu.:-0.8529717   0:5346   0:9834   0:10539   0:11181  
##  Median :0.0000   Median : 0.0068661                                        
##  Mean   :0.1429   Mean   : 0.0000000                                        
##  3rd Qu.:0.0000   3rd Qu.: 0.7950507                                        
##  Max.   :1.0000   Max.   : 1.9415011                                        
##  Hispanic educ_hs  educ_smcoll educ_as   educ_bach educ_adv  married  widowed  
##  1:2172   1:2885   1:1917      1: 1356   1:2203    1: 1260   1:6970   1:  403  
##  0:9203   0:8490   0:9458      0:10019   0:9172    0:10115   0:4405   0:10972  
##                                                                                
##                                                                                
##                                                                                
##                                                                                
##  divorc_sep Region.Midwest Region.South Region.West born.Mex.CentAm.Carib
##  1:1454     1:2309         1:3953       1:3236      1: 1356              
##  0:9921     0:9066         0:7422       0:8139      0:10019              
##                                                                          
##                                                                          
##                                                                          
##                                                                          
##  born.S.Am born.Eur  born.f.USSR born.Africa born.MidE born.India.subc
##  1:  154   1:  158   1:   29     1:   92     1:   48   1:  181        
##  0:11221   0:11217   0:11346     0:11283     0:11327   0:11194        
##                                                                       
##                                                                       
##                                                                       
##                                                                       
##  born.Asia born.SE.Asia born.elsewhere born.unknown
##  1:  167   1:  290      1:   64        1:   37     
##  0:11208   0:11085      0:11311        0:11338     
##                                                    
##                                                    
##                                                    
## 
OSL or Linear Probability Model. First I am going to run theLinear Probability Model (ie good ol’ OLS) and logit. As below

# LPM
model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
## 
## Call:
## lm(formula = sobj$formula, data = sobj$data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.65830 -0.17438 -0.08277  0.01038  1.05038 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             0.405485   0.071797   5.648 1.67e-08 ***
## Age                    -0.059457   0.003373 -17.627  < 2e-16 ***
## female1                -0.016074   0.003075  -5.227 1.76e-07 ***
## AfAm1                  -0.010823   0.004775  -2.267 0.023425 *  
## Asian1                 -0.017710   0.010845  -1.633 0.102502    
## RaceOther1              0.033053   0.011846   2.790 0.005275 ** 
## Hispanic1               0.014620   0.005885   2.484 0.013001 *  
## educ_hs1               -0.013234   0.005195  -2.547 0.010864 *  
## educ_smcoll1           -0.037275   0.005703  -6.535 6.62e-11 ***
## educ_as1               -0.049020   0.006197  -7.910 2.81e-15 ***
## educ_bach1             -0.069733   0.005668 -12.304  < 2e-16 ***
## educ_adv1              -0.079914   0.006452 -12.387  < 2e-16 ***
## married1               -0.043353   0.004022 -10.780  < 2e-16 ***
## widowed1               -0.023963   0.009266  -2.586 0.009717 ** 
## divorc_sep1            -0.008192   0.005579  -1.468 0.142005    
## Region.Midwest1         0.015559   0.005073   3.067 0.002169 ** 
## Region.South1           0.031463   0.004593   6.851 7.74e-12 ***
## Region.West1            0.010911   0.004832   2.258 0.023961 *  
## born.Mex.CentAm.Carib1  0.097307   0.006911  14.079  < 2e-16 ***
## born.S.Am1              0.059409   0.013967   4.253 2.12e-05 ***
## born.Eur1               0.012398   0.013014   0.953 0.340792    
## born.f.USSR1            0.009784   0.030152   0.324 0.745569    
## born.Africa1            0.060093   0.017240   3.486 0.000493 ***
## born.MidE1              0.030920   0.023429   1.320 0.186955    
## born.India.subc1        0.047192   0.016044   2.941 0.003273 ** 
## born.Asia1              0.059939   0.016279   3.682 0.000233 ***
## born.SE.Asia1           0.041278   0.014084   2.931 0.003387 ** 
## born.elsewhere1         0.036129   0.020409   1.770 0.076718 .  
## born.unknown1          -0.010154   0.026857  -0.378 0.705384    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3233 on 11346 degrees of freedom
## Multiple R-squared:  0.1488, Adjusted R-squared:  0.1467 
## F-statistic: 70.83 on 28 and 11346 DF,  p-value: < 2.2e-16
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
## Warning: contrasts dropped from factor female
## Warning: contrasts dropped from factor AfAm
## Warning: contrasts dropped from factor Asian
## Warning: contrasts dropped from factor RaceOther
## Warning: contrasts dropped from factor Hispanic
## Warning: contrasts dropped from factor educ_hs
## Warning: contrasts dropped from factor educ_smcoll
## Warning: contrasts dropped from factor educ_as
## Warning: contrasts dropped from factor educ_bach
## Warning: contrasts dropped from factor educ_adv
## Warning: contrasts dropped from factor married
## Warning: contrasts dropped from factor widowed
## Warning: contrasts dropped from factor divorc_sep
## Warning: contrasts dropped from factor Region.Midwest
## Warning: contrasts dropped from factor Region.South
## Warning: contrasts dropped from factor Region.West
## Warning: contrasts dropped from factor born.Mex.CentAm.Carib
## Warning: contrasts dropped from factor born.S.Am
## Warning: contrasts dropped from factor born.Eur
## Warning: contrasts dropped from factor born.f.USSR
## Warning: contrasts dropped from factor born.Africa
## Warning: contrasts dropped from factor born.MidE
## Warning: contrasts dropped from factor born.India.subc
## Warning: contrasts dropped from factor born.Asia
## Warning: contrasts dropped from factor born.SE.Asia
## Warning: contrasts dropped from factor born.elsewhere
## Warning: contrasts dropped from factor born.unknown
pred_model_lpm1 <- (pred_vals_lpm > 0.5)
table(pred = pred_model_lpm1, true = dat_test$NOTCOV)
##        true
## pred        0     1
##   FALSE 48068  7991
##   TRUE    229   554
# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
## 
## Call:
## glm(formula = sobj$formula, family = binomial, data = sobj$data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9721  -0.5450  -0.3543  -0.2072   2.9377  
## 
## Coefficients:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)             0.31722    0.71675   0.443 0.658071    
## Age                    -0.62360    0.03600 -17.321  < 2e-16 ***
## female1                -0.14138    0.02975  -4.752 2.02e-06 ***
## AfAm1                  -0.06063    0.04557  -1.330 0.183378    
## Asian1                 -0.09826    0.10760  -0.913 0.361136    
## RaceOther1              0.23105    0.09467   2.441 0.014664 *  
## Hispanic1               0.11685    0.05116   2.284 0.022357 *  
## educ_hs1               -0.07239    0.04143  -1.747 0.080626 .  
## educ_smcoll1           -0.27848    0.04887  -5.699 1.21e-08 ***
## educ_as1               -0.39730    0.05682  -6.992 2.71e-12 ***
## educ_bach1             -0.70071    0.05750 -12.187  < 2e-16 ***
## educ_adv1              -1.09261    0.09516 -11.482  < 2e-16 ***
## married1               -0.34980    0.03567  -9.806  < 2e-16 ***
## widowed1               -0.12601    0.10266  -1.228 0.219632    
## divorc_sep1             0.01547    0.04861   0.318 0.750232    
## Region.Midwest1         0.18033    0.05490   3.285 0.001021 ** 
## Region.South1           0.33700    0.04835   6.969 3.18e-12 ***
## Region.West1            0.13701    0.05110   2.681 0.007339 ** 
## born.Mex.CentAm.Carib1  0.58547    0.05490  10.665  < 2e-16 ***
## born.S.Am1              0.51226    0.10928   4.687 2.77e-06 ***
## born.Eur1               0.17722    0.14889   1.190 0.233923    
## born.f.USSR1            0.13706    0.37637   0.364 0.715737    
## born.Africa1            0.54163    0.14420   3.756 0.000173 ***
## born.MidE1              0.36879    0.21618   1.706 0.088015 .  
## born.India.subc1        0.55466    0.15623   3.550 0.000385 ***
## born.Asia1              0.60642    0.15271   3.971 7.16e-05 ***
## born.SE.Asia1           0.38733    0.13864   2.794 0.005209 ** 
## born.elsewhere1         0.41231    0.18114   2.276 0.022834 *  
## born.unknown1          -0.07097    0.25733  -0.276 0.782695    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 9333.7  on 11374  degrees of freedom
## Residual deviance: 7637.3  on 11346  degrees of freedom
## AIC: 7695.3
## 
## Number of Fisher Scoring iterations: 6
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
## Warning: contrasts dropped from factor female
## Warning: contrasts dropped from factor AfAm
## Warning: contrasts dropped from factor Asian
## Warning: contrasts dropped from factor RaceOther
## Warning: contrasts dropped from factor Hispanic
## Warning: contrasts dropped from factor educ_hs
## Warning: contrasts dropped from factor educ_smcoll
## Warning: contrasts dropped from factor educ_as
## Warning: contrasts dropped from factor educ_bach
## Warning: contrasts dropped from factor educ_adv
## Warning: contrasts dropped from factor married
## Warning: contrasts dropped from factor widowed
## Warning: contrasts dropped from factor divorc_sep
## Warning: contrasts dropped from factor Region.Midwest
## Warning: contrasts dropped from factor Region.South
## Warning: contrasts dropped from factor Region.West
## Warning: contrasts dropped from factor born.Mex.CentAm.Carib
## Warning: contrasts dropped from factor born.S.Am
## Warning: contrasts dropped from factor born.Eur
## Warning: contrasts dropped from factor born.f.USSR
## Warning: contrasts dropped from factor born.Africa
## Warning: contrasts dropped from factor born.MidE
## Warning: contrasts dropped from factor born.India.subc
## Warning: contrasts dropped from factor born.Asia
## Warning: contrasts dropped from factor born.SE.Asia
## Warning: contrasts dropped from factor born.elsewhere
## Warning: contrasts dropped from factor born.unknown
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$NOTCOV)
##        true
## pred        0     1
##   FALSE 47482  7243
##   TRUE    815  1302
Now I am going to run the random forest. As below.

require('randomForest')
## Loading required package: randomForest
## randomForest 4.6-14
## Type rfNews() to see new features/changes/bug fixes.
set.seed(54321)
model_randFor <- randomForest(as.factor(NOTCOV) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
print(model_randFor)
## 
## Call:
##  randomForest(formula = as.factor(NOTCOV) ~ ., data = sobj$data,      importance = TRUE, proximity = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 5
## 
##         OOB estimate of  error rate: 13.39%
## Confusion matrix:
##      0   1 class.error
## 0 9627 122   0.0125141
## 1 1401 225   0.8616236
round(importance(model_randFor),2)
##                           0     1 MeanDecreaseAccuracy MeanDecreaseGini
## Age                   25.02 42.24                47.60           261.32
## female                10.87 10.88                15.21            33.50
## AfAm                   2.68  9.57                 8.46            21.99
## Asian                 15.64 -1.46                16.33            15.54
## RaceOther              3.24  4.56                 5.10            14.33
## Hispanic              -1.00 19.29                28.23            78.78
## educ_hs               19.56 -4.81                17.43            25.20
## educ_smcoll           13.89 12.63                20.85            24.47
## educ_as               13.25 10.87                17.89            19.21
## educ_bach             15.22 26.11                28.30            32.96
## educ_adv              13.74 18.74                22.68            28.01
## married               20.66  7.05                23.89            53.78
## widowed               10.53 -3.63                 9.79            10.25
## divorc_sep            12.18 -4.75                10.13            17.98
## Region.Midwest         5.04  7.10                 9.85            19.01
## Region.South           6.99 14.19                16.35            28.68
## Region.West            4.39  7.87                13.02            22.70
## born.Mex.CentAm.Carib  3.35 44.83                35.08           122.08
## born.S.Am             -3.27 12.91                 3.53            10.71
## born.Eur              -2.11  1.64                -1.19             6.33
## born.f.USSR           -8.91 -0.69                -9.03             1.12
## born.Africa           -0.33 10.39                 4.08             8.06
## born.MidE             -5.95  7.72                -1.95             4.00
## born.India.subc       12.34 -8.03                10.84             6.75
## born.Asia              6.72  1.35                 7.29             7.99
## born.SE.Asia           7.72  3.94                 9.17             8.03
## born.elsewhere        -0.93  4.27                 0.89             6.55
## born.unknown          -5.72  1.71                -4.83             2.77
varImpPlot(model_randFor)


# look at confusion matrix for this too
pred_model1 <- predict(model_randFor,  s_dat_test)
table(pred = pred_model1, true = dat_test$NOTCOV)
##     true
## pred     0     1
##    0 47675  7342
##    1   622  1203
Support Vector Machines(SVM). Now I am going to run SVM model. As below. After the SVM model gave false positive is 46665(82.09%) and false negative is 1826(3.21%)

require(e1071)
## Loading required package: e1071
# tuned_parameters <- tune.svm(as.factor(NOTCOV) ~ ., data = sobj$data, gamma = 10^(-3:0), cost = 10^(-2:1)) 
# summary(tuned_parameters)
# figure best parameters and input into next
svm.model <- svm(as.factor(NOTCOV) ~ ., data = sobj$data, cost = 10, gamma = 0.1)
svm.pred <- predict(svm.model, s_dat_test)
table(pred = svm.pred, true = dat_test$NOTCOV)
##     true
## pred     0     1
##    0 46665  6719
##    1  1632  1826
prop.table(table(pred = svm.pred, true = dat_test$NOTCOV))
##     true
## pred          0          1
##    0 0.82095985 0.11820485
##    1 0.02871116 0.03212413
LASSO model Now I am going to run next machine learning technique’LASSO’.It combines LASSO with Ridge and the alpha parameter (from 0 to 1) determines the relative weight. Begin with alpha = 1 so just LASSO.Where after run the model I found result as below pred 0 1 FALSE 28005(False Positive) 2907 TRUE 20292 5638(False Negative)

# Elastic Net
require(glmnet)
## Loading required package: glmnet
## Loading required package: Matrix
## Loaded glmnet 4.0-2
model1_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$NOTCOV) 
# default is alpha = 1, lasso
par(mar=c(4.5,4.5,1,4))
plot(model1_elasticnet)
vnat=coef(model1_elasticnet)
vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
axis(4, at=vnat,line=-.5,label=names(sobj$data[,-1]),las=1,tick=FALSE, cex.axis=0.5) 


plot(model1_elasticnet, xvar = "lambda")


plot(model1_elasticnet, xvar = "dev", label = TRUE)


print(model1_elasticnet)
## 
## Call:  glmnet(x = as.matrix(sobj$data[, -1]), y = sobj$data$NOTCOV) 
## 
##    Df  %Dev   Lambda
## 1   0  0.00 0.089300
## 2   1  1.11 0.081370
## 3   1  2.02 0.074140
## 4   2  2.96 0.067560
## 5   2  4.13 0.061550
## 6   3  5.13 0.056090
## 7   3  5.99 0.051100
## 8   3  6.70 0.046560
## 9   4  7.37 0.042430
## 10  4  8.07 0.038660
## 11  4  8.65 0.035220
## 12  6  9.19 0.032090
## 13  6  9.83 0.029240
## 14  6 10.37 0.026650
## 15  6 10.82 0.024280
## 16  7 11.20 0.022120
## 17  7 11.52 0.020160
## 18  8 11.84 0.018370
## 19  9 12.13 0.016730
## 20  9 12.40 0.015250
## 21  9 12.62 0.013890
## 22 10 12.82 0.012660
## 23 10 13.01 0.011530
## 24 11 13.18 0.010510
## 25 12 13.33 0.009576
## 26 13 13.47 0.008725
## 27 15 13.65 0.007950
## 28 15 13.80 0.007244
## 29 15 13.93 0.006600
## 30 17 14.04 0.006014
## 31 18 14.14 0.005480
## 32 17 14.22 0.004993
## 33 18 14.30 0.004549
## 34 19 14.36 0.004145
## 35 19 14.42 0.003777
## 36 21 14.47 0.003441
## 37 21 14.52 0.003136
## 38 21 14.56 0.002857
## 39 21 14.59 0.002603
## 40 21 14.62 0.002372
## 41 22 14.64 0.002161
## 42 24 14.67 0.001969
## 43 25 14.69 0.001794
## 44 25 14.72 0.001635
## 45 25 14.74 0.001490
## 46 25 14.76 0.001357
## 47 25 14.78 0.001237
## 48 26 14.79 0.001127
## 49 27 14.80 0.001027
## 50 27 14.82 0.000936
## 51 27 14.83 0.000852
## 52 28 14.83 0.000777
## 53 28 14.84 0.000708
## 54 28 14.85 0.000645
## 55 28 14.85 0.000588
## 56 28 14.86 0.000535
## 57 28 14.86 0.000488
## 58 28 14.86 0.000445
## 59 28 14.87 0.000405
## 60 28 14.87 0.000369
## 61 28 14.87 0.000336
## 62 28 14.87 0.000306
## 63 28 14.87 0.000279
## 64 28 14.87 0.000254
## 65 28 14.87 0.000232
## 66 28 14.88 0.000211
## 67 28 14.88 0.000192
## 68 28 14.88 0.000175
## 69 28 14.88 0.000160
## 70 28 14.88 0.000146
## 71 28 14.88 0.000133
## 72 28 14.88 0.000121
## 73 28 14.88 0.000110
## 74 28 14.88 0.000100
## 75 28 14.88 0.000091
## 76 28 14.88 0.000083
cvmodel1_elasticnet = cv.glmnet(data.matrix(sobj$data[,-1]),data.matrix(sobj$data$NOTCOV)) 
cvmodel1_elasticnet$lambda.min
## [1] 0.0001100988
log(cvmodel1_elasticnet$lambda.min)
## [1] -9.114132
coef(cvmodel1_elasticnet, s = "lambda.min")
## 29 x 1 sparse Matrix of class "dgCMatrix"
##                                 1
## (Intercept)            0.92645173
## Age                   -0.05942040
## female                 0.03202783
## AfAm                   0.02095675
## Asian                  0.03115849
## RaceOther             -0.06583085
## Hispanic              -0.02971641
## educ_hs                0.02454518
## educ_smcoll            0.07257092
## educ_as                0.09595680
## educ_bach              0.13739143
## educ_adv               0.15771988
## married                0.08592967
## widowed                0.04655837
## divorc_sep             0.01540898
## Region.Midwest        -0.02982383
## Region.South          -0.06171720
## Region.West           -0.02060702
## born.Mex.CentAm.Carib -0.19468344
## born.S.Am             -0.11704168
## born.Eur              -0.02346051
## born.f.USSR           -0.01687763
## born.Africa           -0.11830008
## born.MidE             -0.05991771
## born.India.subc       -0.08918261
## born.Asia             -0.11476860
## born.SE.Asia          -0.07779885
## born.elsewhere        -0.07044978
## born.unknown           0.01839571
pred1_elasnet <- predict(model1_elasticnet, newx = data.matrix(s_dat_test), s = cvmodel1_elasticnet$lambda.min)
pred_model1_elasnet <- (pred1_elasnet < mean(pred1_elasnet)) 
table(pred = pred_model1_elasnet, true = dat_test$NOTCOV)
##        true
## pred        0     1
##   FALSE 28009  2908
##   TRUE  20288  5637
model2_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$NOTCOV, alpha = 1) 
# or try different alpha values to see if you can improve
Now I am summarising all machine learning techniques model is one place as below.

After run these all model I have found following result.Every Model gives different prediction as belwo 1. Under Ramdom Forest true pred 0 1 0 47675 7342 1 622 1203 true pred 0 1 0 0.83872841 0.12916505 1 0.01094261 0.02116393

2.Under SVM

true
pred 0 1 0 46665 6719 1 1632 1826 true pred 0 1 0 0.82095985 0.11820485 1 0.02871116 0.03212413 3.Under Elastic Net Model.

   true
pred 0 1 FALSE 28005 2907 TRUE 20292 5638 true pred 0 1 FALSE 0.49268147 0.05114176 TRUE 0.35698955 0.09918722

#Random Forest
table(pred = pred_model1, true = dat_test$NOTCOV)
##     true
## pred     0     1
##    0 47675  7342
##    1   622  1203
prop.table(table(pred = pred_model1, true = dat_test$NOTCOV))
##     true
## pred          0          1
##    0 0.83872841 0.12916505
##    1 0.01094261 0.02116393
#Support Vector Machines
table(pred = svm.pred, true = dat_test$NOTCOV)
##     true
## pred     0     1
##    0 46665  6719
##    1  1632  1826
prop.table(table(pred = svm.pred, true = dat_test$NOTCOV))
##     true
## pred          0          1
##    0 0.82095985 0.11820485
##    1 0.02871116 0.03212413
#Elastic Net
table(pred = pred_model1_elasnet, true = dat_test$NOTCOV)
##        true
## pred        0     1
##   FALSE 28009  2908
##   TRUE  20288  5637
prop.table(table(pred = pred_model1_elasnet, true = dat_test$NOTCOV))
##        true
## pred             0          1
##   FALSE 0.49275184 0.05115935
##   TRUE  0.35691918 0.09916963
