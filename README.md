
LAB7
---
## title: "LAB7"
## output: github_document
## author: "dawa lama"
## date: "11/10/2020"
## Econ B2000, MA Econometrics
## Kevin R Foster, the Colin Powell School at the City College of New York, CUNY
## Fall 2020

---

```{r}
load("C:/Users/singh/OneDrive/Desktop/project new/lab7/NHIS_2014.RData")
```


Predicting Health Insurance Status 
Predicting the People that have health insurance coverage and do not have any health insurance coverage available by using different model. 


IN this lab, I will estimate a variety of models to try to predict if a person has health insurance. Start with logit like last week then some different models: Random Forest, Support Vector Machines, and Elastic Net (which is not optimal for a 0/1 dependent but it works for a demonstration) via data set NHIS. Here I am going to include the age between 18 to 75.

I run the code and than gae me result as below.

I found age between 25-75
56842= Cover/who has a health insurance(83.33% out of 100% )
11375= No of people not cover by health insurance(16.67%) 


```{r}
data_use1$earn_lastyr <- as.factor(data_use1$ERNYR_P)
levels(data_use1$earn_lastyr) <- c("0","$01-$4999","$5000-$9999","$10000-$14999","$15000-$19999","$20000-$24999","$25000-$34999","$35000-$44999","$45000-$54999","$55000-$64999","$65000-$74999","$75000 and over",NA,NA,NA)
```

First decide on how you're defining your subgroup (all adults? Within certain age? Other?) then find some basic statistics -- what fraction are not covered? (Later go back to look at simple stats for subgroups to see if there are sharp differences.)

```{r}
dat2 <- subset(data_use1, (AGE_P >= 25) & (AGE_P <= 75))
covprop <- dat2$NOTCOV
table(covprop)
```



I set the subset age between age 25-75, which gave the result below-

I run a logit regression.As below.And which gave the resilt below.
```{r}
model_logit1 <- glm(NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + RaceOther  
                    + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
                    + married + widowed + divorc_sep + veteran_stat + REGION + region_born,
                    family = binomial, data = dat2)
summary(model_logit1)
```
First I am going to change the data_use1 and give name dat2, and I run the code as below.
```{r}
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

```
Now I am going to create standardize analysis, which is blow.As question said to try the max value 0.75, I have try with 0.6.  Summary(restrict_1) has gave me result as below, where we can see 56482 is cover by health Insurance and 11375 is not cover by health isurance.
```{r}
require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$NOTCOV)
restrict_1 <- as.logical(round(runif(NN,min=0,max=0.6))) # use fraction as training data
#runif(<0.6)
#we can bump this down depending on how fast our computer is
#round a number from the uniform distribution, a number between 1 and 0.6 (logical function 0 wi)
summary(restrict_1)
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
```
OSL or Linear Probability Model.
First I am going to run theLinear Probability Model (ie good ol' OLS) and logit. As below


```{r}
# LPM
model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > 0.5)
table(pred = pred_model_lpm1, true = dat_test$NOTCOV)
# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$NOTCOV)
```
Now I am going to run the random forest. As below.

```{r}
require('randomForest')
set.seed(54321)
model_randFor <- randomForest(as.factor(NOTCOV) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
print(model_randFor)
round(importance(model_randFor),2)
varImpPlot(model_randFor)
# look at confusion matrix for this too
pred_model1 <- predict(model_randFor,  s_dat_test)
table(pred = pred_model1, true = dat_test$NOTCOV)
```

Support Vector Machines(SVM).
Now I am going to run SVM model. As below.
After the SVM model gave false positive is 46665(82.09%) and false negative is 1826(3.21%)

```{r}
require(e1071)
# tuned_parameters <- tune.svm(as.factor(NOTCOV) ~ ., data = sobj$data, gamma = 10^(-3:0), cost = 10^(-2:1)) 
# summary(tuned_parameters)
# figure best parameters and input into next
svm.model <- svm(as.factor(NOTCOV) ~ ., data = sobj$data, cost = 10, gamma = 0.1)
svm.pred <- predict(svm.model, s_dat_test)
table(pred = svm.pred, true = dat_test$NOTCOV)
prop.table(table(pred = svm.pred, true = dat_test$NOTCOV))
```
LASSO model
Now I am going to run next machine learning technique'LASSO'.It combines LASSO with Ridge and the alpha parameter (from 0 to 1) determines the relative weight. Begin with alpha = 1 so just LASSO.Where after run the model I found result as below
pred        0                   1
  FALSE 28005(False Positive)  2907
  TRUE  20292                  5638(False Negative)

```{r}
# Elastic Net
require(glmnet)
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
cvmodel1_elasticnet = cv.glmnet(data.matrix(sobj$data[,-1]),data.matrix(sobj$data$NOTCOV)) 
cvmodel1_elasticnet$lambda.min
log(cvmodel1_elasticnet$lambda.min)
coef(cvmodel1_elasticnet, s = "lambda.min")
pred1_elasnet <- predict(model1_elasticnet, newx = data.matrix(s_dat_test), s = cvmodel1_elasticnet$lambda.min)
pred_model1_elasnet <- (pred1_elasnet < mean(pred1_elasnet)) 
table(pred = pred_model1_elasnet, true = dat_test$NOTCOV)
model2_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$NOTCOV, alpha = 1) 
# or try different alpha values to see if you can improve
```
Now I am summarising all machine learning techniques model is one place as below.


```{r}
#Random Forest
table(pred = pred_model1, true = dat_test$NOTCOV)
prop.table(table(pred = pred_model1, true = dat_test$NOTCOV))
#Support Vector Machines
table(pred = svm.pred, true = dat_test$NOTCOV)
prop.table(table(pred = svm.pred, true = dat_test$NOTCOV))
#Elastic Net
table(pred = pred_model1_elasnet, true = dat_test$NOTCOV)
prop.table(table(pred = pred_model1_elasnet, true = dat_test$NOTCOV))
```
