HE = read.csv("HealthExpend (Modified).csv", header = T, na.strings = "?")
attach(HE)

summary(HE)
# Determine variable types
str(HE)
# Coerce categorical variables to factors
factors = c("ANYLIMIT", "COLLEGE", "HIGHSCH", "GENDER", "MNHPOOR", "INSURE", "USC", "UNEMPLOY", "MANAGEDCARE")
HE[factors] = lapply(HE[factors], factor)
# Confirm type coercion
str(HE)

install.packages("ggplot2")
library(ggplot2)

plotFactors = function(f.data, f.x, f.y, f.factors, f.size = 2) {
  if (!require(ggplot2)) { return("Could not load 'ggplot2' package") }
  plot = ggplot(f.data, aes_string(x = f.x, y = f.y))

  for (i in f.factors) {
    print(plot + geom_point(aes_string(color = i), size = f.size))
  }
}

NonZeroExpIP = HE[HE$EXPENDIP > 0,]

plotFactors(NonZeroExpIP, "EXPENDIP", "AGE", list("INCOME", "GENDER", "MARISTAT", "REGION", "factor(FAMSIZE)"))
# Single data-point massively skewing dataset; identify via diagnostic plots

lm.fit = lm(EXPENDIP ~ ., data = HE)
par(mfrow = c(2, 2))
plot(lm.fit)

# Observation #733 causing extreme problems based on diagnostic plots
HE[733,]

# Remove outlier
HE = HE[-733,]

# Re-plot factors
NonZeroExpIP = HE[HE$EXPENDIP > 0,]

plotFactors(NonZeroExpIP, "AGE", "EXPENDIP", list("INCOME", "GENDER", "MARISTAT", "REGION", "factor(FAMSIZE)"))
plotFactors(NonZeroExpIP, "AGE", "log10(EXPENDIP)", list("INCOME", "GENDER", "MARISTAT", "REGION", "factor(FAMSIZE)"))
plotFactors(NonZeroExpIP, "log10(AGE)", "EXPENDIP", list("INCOME", "GENDER", "MARISTAT", "REGION", "factor(FAMSIZE)"))

# BEGIN - Multi-linear regression of EXPENDIP without EXPENDOP
  lm.fit = lm(EXPENDIP ~ . -EXPENDOP, data = HE)
  summary(lm.fit)

  # EDUC returning NA values--signifies collinearity; check which variables are conflicting
  alias(lm.fit)

  # Remove EDUC because of perfect collinearity with HIGHSCH and COLLEGE
  lm.fit = update(lm.fit, ~ . -EDUC)
  summary(lm.fit)

  # BEGIN - Backwards selection
    lm.fit = update(lm.fit, ~ . -HIGHSCH)
    summary(lm.fit)
    lm.fit = update(lm.fit, ~ . -INSURE)
    summary(lm.fit)
    lm.fit = update(lm.fit, ~ . -FAMSIZE)
    summary(lm.fit)
    lm.fit = update(lm.fit, ~ . -INDUSCLASS)
    summary(lm.fit)
    lm.fit = update(lm.fit, ~ . -UNEMPLOY)
    summary(lm.fit)
    lm.fit = update(lm.fit, ~ . -USC)
    summary(lm.fit)
    lm.fit = update(lm.fit, ~ . -RACE)
    summary(lm.fit)
    # Removing RACE surprisingly increases ADJR2
    lm.fit = update(lm.fit, ~ . -MNHPOOR)
    summary(lm.fit)
    # Removing MNHPOOR has negligble positive effect on p-values only
    # Removing ANYLIMIT, COLLEGE, MANAGEDCARE, MARISTAT, PHSTAT, INCOME, or REGION lower ADJR2 at this point
  # END

  # Optimal model reached with manual backwards selection:
  lm.fit = lm(EXPENDIP ~ . -EXPENDOP -EDUC -HIGHSCH -INSURE -FAMSIZE -INDUSCLASS -UNEMPLOY -USC -RACE -MNHPOOR, data = HE)
  summary(lm.fit)

  # BEGIN - Interaction terms
    lm.fit = update(lm.fit, ~ . +MARISTAT:INCOME)
    summary(lm.fit)
    lm.fit = update(lm.fit, ~ . +PHSTAT:INCOME)
    summary(lm.fit)
    # PHSTAT:MARISTAT has a negligible increase
    # REGION:INCOME, REGION:COLLEGE, AGE:INCOME, AGE:GENDER, MARISTAT:GENDER, ANYLIMIT:MANAGEDCARE each decrease ADJR2
  # END

  # Optimal model reached with interaction terms:
  lm.fit = lm(EXPENDIP ~ . -EXPENDOP -EDUC -HIGHSCH -INSURE -FAMSIZE -INDUSCLASS -UNEMPLOY -USC -RACE -MNHPOOR +MARISTAT:INCOME +PHSTAT:INCOME, data = HE)
  summary(lm.fit)

  plot(glm.fit)

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 2504.142 in USD

    # VIF for collinearity:
    library(car)
    vif(lm.fit)
    # No issues of multi-collinearity (all below 5)

    # LOOCV
    library(boot)
    glm.fit = glm(EXPENDIP ~ . -EXPENDOP -EDUC -HIGHSCH -INSURE -FAMSIZE -INDUSCLASS -UNEMPLOY -USC -RACE -MNHPOOR +MARISTAT:INCOME +PHSTAT:INCOME, data = HE)
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 2699.467 (default), 2699.416 (bias corrected) in USD

    # K-Fold CV at K = 5
    cv.err = cv.glm(HE, glm.fit, K = 5)
    sqrt(cv.err$delta)
    # Test RMSE = 2677.66 (default), 2657.80 (bias corrected) in USD [NO SEED SET]

    # K-Fold CV at K = 10
    cv.err = cv.glm(HE, glm.fit, K = 10)
    sqrt(cv.err$delta)
    # Test RMSE = 2711.632 (default), 2700.470 (bias corrected) in USD [NO SEED SET]

    # RMSE lowest in K-Fold (5) at 2657.80 on original run with no seed set. After repeating with different seeds,
    # RMSE varies by siginificant amounts for K-Fold CV. Therefore, LOOCV will be used for comparison only to
    # achieve the most accurate and consistent results.
  # END
# END

# BEGIN - Model selection using stepAIC
  library(MASS)

  lm.fit = lm(EXPENDIP ~ . -EXPENDOP -EDUC, data = HE)
  step = stepAIC(lm.fit, direction = "backward")
  step$anova

  lm.fit = lm(EXPENDIP ~ . -EXPENDOP -EDUC, data = HE)
  step = stepAIC(lm.fit, direction = "both")
  step$anova

  # Optimal model achieved in both backward-stepwise and both-stepwise:
  lm.fit = lm(EXPENDIP ~ AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT, data = HE)
  summary(lm.fit)

  # Refitting the original model with interaction terms causes excessively long processing time in R due to
  # single-core restrictions. Parallel processing not recommended / infeasible for stepwise selection. Instead,
  # interaction terms are fitted to the optimal base model achieved above.
  lm.fit = lm(EXPENDIP ~ (AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT)^2, data = HE)
  step = stepAIC(lm.fit, direction = "backward")
  # Processing takes less than two seconds to find optimal model with interaction terms
  step$anova

  sum = summary(lm.fit)
  sum$adj.r.squared
  sum$fstatistic
  sum$sigma
  # Significant increase in ADJR2

  # StepAIC with direction = "both"
  lm.fit = lm(EXPENDIP ~ (AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT)^2, data = HE)
  step = stepAIC(lm.fit, direction = "both")
  step$anova

  sum = summary(lm.fit)
  sum$adj.r.squared
  sum$fstatistic
  sum$sigma
  # Results are identical to backward-stepwise model with interaction terms

  # Optimal model achieved through two-term interaction stepAIC in both backward and both directions:
  lm.fit = lm(EXPENDIP ~ AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT + AGE:GENDER + AGE:COUNTIP +
                  AGE:COUNTOP + GENDER:COUNTIP + COUNTIP:COUNTOP + COUNTIP:INCOME + COUNTIP:PHSTAT +
                  COUNTOP:INCOME + COUNTOP:PHSTAT + INCOME:PHSTAT, data = HE)
  summary(lm.fit)

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 2169.016 in USD

    # VIF for collinearity:
    library(car)
    vif(lm.fit)
    # 3 variables above cut-off of 5, which suggests multicollinearity; however, one of these variables is an
    # interaction term of AGE:COUNTOP, and interaction terms on a highly significant predictor will naturally
    # have higher indexes for multicollinearity as shown by the interactions on AGE versus the interactions on
    # the other predictors. The other two predictors with an index higher than 5 are COUNTIP and COUNTOP, which
    # are known to be highly correlated to the response variables from the beginning; this is acceptable as they
    # are logically correlate regardless of the model fit. Therefore, there are no issues of multicollinearity.

    # LOOCV
    library(boot)
    glm.fit = glm(EXPENDIP ~ AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT + AGE:GENDER + AGE:COUNTIP +
                    AGE:COUNTOP + GENDER:COUNTIP + COUNTIP:COUNTOP + COUNTIP:INCOME + COUNTIP:PHSTAT +
                    COUNTOP:INCOME + COUNTOP:PHSTAT + INCOME:PHSTAT, data = HE)
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 2594.489 (default), 2594.370 (bias corrected) in USD
    # RMSE ower than manual model RMSE of 2657.80
  # END

  # Fit model with three-term interactions
  lm.fit = lm(EXPENDIP ~ (AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT)^3, data = HE)
  step = stepAIC(lm.fit, direction = "both")
  step$anova

  sum = summary(lm.fit)
  sum$adj.r.squared
  sum$fstatistic
  sum$sigma
  # ADJR2 significantly increased over two-term interaction model. However, model includes too many interaction
  # terms to feasibly interpret. RSE has also decreased, but so have the degrees of freedom. Model may also be
  # overfitted and F-Stat has decreased (as well as the DF).

  # Optimal model achieved through three-term interaction stepAIC on both directions:
  lm.fit = lm(EXPENDIP ~ AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT +
                  AGE:GENDER + AGE:COUNTIP + AGE:COUNTOP + AGE:INCOME + AGE:PHSTAT +
                  GENDER:COUNTIP + GENDER:COUNTOP + GENDER:INCOME + GENDER:PHSTAT +
                  COUNTIP:COUNTOP + COUNTIP:INCOME + COUNTIP:PHSTAT + COUNTOP:INCOME +
                  COUNTOP:PHSTAT + INCOME:PHSTAT + AGE:GENDER:COUNTIP + AGE:GENDER:COUNTOP +
                  AGE:COUNTIP:COUNTOP + AGE:COUNTIP:INCOME + AGE:COUNTIP:PHSTAT +
                  AGE:COUNTOP:INCOME + AGE:COUNTOP:PHSTAT + GENDER:COUNTIP:COUNTOP +
                  GENDER:COUNTIP:INCOME + GENDER:COUNTIP:PHSTAT + GENDER:COUNTOP:INCOME +
                  GENDER:COUNTOP:PHSTAT + COUNTIP:COUNTOP:INCOME + COUNTIP:COUNTOP:PHSTAT +
                  COUNTIP:INCOME:PHSTAT + COUNTOP:INCOME:PHSTAT)
  summary(lm.fit)

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 1832.066 in USD

    # Collinearity not tested because of three-term interaction terms; will cause excessive amount of false
    # positives for multicollinearity. Additionally, VIF cannot be used on the model as there are aliased
    # coefficients in the model.

    # LOOCV
    library(boot)
    glm.fit = glm(EXPENDIP ~ AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT +
                    AGE:GENDER + AGE:COUNTIP + AGE:COUNTOP + AGE:INCOME + AGE:PHSTAT +
                    GENDER:COUNTIP + GENDER:COUNTOP + GENDER:INCOME + GENDER:PHSTAT +
                    COUNTIP:COUNTOP + COUNTIP:INCOME + COUNTIP:PHSTAT + COUNTOP:INCOME +
                    COUNTOP:PHSTAT + INCOME:PHSTAT + AGE:GENDER:COUNTIP + AGE:GENDER:COUNTOP +
                    AGE:COUNTIP:COUNTOP + AGE:COUNTIP:INCOME + AGE:COUNTIP:PHSTAT +
                    AGE:COUNTOP:INCOME + AGE:COUNTOP:PHSTAT + GENDER:COUNTIP:COUNTOP +
                    GENDER:COUNTIP:INCOME + GENDER:COUNTIP:PHSTAT + GENDER:COUNTOP:INCOME +
                    GENDER:COUNTOP:PHSTAT + COUNTIP:COUNTOP:INCOME + COUNTIP:COUNTOP:PHSTAT +
                    COUNTIP:INCOME:PHSTAT + COUNTOP:INCOME:PHSTAT)
    cv.err = cv.glm(HE, glm.fit)
    # Warning messages of rank defficiency indicate same issue of perfect correlation mentioned above with VIF.
    alias(glm.fit)
    # COUNTIP:INCOMENPOOR:PHSTATVGOO perfectly correlated with

    sqrt(cv.err$delta)
    # Test RMSE = 14551.675 (default), 3665.691 (bias corrected) in USD
    # RMSE worse than both previous models; indicates, along with previous warnings and errors, that the model
    # is overfit to the training data and cannot feasibly make predictions.

    # Remove problematic interaction-term to see if the model improves enough to use
    glm.fit = update(glm.fit, ~ . -COUNTIP:INCOME:PHSTAT)
    alias(glm.fit)
    # Perfect correlation issues cleared from alis results
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 14515.979 (default), 3605.982 (bias corrected) in USD
    # RMSE marginally improves after removing COUNTIP:INCOME:PHSTAT interaction term; however, model still highly
    # inaccurate compared to optimal models achieved through manual stepwise and stepAIC.
  # END

  # Conclusion: Optimal model achieved above via stepAIC with two-term interactions.
# END

# BEGIN - Failed attempt at using regsubsets of "leaps" package for finding optimal regression model
  # install.packages("leaps")
  # library(leaps)
  #
  # reg.fit = regsubsets(EXPENDIP ~ . -EXPENDOP -EDUC, data = HE)
  # reg.sum = summary(reg.fit)
  #
  # reg.fit.max = regsubsets(EXPENDIP ~ . -EXPENDOP -EDUC, data = HE, nvmax = 22)
  # reg.sum.max = summary(reg.fit.max)
  #
  # par(mfrow = c(2, 2))
  # plot(reg.sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
  # plot(reg.sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R-Sq", type = "l")
  # max = which.max(reg.sum$adjr2)
  # points(max, reg.sum$adjr2[max], col = "red", cex = 2, pch = 20)
  #
  # plot(reg.sum.max$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
  # plot(reg.sum.max$adjr2, xlab = "Number of Variables", ylab = "Adjusted R-Sq", type = "l")
  #
  # plot(reg.fit.max, scale = "r2")
  # plot(reg.fit.max, scale = "adjr2")
  # plot(reg.fit.max, scale = "Cp")
  # plot(reg.fit.max, scale = "bic")
  #
  # reg.fit.bwd = regsubsets(EXPENDIP ~ . -EXPENDOP -EDUC, data = HE, nvmax = 22, method = "backward")
  # summary(reg.fit.bwd)
# END

# pairs(HE, upper.panel = NULL, pch = 19, main = "Scatterplot Matrix of 'HE'", row1attop = F, lower.panel = panel.smooth)

# BEGIN - Hierarchical Clustering - doesn't work
  # HE1 = HE[c("AGE", "FAMSIZE", "COUNTIP", "EXPENDIP", "COUNTOP", "EXPENDOP")]
  # HE1.scl = scale(HE1, center = F)
  # hclus.comp = hclust(dist(HE1.scl), method = "average")
  # plot(hclus.comp, main = "Complete Linkage", xlab = "", sub = "", cex = .9)
# END
