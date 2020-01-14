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

plotFactors = function(f.data, f.x, f.y, f.factors, f.type = "scatter", f.size = 2, f.save = F) {
  if (!require(ggplot2)) { return("Could not load 'ggplot2' package") }
  for (f.x_i in f.x) {
    plot = ggplot(f.data, aes_string(x = f.x_i, y = f.y))

    for (f.f_i in f.factors) {
      if (f.type == "scatter") { print(plot + geom_point(aes_string(color = f.f_i), size = f.size)) }
      else if (f.type == "box") { print(plot + geom_boxplot(aes_string(color = f.f_i), size = f.size)) }
      if (f.save == T) {
        folder = paste(f.y, "vs.", f.x_i)
        if (!dir.exists(file.path("Images/", folder))) { dir.create(file.path("Images/", folder)) }
        ggsave(paste(f.f_i, ".jpg", sep = ""), path = paste("Images/", folder, sep = ""))
      }
    }
  }
}

NonZeroExpIP = HE[HE$EXPENDIP > 0,]
factors = list("INCOME", "GENDER", "MARISTAT", "REGION", "factor(FAMSIZE)")

plotFactors(f.data = NonZeroExpIP, f.x = list("AGE"), f.y = "EXPENDIP", f.factors = factors)
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

plotFactors(f.data = NonZeroExpIP, f.x = list("AGE"), f.y = "EXPENDIP", f.factors = factors, f.save = T)
plotFactors(f.data = NonZeroExpIP, f.x = list("AGE"), f.y = "log10(EXPENDIP)", f.factors = factors, f.save = T)
plotFactors(f.data = NonZeroExpIP, f.x = factors, f.y = "EXPENDIP", f.factors = factors, f.type = "box",
            f.size = 0.6, f.save = T)


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
  lm.fit = stepAIC(lm.fit, direction = "backward")
  lm.fit$anova

  lm.fit = lm(EXPENDIP ~ . -EXPENDOP -EDUC, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Test with EDUC added back into the model and COLLEGE and HIGHSCH removed instead. EDUC has an additional level
  # for comparisons below HIGHSCH labeled as LHIGHSCH; this level could potentially unearth correlations where
  # COLLEGE and HIGHSCH were not deemed key predictors by stepAIC.
  lm.fit = lm(EXPENDIP ~ . -EXPENDOP -COLLEGE -HIGHSCH, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova
  # The final model achieved is identical to the models achieved above with EDUC removed and COLLEGE and HIGHSCH
  # included in the model. This confirms that education level has no significant bearing on EXPENDIP.

  # Optimal model achieved in both backward-stepwise and both-stepwise:
  lm.fit = lm(EXPENDIP ~ AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT, data = HE)
  summary(lm.fit)

  # The following code can take upwards of 20 minutes to run.
  lm.fit = lm(EXPENDIP ~ (. -EXPENDOP -COLLEGE -HIGHSCH)^2, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "backward")
  # Refitting the original model with interaction terms causes excessively long processing time in R due to
  # single-core restrictions. Parallel processing not recommended / infeasible for stepwise selection.
  lm.fit$anova

  # Optimal model achieved from two-term interaction stepAIC on full set of predictors:
  summary(lm.fit)
  # Extreme increase in ADJR2 from 0.5216 on previous optimal model to 0.7758. RSE also drastically reduced
  # from 2569 on 1986 DF to 1758 on 1747 DF. However, F-stat reduced from 182.5 on 12 and 1986 DF to 28.54
  # on 251 and 1747 DF. Model is now very difficult to interpret as well, but there is some newfound knowledge
  # of correlations obtained from the surface. Fitting models without COUNTIP and COUNTOP may be very worthwhile.

  alias(lm.fit)
  # There are also singularities in the model revolving around the dummy variable INDUSCLASSMILITARY, which is
  # another correlation worth looking into in an isolated context as well as worth removing the affected
  # interaction terms from the model for refitting.
  lm.fit = update(lm.fit, ~ . -COUNTIP:INDUSCLASS)
  summary(lm.fit)
  # ADJR2 reduced from 0.7758 to 0.7637. RSE increased from 1758 on 1747 DF to 1805 on 1757 DF. F-stat reduced
  # from 28.54 on 251 and 1747 DF to 27.79 on 241 and 1757 DF.
  lm.fit = update(lm.fit, ~ . -MNHPOOR:INDUSCLASS)
  summary(lm.fit)
  # ADJR2 reduced from 0.7637 to 0.7592. RSE increased from 1805 on 1757 DF to 1822 on 1767 DF. F-stat increased
  # from 27.79 on 241 and 1757 DF to 28.28 on 231 and 1767 DF.
  lm.fit = update(lm.fit, ~ . -PHSTAT:INDUSCLASS)
  summary(lm.fit)
  # ADJR2 reduced from 0.7592 to 0.7405. RSE increased from 1822 on 1767 DF to 1892 on 1806 DF. F-stat increased
  # from 28.28 on 231 and 1767 DF to 30.69 on 192 and 1806 DF.
  alias(lm.fit)
  # Singularities have been removed as confirmed by summary and aliases.

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 1798.043 in USD

    # VIF for collinearity:
    library(car)
    vif(lm.fit)
    # Multicollinearity overall non-issue. COUNTIP and COUNTOP highly correlated as well as a few interaction
    # terms involving them; as discovered above, fitting regressions without one or both of these predictors
    # may yield more interesting / accurate results. Aside from these predictors, there are only two others
    # above 5 (MNHPOOR at 8.259436 and UNEMPLOY at 6.180528). These predictors will be left in the model as
    # a cut-off of 5 is rather low for a model with this many predictors; a more reasonable, and still common,
    # cut-off of 10 will be used instead. Therefore, there are no significant issues of multi-collinearity.

    # LOOCV
    library(boot)
    glm.fit = glm(EXPENDIP ~ AGE + ANYLIMIT + GENDER + MNHPOOR + INSURE + USC +
                    UNEMPLOY + MANAGEDCARE + FAMSIZE + COUNTIP + COUNTOP + RACE +
                    REGION + EDUC + MARISTAT + INCOME + PHSTAT + INDUSCLASS +
                    AGE:MNHPOOR + AGE:USC + AGE:COUNTIP + AGE:COUNTOP + ANYLIMIT:GENDER +
                    ANYLIMIT:MNHPOOR + ANYLIMIT:INSURE + ANYLIMIT:MANAGEDCARE +
                    ANYLIMIT:FAMSIZE + ANYLIMIT:COUNTIP + ANYLIMIT:COUNTOP +
                    ANYLIMIT:EDUC + ANYLIMIT:MARISTAT + GENDER:COUNTIP + MNHPOOR:UNEMPLOY +
                    MNHPOOR:FAMSIZE + MNHPOOR:COUNTOP + MNHPOOR:REGION + MNHPOOR:MARISTAT +
                    MNHPOOR:PHSTAT + MNHPOOR:INDUSCLASS + INSURE:COUNTIP + INSURE:COUNTOP +
                    INSURE:PHSTAT + USC:UNEMPLOY + USC:COUNTIP + USC:COUNTOP +
                    UNEMPLOY:COUNTIP + UNEMPLOY:COUNTOP + UNEMPLOY:REGION + UNEMPLOY:MARISTAT +
                    UNEMPLOY:INCOME + UNEMPLOY:PHSTAT + MANAGEDCARE:COUNTOP +
                    FAMSIZE:COUNTIP + FAMSIZE:MARISTAT + COUNTIP:COUNTOP + COUNTIP:RACE +
                    COUNTIP:REGION + COUNTIP:EDUC + COUNTIP:MARISTAT + COUNTIP:INCOME +
                    COUNTIP:PHSTAT + COUNTIP:INDUSCLASS + COUNTOP:RACE + COUNTOP:REGION +
                    COUNTOP:MARISTAT + COUNTOP:INCOME + COUNTOP:PHSTAT + COUNTOP:INDUSCLASS +
                    REGION:PHSTAT + MARISTAT:INCOME + INCOME:PHSTAT + PHSTAT:INDUSCLASS, data = HE)
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 2979.982 (default), 2979.591 (bias corrected) in USD
    # RMSE higher than manual model RMSE of 2657.80
  # END

  # Due to a significantly increased RMSE, the model will be refit with two-term interactions on the optimal
  # base model achieved above instead. This will also greatly reduce model complexity and processing time.
  lm.fit = lm(EXPENDIP ~ (AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT)^2, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "backward")
  # Processing takes less than two seconds to find optimal model with interaction terms
  lm.fit$anova
  summary(lm.fit)
  # Increase in ADJR2 from manual model, but decrease from overfitted complex model.

  # StepAIC with direction = "both"
  lm.fit = lm(EXPENDIP ~ (AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT)^2, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova
  summary(lm.fit)
  # Results are identical to backward-stepwise model with interaction terms.

  # Optimal model achieved through two-term interaction stepAIC in backward and both directions:
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
    # RMSE lower than manual model RMSE of 2657.80
  # END

  # Fit model with three-term interactions
  lm.fit = lm(EXPENDIP ~ (AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT)^3, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova
  # Optimal model achieved through three-term interaction stepAIC on both directions:
  summary(lm.fit)
  # ADJR2 significantly increased over two-term interaction model. However, model includes too many interaction
  # terms to feasibly interpret. RSE has also decreased, but so have the degrees of freedom. Model may also be
  # overfitted and F-Stat has decreased (as well as the DF).

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
    # Perfect correlation issues cleared from alias results
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 14515.979 (default), 3605.982 (bias corrected) in USD
    # RMSE marginally improves after removing COUNTIP:INCOME:PHSTAT interaction term; however, model still highly
    # inaccurate compared to optimal models achieved through manual stepwise and stepAIC.
  # END

  # Conclusion: Optimal model achieved via stepAIC with two-term interactions after achieving optimal base model.
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
