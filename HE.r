packageLoad = function(packages) {
  installed = installed.packages()[, "Package"]
  for (p in packages) {
    if (!(p %in% installed)) { install.packages(p) }
    library(p, character.only = T)
  }
}

packageLoad(c("ggplot2", "car", "boot", "MASS"))

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

plotFactors = function(f.data, f.x, f.y, f.factors, f.type = "scatter", f.size = 2, f.save = F) {
  if (!require(ggplot2)) { return("Could not load 'ggplot2' package") }
  if (!dir.exists("Images")) { dir.create("Images") }
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
    vif(lm.fit)
    # No issues of multi-collinearity (all below 5)

    # LOOCV
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

  # BEGIN - Model selection using stepAIC
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
    # ADJR2 = 0.5216, RSE = 2569 on 1986 DF, F-stat = 182.5 on 12 and 1986 DF.

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
      vif(lm.fit)
      # Multicollinearity overall non-issue. COUNTIP and COUNTOP highly correlated as well as a few interaction
      # terms involving them; as discovered above, fitting regressions without one or both of these predictors
      # may yield more interesting / accurate results. Aside from these predictors, there are only two others
      # above 5 (MNHPOOR at 8.259436 and UNEMPLOY at 6.180528). These predictors will be left in the model as
      # a cut-off of 5 is rather low for a model with this many predictors; a more reasonable, and still common,
      # cut-off of 10 will be used instead. Therefore, there are no significant issues of multi-collinearity.

      # LOOCV
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
    # ADJR2 = 0.6501, RSE = 2197 on 1949 DF, F-stat = 76.75 on 49 and 1949 DF. Signifcant improvement in
    # model after fitting with two-term interactions.

    # BEGIN - Validation testing
      # Estimated training RMSE:
      sqrt(mean(lm.fit$residuals^2))
      # RMSE = 2169.016 in USD

      # VIF for collinearity:
      vif(lm.fit)
      # 3 variables above cut-off of 5, which suggests multicollinearity; however, one of these variables is an
      # interaction term of AGE:COUNTOP, and interaction terms on a highly significant predictor will naturally
      # have higher indexes for multicollinearity as shown by the interactions on AGE versus the interactions on
      # the other predictors. The other two predictors with an index higher than 5 are COUNTIP and COUNTOP, which
      # are known to be highly correlated to the response variables from the beginning; this is acceptable as they
      # are logically correlate regardless of the model fit. Therefore, there are no issues of multicollinearity.

      # LOOCV
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
  # END
  # Conclusion: Optimal model achieved via stepAIC with two-term interactions after achieving optimal base model.
# END

# BEGIN - Multi-linear regression of EXPENDIP without EXPENDOP, COUNTOP, and COUNTIP
  # Based on findings above, the initial model must have COLLEGE and HIGHSCH removed in all models.
  # The stepAIC function will be used with direction "both" to achieve an optimal base model, and
  # two-term interactions will be fitted to that model.
  lm.fit = lm(EXPENDIP ~ . -EXPENDOP -COUNTOP -COUNTIP -COLLEGE -HIGHSCH, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Optimal model achieved:
  lm.fit = lm(EXPENDIP ~ AGE + ANYLIMIT + INSURE + UNEMPLOY + PHSTAT, data = HE)
  summary(lm.fit)
  # ADJR2 = 0.07339, RSE = 3575 on 1990 DF, F-stat = 20.78 on 8 and 1990 DF. Model completely unfit for use.

  lm.fit = lm(EXPENDIP ~ (AGE + ANYLIMIT + INSURE + UNEMPLOY + PHSTAT)^2, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Optimal model achieved with two-term interactions:
  lm.fit = lm(EXPENDIP ~ AGE + ANYLIMIT + INSURE + UNEMPLOY + PHSTAT + AGE:ANYLIMIT + INSURE:PHSTAT, data = HE)
  summary(lm.fit)
  # ADJR2 = 0.09041, RSE = 3542 on 1985 DF, F-stat = 16.28 on 13 and 1985 DF. No practical improvement in
  # model after fitting with two-term interactions.

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 3529.186 in USD

    # VIF for collinearity:
    vif(lm.fit)
    # No issues of multi-collinearity

    # LOOCV
    glm.fit = glm(EXPENDIP ~ AGE + ANYLIMIT + INSURE + UNEMPLOY + PHSTAT + AGE:ANYLIMIT + INSURE:PHSTAT, data = HE)
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 3565.803 (default), 3565.794 (bias corrected) in USD
  # END

  # Conclusion: Model of EXPENDIP without EXPENDOP, COUNTOP, and COUNTIP significantly worse than optimal
  # model achieved above of EXPENDIP with COUNTOP and COUNTIP included.
# END

# BEGIN - Multi-linear regression of EXPENDIP without EXPENDOP, COUNTOP
  lm.fit = lm(EXPENDIP ~ . -EXPENDOP -COUNTOP -COLLEGE -HIGHSCH, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Optimal model achieved:
  lm.fit = lm(EXPENDIP ~ AGE + COUNTIP + INCOME + PHSTAT, data = HE)
  summary(lm.fit)
  # ADJR2 = 0.4989, RSE = 2629 on 1988 DF, F-stat = 199.98 on 10 and 1988 DF. Model comparable to manual
  # model tested above.

  lm.fit = lm(EXPENDIP ~ (AGE + COUNTIP + INCOME + PHSTAT)^2, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Optimal model achieved with two-term interactions:
  lm.fit = lm(EXPENDIP ~ AGE + COUNTIP + INCOME + PHSTAT + AGE:COUNTIP + COUNTIP:INCOME +
                COUNTIP:PHSTAT, data = HE)
  summary(lm.fit)
  # ADJR2 = 0.5641, RSE = 2452 on 1979 DF, F-stat = 137.1 on 19 and 1979 DF. Decent improvement in model
  # after fitting with two-term interactions.

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 2439.381 in USD

    # VIF for collinearity:
    vif(lm.fit)
    # No issues of multi-collinearity

    # LOOCV
    glm.fit = glm(EXPENDIP ~ AGE + COUNTIP + INCOME + PHSTAT + AGE:COUNTIP + COUNTIP:INCOME +
                    COUNTIP:PHSTAT, data = HE)
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 2725.809 (default), 2725.732 (bias corrected) in USD
  # END

  # Conclusion: Model worse than current optimum RMSE of 2594.370 for EXPENDIP without EXPENDOP. Based on
  # the reduction of quality in this model by removing COUNTOP from the predictors and the drastic decrease
  # observed in the previous model from removing COUNTOP and COUNTIP, there is strong suggestion that a
  # model with EXPENDOP included as a predictor will be an improvement.
# END

# BEGIN - Multi-linear regression of EXPENDIP with no predictors removed (excluding perfect correlations)
  lm.fit = lm(EXPENDIP ~ . -COLLEGE -HIGHSCH, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Optimal model achieved:
  lm.fit = lm(EXPENDIP ~ AGE + USC + COUNTIP + COUNTOP + EXPENDOP + INCOME, data = HE)
  summary(lm.fit)
  # ADJR2 = 0.538, RSE = 2524 on 1989 DF, F-stat = 259.5 on 9 and 1989 DF. Model marginally better than
  # base model achieved with EXPENDOP excluded.

  lm.fit = lm(EXPENDIP ~ (AGE + USC + COUNTIP + COUNTOP + EXPENDOP + INCOME)^2, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Optimal model achieved with two-term interactions:
  lm.fit = lm(EXPENDIP ~ AGE + USC + COUNTIP + COUNTOP + EXPENDOP + INCOME + AGE:USC + AGE:COUNTIP
              + AGE:COUNTOP + AGE:EXPENDOP + AGE:INCOME + USC:COUNTIP + USC:EXPENDOP + COUNTIP:COUNTOP
              + COUNTIP:INCOME + COUNTOP:EXPENDOP + COUNTOP:INCOME + EXPENDOP:INCOME, data = HE)
  summary(lm.fit)
  # ADJR2 = 0.6493, RSE = 2199 on 1965 DF, F-stat = 113.1 on 33 and 1965 DF. Signifcant improvement in
  # model after fitting with two-term interactions. However, results practically identical to optimal
  # model achieved with EXPENDOP excluded.

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 2180.334 in USD

    # VIF for collinearity:
    vif(lm.fit)
    # As expected, COUNTIP, COUNTOP, and EXPENDOP show notable amounts of multicollinearity. This can be
    # safely ignored as these relationships will naturally be correlated.

    # LOOCV
    glm.fit = glm(EXPENDIP ~ AGE + USC + COUNTIP + COUNTOP + EXPENDOP + INCOME + AGE:USC + AGE:COUNTIP
                  + AGE:COUNTOP + AGE:EXPENDOP + AGE:INCOME + USC:COUNTIP + USC:EXPENDOP + COUNTIP:COUNTOP
                  + COUNTIP:INCOME + COUNTOP:EXPENDOP + COUNTOP:INCOME + EXPENDOP:INCOME, data = HE)
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 2505.386 (default), 2505.297 (bias corrected) in USD
  # END

  # Conclusion: Model offers sizable improvement over previously achieved optimum RMSE of 2594.370 for
  # EXPENDIP without EXPENDOP.
# END

factors = list("INCOME", "GENDER", "MARISTAT", "REGION", "factor(FAMSIZE)")
NonZeroExpOP = HE[HE$EXPENDOP > 0,]

plotFactors(f.data = NonZeroExpOP, f.x = list("AGE"), f.y = "EXPENDOP", f.factors = factors, f.save = T)
plotFactors(f.data = NonZeroExpOP, f.x = list("AGE"), f.y = "log10(EXPENDOP)", f.factors = factors, f.save = T)
plotFactors(f.data = NonZeroExpOP, f.x = factors, f.y = "EXPENDOP", f.factors = factors,
            f.type = "box", f.size = 0.6, f.save = T)
plotFactors(f.data = NonZeroExpOP, f.x = factors, f.y = "log10(EXPENDOP)", f.factors = factors,
            f.type = "box", f.size = 0.6, f.save = T)

# BEGIN - Multi-linear regression of EXPENDOP with no predictors removed (excluding perfect correlations)
  # Beginning with the best methodology determined from the tests done previously; will then perform the
  # same model versions tested above for robustness and verification that this set of predictors results
  # in the best model fit.
  lm.fit = lm(EXPENDOP ~ . -COLLEGE -HIGHSCH, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Optimal model achieved:
  lm.fit = lm(EXPENDOP ~ AGE + GENDER + INSURE + USC + COUNTIP + EXPENDIP + COUNTOP + RACE + REGION +
                PHSTAT, data = HE)
  summary(lm.fit)
  # ADJR2 = 0.4863, RSE = 2338 on 1980 DF, F-stat = 106.1 on 18 and 1980 DF.

  lm.fit = lm(EXPENDOP ~ (AGE + GENDER + INSURE + USC + COUNTIP + EXPENDIP + COUNTOP + RACE + REGION +
                            PHSTAT)^2, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Optimal model achieved with two-term interactions:
  lm.fit = lm(EXPENDOP ~ AGE + GENDER + INSURE + USC + COUNTIP + EXPENDIP + COUNTOP + RACE + REGION +
                PHSTAT + AGE:COUNTIP + AGE:EXPENDIP + AGE:COUNTOP + AGE:PHSTAT + GENDER:COUNTIP +
                GENDER:EXPENDIP + GENDER:RACE + GENDER:REGION + INSURE:COUNTOP + USC:COUNTIP +
                USC:COUNTOP + USC:RACE + COUNTIP:EXPENDIP + COUNTIP:RACE + EXPENDIP:COUNTOP +
                EXPENDIP:REGION + EXPENDIP:PHSTAT + COUNTOP:RACE + COUNTOP:REGION + COUNTOP:PHSTAT +
                RACE:REGION + RACE:PHSTAT + REGION:PHSTAT + INSURE:COUNTIP, data = HE)
  summary(lm.fit)
  # ADJR2 = 0.6477, RSE = 1936 on 1893 DF, F-stat = 35.98 on 105 and 1893 DF. Signifcant improvement in
  # model after fitting with two-term interactions. However, there is a singularity on RACEWHITE:PHSTATPOOR,
  # which will be removed after evaluating the model with it in.

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 1884.091 in USD

    # VIF cannot be run on the model as is due to the singularity.

    # LOOCV
    glm.fit = glm(EXPENDOP ~ AGE + GENDER + INSURE + USC + COUNTIP + EXPENDIP + COUNTOP + RACE + REGION +
                    PHSTAT + AGE:COUNTIP + AGE:EXPENDIP + AGE:COUNTOP + AGE:PHSTAT + GENDER:COUNTIP +
                    GENDER:EXPENDIP + GENDER:RACE + GENDER:REGION + INSURE:COUNTOP + USC:COUNTIP +
                    USC:COUNTOP + USC:RACE + COUNTIP:EXPENDIP + COUNTIP:RACE + EXPENDIP:COUNTOP +
                    EXPENDIP:REGION + EXPENDIP:PHSTAT + COUNTOP:RACE + COUNTOP:REGION + COUNTOP:PHSTAT +
                    RACE:REGION + RACE:PHSTAT + REGION:PHSTAT + INSURE:COUNTIP, data = HE)
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 4441.884 (default), 4441.030 (bias corrected) in USD. Model is highly inaccurate and one
    # of the worst produced so far. This is almost certainly because of the singularity.
  # END

  lm.fit = update(lm.fit, ~ . -RACE:PHSTAT)
  summary(lm.fit)
  # ADJR2 = 0.6318, RSE = 1979 on 1908 DF, F-stat = 39.09 on 90 and 1908 DF. Model slightly worsens after
  # removing RACE:PHSTAT to account for the singularity.

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 1933.781 in USD

    # VIF for collinearity:
    vif(lm.fit)
    # As expected, COUNTIP, COUNTOP, and EXPENDOP show significant amounts of multicollinearity. This can
    # be safely ignored as these relationships will naturally be correlated. There are some other issues
    # with GENDER and REGION being slightly over the cut-off mark of 5; however, this is not enough to
    # remove these predictors in such a complex model.

    # LOOCV
    glm.fit = glm(EXPENDOP ~ AGE + GENDER + INSURE + USC + COUNTIP + EXPENDIP + COUNTOP + RACE + REGION +
                    PHSTAT + AGE:COUNTIP + AGE:EXPENDIP + AGE:COUNTOP + AGE:PHSTAT + GENDER:COUNTIP +
                    GENDER:EXPENDIP + GENDER:RACE + GENDER:REGION + INSURE:COUNTOP + USC:COUNTIP +
                    USC:COUNTOP + USC:RACE + COUNTIP:EXPENDIP + COUNTIP:RACE + EXPENDIP:COUNTOP +
                    EXPENDIP:REGION + EXPENDIP:PHSTAT + COUNTOP:RACE + COUNTOP:REGION + COUNTOP:PHSTAT +
                    RACE:REGION + REGION:PHSTAT + INSURE:COUNTIP, data = HE)
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 3413.708 (default), 3413.188 (bias corrected) in USD
  # END
# END

# BEGIN - Multi-linear regression of EXPENDOP without EXPENDIP
  lm.fit = lm(EXPENDOP ~ . -EXPENDIP -COLLEGE -HIGHSCH, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Optimal model achieved:
  lm.fit = lm(EXPENDOP ~ AGE + GENDER + INSURE + COUNTIP + COUNTOP + RACE + REGION + PHSTAT, data = HE)
  summary(lm.fit)
  # ADJR2 = 0.4668, RSE = 2382 on 1982 DF, F-stat = 110.3 on 16 and 1982 DF.

  lm.fit = lm(EXPENDOP ~ (AGE + GENDER + INSURE + COUNTIP + COUNTOP + RACE + REGION + PHSTAT)^2, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Optimal model achieved with two-term interactions:
  lm.fit = lm(EXPENDOP ~ AGE + GENDER + INSURE + COUNTIP + COUNTOP + RACE +
                REGION + PHSTAT + AGE:COUNTIP + AGE:COUNTOP + AGE:PHSTAT +
                GENDER:COUNTOP + GENDER:RACE + GENDER:REGION + INSURE:COUNTIP +
                COUNTIP:RACE + COUNTIP:REGION + COUNTOP:RACE + COUNTOP:REGION +
                COUNTOP:PHSTAT + RACE:REGION + RACE:PHSTAT + REGION:PHSTAT, data = HE)
  summary(lm.fit)
  # ADJR2 = 0.6238, RSE = 2001 on 1910 DF, F-stat = 38.65 on 88 and 1910 DF.

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 1955.615 in USD

    # VIF cannot be run on the model as is due to the singularity.

    # LOOCV
    glm.fit = glm(EXPENDOP ~ AGE + GENDER + INSURE + COUNTIP + COUNTOP + RACE +
                    REGION + PHSTAT + AGE:COUNTIP + AGE:COUNTOP + AGE:PHSTAT +
                    GENDER:COUNTOP + GENDER:RACE + GENDER:REGION + INSURE:COUNTIP +
                    COUNTIP:RACE + COUNTIP:REGION + COUNTOP:RACE + COUNTOP:REGION +
                    COUNTOP:PHSTAT + RACE:REGION + RACE:PHSTAT + REGION:PHSTAT, data = HE)
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 4470.911 (default), 4470.051 (bias corrected) in USD. Once again, the model
    # is highly inaccurate and one of the worst produced so far because of the singularity.
  # END

  lm.fit = update(lm.fit, ~ . -RACE:PHSTAT)
  summary(lm.fit)
  # ADJR2 = 0.6094, RSE = 2039 on 1925 DF, F-stat = 43.69 on 73 and 1925 DF. Model slightly worsens after
  # removing RACE:PHSTAT to account for the singularity.

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 2000.671 in USD

    # VIF for collinearity:
    vif(lm.fit)
    # As expected, COUNTIP and COUNTOP show significant amounts of multicollinearity. This can
    # be safely ignored as these relationships will naturally be correlated. There are some other issues
    # with GENDER and REGION being slightly over the cut-off mark of 5; however, this is not enough to
    # remove these predictors in such a complex model.

    # LOOCV
    glm.fit = glm(EXPENDOP ~ AGE + GENDER + INSURE + COUNTIP + COUNTOP + RACE +
                    REGION + PHSTAT + AGE:COUNTIP + AGE:COUNTOP + AGE:PHSTAT +
                    GENDER:COUNTOP + GENDER:RACE + GENDER:REGION + INSURE:COUNTIP +
                    COUNTIP:RACE + COUNTIP:REGION + COUNTOP:RACE + COUNTOP:REGION +
                    COUNTOP:PHSTAT + RACE:REGION + REGION:PHSTAT, data = HE)
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 3195.355 (default), 3194.917 (bias corrected) in USD
  # END
# END

# BEGIN - Multi-linear regression of EXPENDOP without EXPENDIP, COUNTIP, and COUNTOP
  lm.fit = lm(EXPENDOP ~ . -EXPENDIP -COUNTIP -COUNTOP -COLLEGE -HIGHSCH, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Optimal model achieved:
  lm.fit = lm(EXPENDOP ~ AGE + ANYLIMIT + INSURE + USC + RACE + REGION + PHSTAT, data = HE)
  summary(lm.fit)
  # ADJR2 = 0.1409, RSE = 3023 on 1983 DF, F-stat = 22.84 on 15 and 1983 DF.

  lm.fit = lm(EXPENDOP ~ (AGE + ANYLIMIT + INSURE + USC + RACE + REGION + PHSTAT)^2, data = HE)
  lm.fit = stepAIC(lm.fit, direction = "both")
  lm.fit$anova

  # Optimal model achieved with two-term interactions:
  lm.fit = lm(EXPENDOP ~ AGE + ANYLIMIT + INSURE + USC + RACE + REGION + PHSTAT +
                AGE:ANYLIMIT + AGE:INSURE + AGE:REGION + AGE:PHSTAT + ANYLIMIT:USC +
                ANYLIMIT:RACE + ANYLIMIT:PHSTAT + INSURE:USC + INSURE:RACE +
                RACE:REGION + RACE:PHSTAT + REGION:PHSTAT, data = HE)
  summary(lm.fit)
  # ADJR2 = 0.2246, RSE = 2872 on 1921 DF, F-stat = 8.516 on 77 and 1921 DF.

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 2815.782 in USD

    # VIF cannot be run on the model as is due to the singularity.

    # LOOCV
    glm.fit = glm(EXPENDOP ~ AGE + ANYLIMIT + INSURE + USC + RACE + REGION + PHSTAT +
                    AGE:ANYLIMIT + AGE:INSURE + AGE:REGION + AGE:PHSTAT + ANYLIMIT:USC +
                    ANYLIMIT:RACE + ANYLIMIT:PHSTAT + INSURE:USC + INSURE:RACE +
                    RACE:REGION + RACE:PHSTAT + REGION:PHSTAT, data = HE)
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 3212.161 (default), 3212.051 (bias corrected) in USD.
  # END

  lm.fit = update(lm.fit, ~ . -RACE:PHSTAT)
  summary(lm.fit)
  # ADJR2 = 0.2136, RSE = 2893 on 1936 DF, F-stat = 9.752 on 62 and 1936 DF. Model slightly worsens after
  # removing RACE:PHSTAT to account for the singularity.

  # BEGIN - Validation testing
    # Estimated training RMSE:
    sqrt(mean(lm.fit$residuals^2))
    # RMSE = 2846.772 in USD

    # VIF for collinearity:
    vif(lm.fit)
    # Issues of multicollinearity with ANYLIMIT, INSURE, and REGION above 5.

    # LOOCV
    glm.fit = glm(EXPENDOP ~ AGE + ANYLIMIT + INSURE + USC + RACE + REGION + PHSTAT +
                    AGE:ANYLIMIT + AGE:INSURE + AGE:REGION + AGE:PHSTAT + ANYLIMIT:USC +
                    ANYLIMIT:RACE + ANYLIMIT:PHSTAT + INSURE:USC + INSURE:RACE +
                    RACE:REGION + REGION:PHSTAT, data = HE)
    cv.err = cv.glm(HE, glm.fit)
    sqrt(cv.err$delta)
    # Test RMSE = 3106.735 (default), 3106.667 (bias corrected) in USD
  # END
# END

# Conclusions of multi-linear regressions:
#   The optimal model achieved for EXPENDIP is one with all predictors included (excluding perfect
#   correlations) resulting in a bias-corrected test RMSE of 2505.297 achieved via LOOCV. The optimal
#   model achieved for EXPENDOP is one without EXPENDIP as a predictor. The bias-corrected test RMSE
#   achieved through LOOCV is 3194.917, which is worse than the model without EXPENDIP, COUNTIP, and
#   COUNTOP; however, the summary statistics of the former model (ADJR2 = 0.6094, RSE = 2039 on 1925
#   DF, F-stat = 43.69 on 73 and 1925 DF) are significantly superior to those of the latter (ADJR2 =
#   0.2136, RSE = 2893 on 1936 DF, F-stat = 9.752 on 62 and 1936 DF). The previous findings for the
#   EXPENDIP model without EXPENDOP, COUNTOP, and COUNTIP as predictors also indicate that removing
#   the COUNTOP and COUNTIP predictors monumentally reduce the accuracy of the prediction model, which
#   is in-line with the findings for the EXPENDOP version of the model. Therefore, despite having a
#   higher RMSE than the model excluding COUNTOP and COUNTIP, the EXPENDOP model with only EXPENDIP
#   removed as a predictor seems to be the most accurate.

detach(HE)
HE1 = HE
attach(HE1)
HE1$EXPENDIP[HE1$EXPENDIP > 0] = "Yes"
HE1$EXPENDIP[HE1$EXPENDIP == 0] = "No"
HE1$EXPENDIP = factor(HE1$EXPENDIP)
HE1$EXPENDOP[HE1$EXPENDOP > 0] = "Yes"
HE1$EXPENDOP[HE1$EXPENDOP == 0] = "No"
HE1$EXPENDOP = factor(HE1$EXPENDOP)

set.seed(1)
indexes = sample(1:nrow(HE1), 0.7 * nrow(HE1))
trainHE1 = HE1[indexes,]
testHE1 = HE1[-indexes,]
table(trainHE1["EXPENDIP"])
table(testHE1["EXPENDIP"])

logitStats = function(f.model, f.test, f.y) {
  prob = predict(logit.fit, newdata = f.test, type = "response")
  pred = ifelse(prob > 0.5, "Yes", "No")
  conmtx = table(pred, f.y)
  print("Accuracy rate:" + mean(pred == testHE1$EXPENDIP))
  print("Precision rate:" + conmtx["Yes", "Yes"] / conmtx[,"Yes"])
  print("Recall rate:" + mean(pred == testHE1$EXPENDIP))
}

# Including COUNTIP causes the model to fail as it is perfectly correlated to whether or not
# EXPENDIP is 'Yes' or 'No'.
logit.fit = glm(EXPENDIP ~ . -COUNTIP -COLLEGE -HIGHSCH, data = trainHE1, family = binomial)
summary(logit.fit)
prob = predict(logit.fit, newdata = testHE1, type = "response")
pred = ifelse(prob > 0.5, "Yes", "No")
table(pred, testHE1$EXPENDIP)
mean(pred == testHE1$EXPENDIP)

logit.fit = stepAIC(logit.fit, direction = "both", trace = F)
logit.fit$anova
# Optimal model achieved via stepAIC with AIC reduced from 631.45 to 605.24:
logit.fit = glm(EXPENDIP ~ AGE + GENDER + INSURE + UNEMPLOY + COUNTOP + EXPENDOP +
                  REGION + PHSTAT + INCOME, data = trainHE1, family = binomial)
summary(logit.fit)
prob = predict(logit.fit, newdata = testHE1, type = "response")
pred = ifelse(prob > 0.5, "Yes", "No")
table(pred, testHE1$EXPENDIP)
mean(pred == testHE1$EXPENDIP)

logit.fit = glm(EXPENDIP ~ (AGE + GENDER + INSURE + UNEMPLOY + COUNTOP + EXPENDOP +
                  REGION + PHSTAT + INCOME)^2, data = trainHE1, family = binomial)
logit.fit = stepAIC(logit.fit, direction = "both", trace = F)
logit.fit$anova

# Optimal model achieved via stepAIC with AIC reduced from 605.24 to :
logit.fit = glm(EXPENDIP ~ AGE + GENDER + INSURE + UNEMPLOY + COUNTOP + EXPENDOP +
                  REGION + PHSTAT + INCOME + AGE:GENDER + AGE:INCOME + GENDER:INSURE +
                  COUNTOP:REGION + COUNTOP:PHSTAT + UNEMPLOY:REGION, data = trainHE1, family = binomial)
summary(logit.fit)
prob = predict(logit.fit, newdata = testHE1, type = "response")
pred = ifelse(prob > 0.5, "Yes", "No")
table(pred, testHE1$EXPENDIP)
mean(pred == testHE1$EXPENDIP)

conmtx = table(pred, f.y)
print("Accuracy rate:" + mean(pred == testHE1$EXPENDIP))
print("Precision rate:" + conmtx["Yes", "Yes"] / conmtx[,"Yes"])
print("Recall rate:" + mean(pred == testHE1$EXPENDIP));
