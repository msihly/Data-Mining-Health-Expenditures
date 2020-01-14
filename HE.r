packageLoad = function(packages) {
  installed = installed.packages()[, "Package"]
  for (p in packages) {
    if (!(p %in% installed)) { install.packages(p) }
    library(p, character.only = T)
  }
}

packageLoad(c("ggplot2", "ggResidpanel", "car", "boot", "MASS"))

HE = read.csv("HealthExpend (Modified).csv")
attach(HE)

summary(HE)
# Determine variable types
str(HE)
# Convert GENDER from 0 and 1 to MALE and FEMALE for readability throughout, especially in plots
HE$GENDER = ifelse(HE$GENDER == 0, "MALE", "FEMALE")

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
      else if (f.type == "line") { print(plot + geom_line(aes_string(color = f.f_i), size = f.size)) }
      else if (f.type == "smooth") { print(plot + geom_smooth(aes_string(color = f.f_i, fill = f.f_i), size = f.size)) }
      else if (f.type == "smooth-point") { print(plot + geom_point(aes_string(color = f.f_i), size = f.size) +
                                             geom_smooth(aes_string(color = f.f_i, fill = f.f_i), size = f.size)) }
      if (f.save == T) {
        folder = paste(ifelse(typeof(f.y) == "character", f.y, "y"), "vs.", ifelse(typeof(f.x_i) == "character", f.x_i, "y"))
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

#####################################################################################################

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

  resid_panel(lm.fit, plots = "all")

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

    resid_panel(lm.fit, plots = "all")

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

    resid_panel(lm.fit, plots = "all")

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

    resid_panel(lm.fit, plots = "all")

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

  resid_panel(lm.fit, plots = "all")

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

  resid_panel(lm.fit, plots = "all")

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

  resid_panel(lm.fit, plots = "all")

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

#####################################################################################################

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

  resid_panel(lm.fit, plots = "all")

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

  resid_panel(lm.fit, plots = "all")

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

  resid_panel(lm.fit, plots = "all")

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

  resid_panel(lm.fit, plots = "all")

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

  resid_panel(lm.fit, plots = "all")

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

  resid_panel(lm.fit, plots = "all")

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

#####################################################################################################

# BEGIN - Logistic regression setup
  # Detach HE so that the modified copy of it doesn't mask the same variables
  detach(HE)
  HE1 = HE
  attach(HE1)

  # Convert EXPENDIP and EXPENDOP from continuous to categorical with non-zero expenditures coded as
  # 'Yes' and zero expenditures coded as 'No'. Update type to factor afterwards.
  HE1$EXPENDIP = ifelse(HE1$EXPENDIP > 0, "Yes", "No")
  HE1$EXPENDOP = ifelse(HE1$EXPENDOP > 0, "Yes", "No")
  factors = c("EXPENDIP", "EXPENDOP")
  HE1[factors] = lapply(HE1[factors], factor)
  str(HE1)

  # Create training and test sets at a 70:30 ratio, respectively
  set.seed(1)
  indexes = sample(1:nrow(HE1), 0.7 * nrow(HE1))
  trainHE1 = HE1[indexes,]
  testHE1 = HE1[-indexes,]
  # Verify the distribution of non-zero to zero expenditures is balanced between training and test sets
  table(trainHE1["EXPENDIP"])
  table(testHE1["EXPENDIP"])

  logitStats = function(f.model, f.test, f.y, f.prob = 0.5) {
    prob = predict(logit.fit, newdata = f.test, type = "response")
    pred = ifelse(prob > f.prob, "Yes", "No")
    conmtx = table(pred, f.y, dnn = NULL)
    precision = conmtx[2, 2] / sum(conmtx[, 2:2])
    recall = conmtx[2, 2] / sum(conmtx[2:2, ])

    print(conmtx)
    writeLines(paste("Accuracy rate:\t", mean(pred == f.y)))
    writeLines(paste("Precision rate:\t", precision))
    writeLines(paste("Recall rate:\t", recall))
    writeLines(paste("F1 Score:\t", 2 * (precision * recall) / (precision + recall)))
  }
# END

#####################################################################################################

# BEGIN - Logistic regression on EXPENDIP
  # Including COUNTIP causes the model to seemingly fail due to perfect correlations; however,
  # this is a logical outcome as all patients with a non-zero EXPENDIP will also have a non-zero
  # COUNTIP, and vice-versa. In essence, EXPENDIP and COUNTIP are interchangeable in this model.
  # The point of this prediction model is to determine the optimal set of predictors that give
  # accurate probabilities of patients falling into either the zero or non-zero cost categories.
  logit.fit = glm(EXPENDIP ~ . -COUNTIP -COLLEGE -HIGHSCH, data = trainHE1, family = binomial)
  summary(logit.fit)
  # AIC baseline of 631.45
  logitStats(logit.fit, testHE1, testHE1$EXPENDIP)
  # Baseline accuracy rate is remarkably high, but the precision rate is extremely low, and the
  # recall rate is lower than acceptable amounts. The model appears to be very good at predicting
  # the true zero-cost patients, but the precision rate, which is almost the exact compliment of
  # the accuracy rate, indicates that the model is just predicting most of the observations as
  # zero-cost without a proper basis for the prediction due to the skewedness of zero-cost to
  # non-zero observations in the dataset.

  # Use stepAIC to see if model can be improved
  logit.fit = stepAIC(logit.fit, direction = "both", trace = F)
  logit.fit$anova
  # Optimal model achieved via stepAIC:
  logit.fit = glm(EXPENDIP ~ AGE + GENDER + INSURE + UNEMPLOY + COUNTOP + EXPENDOP +
                    REGION + PHSTAT + INCOME, data = trainHE1, family = binomial)
  summary(logit.fit)
  # AIC reduced from 631.45 to 605.24
  logitStats(logit.fit, testHE1, testHE1$EXPENDIP)
  # While the AIC has been reduced, the accuracy rate has only been increased by 0.002. Normally,
  # this would still be an improvement as having less predictors in the model while maintaining a
  # similar accuracy rate tells us which predictors actually affect the probabilities. However,
  # the F1 score, which is a measure based on the precision and recall rates, is the ideal metric
  # for models where false positives and negatives have a significant impact AND where the data
  # is heavily imbalanced. Because the F1 score has decreased by 0.03, this model is worse than
  # the base model using all predictors.

  # Fit the optimal model achieved via stepAIC with two-term interactions
  logit.fit = glm(EXPENDIP ~ (AGE + GENDER + INSURE + UNEMPLOY + COUNTOP + EXPENDOP +
                    REGION + PHSTAT + INCOME)^2, data = trainHE1, family = binomial)
  logit.fit = stepAIC(logit.fit, direction = "both", trace = F)
  logit.fit$anova

  # Optimal model achieved via stepAIC:
  logit.fit = glm(EXPENDIP ~ AGE + GENDER + INSURE + UNEMPLOY + COUNTOP + EXPENDOP +
                    REGION + PHSTAT + INCOME + AGE:GENDER + AGE:INCOME + GENDER:INSURE +
                    COUNTOP:REGION + COUNTOP:PHSTAT + UNEMPLOY:REGION, data = trainHE1, family = binomial)
  summary(logit.fit)
  # AIC reduced from 605.24 to 585.74
  logitStats(logit.fit, testHE1, testHE1$EXPENDIP)
  # Fitting two-term interactions to the optimal model achieved via stepAIC further reduces the
  # accuracy of the model in every respect.

  # Attempt to fit two-term interactions to the base model as it, so far, has the best overall accuracy.
  logit.fit = glm(EXPENDIP ~ (. -COUNTIP -COLLEGE -HIGHSCH)^2, data = trainHE1, family = binomial)
  summary(logit.fit)
  # Aliased interaction terms (107 singularities) causing model to fail. AIC more than doubled to 1434.
  logitStats(logit.fit, testHE1, testHE1$EXPENDIP)
  # Worst model so far; false negatives tremendously increased.

  alias(logit.fit)
  # INDUSCLASS responsible for most of the singularities; instead of repeatedly sorting through summary()
  # and alias() results (which was done at first), the model is refit without INDUSCLASS so that the resulting
  # interaction terms don't need to be removed one by one.
  logit.fit = glm(EXPENDIP ~ (. -COUNTIP -COLLEGE -HIGHSCH -INDUSCLASS)^2, data = trainHE1, family = binomial)
  # Using alias (multiple times as the results exceed the console capacity, even with max.print set higher),
  # the following interaction terms have been removed to complete the process of clearing the singularities.
  logit.fit = update(logit.fit, ~ . -INSURE:MANAGEDCARE -COUNTOP:EXPENDOP -RACE:EDUC -RACE:MARISTAT -RACE:INCOME -RACE:PHSTAT -MARISTAT:PHSTAT)
  summary(logit.fit)
  # The AIC has increased by an extreme amount along with the residual deviance. Additionally, all visible
  # predictors have infintesimal p-values, which indicates a likely failed model with perfect correlations.
  # Additionally, due to the excessive length of the resulting formula, the model is now incomprehensible
  # for inference purposes, which is equally as important as the predictive capabilities of the model for
  # the practical implications of this dataset.
  logitStats(logit.fit, testHE1, testHE1$EXPENDIP)
  # Surprisingly, the accuracy of the overall model has only slightly decreased compared to previous models
  # with the biggest impact being a sizable decrease on the recall rate.

  # Despite the previous failures above, an attempt at using stepAIC was made again with the interaction
  # terms on the base set of predictors instead of the optimal model achieved via stepAIC. The intent was
  # to see if the additional interaction terms could provide a marginal improvement in overall accuracy.
  # However, the process was terminated after 40 as it caused R and RStudio to become unresponsive to the
  # point that it could not be terminated without force-closing RStudio. The results would have very likely
  # been worse than what was previously achieved and certainly incomprehensible for inference. The following
  # two lines are the code that was run. They have been commented out as they should not be run.
    # logit.fit = glm(EXPENDIP ~ (. -COUNTIP -COLLEGE -HIGHSCH)^2, data = trainHE1, family = binomial)
    # logit.fit = stepAIC(logit.fit, direction = "both", trace = F)

  # The final attempt at the EXPENDIP models will be without COUNTOP and EXPENDOP to see if there are any
  # predictors that truly have a correlation to the probabilities of zero-cost and non-zero expenditures
  # without having information on expenditure history.
  logit.fit = glm(EXPENDIP ~ . -COUNTIP -COUNTOP -EXPENDOP -COLLEGE -HIGHSCH, data = trainHE1, family = binomial)
  summary(logit.fit)
  # AIC has increased from the baseline of 631.45 to 712.02
  logitStats(logit.fit, testHE1, testHE1$EXPENDIP)
  # The accuracy rate is indentical to the base model, and the recall rate is only 0.0375 less. The precision
  # rate has decreased from 0.14 to 0.08, and the F1 score has decreased considerably from 0.212121 to
  # 0.13333. While the differences are actually rather insignifcant in the confusion matrix, the model is
  # still worse than the baseline model. However, these results are far better than expected and are actually
  # very useful as they reflect the situation of having demographic information and a couple of survey questions
  # without having access to the much more difficult to procure information on previous health expenditures. To
  # achieve a more precise model, complex case control sampling techniques would need to be implemented on the
  # dataset to account for the massive imbalance in zero-cost to non-zero observations.
# END

#####################################################################################################

# BEGIN - Logistic regression on EXPENDOP
  logit.fit = glm(EXPENDOP ~ . -COUNTOP -COLLEGE -HIGHSCH, data = trainHE1, family = binomial)
  summary(logit.fit)
  # Baseline AIC of 1370.5
  logitStats(logit.fit, testHE1, testHE1$EXPENDOP)
  # Model accuracy tremendously better than the best EXPENDIP model. The accuracy rate is only 0.77167, but the
  # F1 score is up to 0.840512. The overall increased accuracy of this model is likely due to the much larger
  # amount of non-zero observations in EXPENDOP than there are in EXPENDIP, which solves much of the previous
  # issues of imbalanced sampling. However, there are still large amounts of false positives and negatives
  # as seen in the confusion matrix, so improving the model would have significant impacts compared to the
  # improvements sought in the EXPENDIP models.

  # While stepAIC failed to improve the EXPENDIP model, it has a much better chance for EXPENDOP now that
  # the sampling issues are largely mitigated.
  logit.fit = stepAIC(logit.fit, direction = "both", trace = F)
  logit.fit$anova
  # Optimal model achieved via stepAIC:
  logit.fit = glm(EXPENDOP ~ ANYLIMIT + GENDER + USC + MANAGEDCARE + FAMSIZE +
                    COUNTIP + EDUC + MARISTAT + PHSTAT + INDUSCLASS, data = trainHE1, family = binomial)
  summary(logit.fit)
  # AIC reduced from 1370.5 to 1354.4
  logitStats(logit.fit, testHE1, testHE1$EXPENDOP)
  # Model has very marginally decreased in accuracy (almost identical)  while removing half of the predictors.
  # Interestingly, AGE and INCOME have been removed from the model, which are known to be the strongest
  # predictors aside from the COUNTs and EXPENDs.

  # Adding AGE and INCOME back into the model converts two false positives to true positives, but the overall
  # model accuracy is practically identical due to the large numbers of false negatives and positives. A much
  # larger decrease is needed in either one of the false predictions.
  logit.fit = glm(EXPENDOP ~ AGE + INCOME + ANYLIMIT + GENDER + USC + MANAGEDCARE + FAMSIZE +
                    COUNTIP + EDUC + MARISTAT + PHSTAT + INDUSCLASS, data = trainHE1, family = binomial)
  logitStats(logit.fit, testHE1, testHE1$EXPENDOP)

  logit.fit = glm(EXPENDOP ~ log10(AGE) + INCOME + ANYLIMIT + GENDER + USC + MANAGEDCARE + FAMSIZE +
                    COUNTIP + EDUC + MARISTAT + PHSTAT + INDUSCLASS, data = trainHE1, family = binomial)
  logitStats(logit.fit, testHE1, testHE1$EXPENDOP)
  # Taking the log of AGE converts 2 false negatives to true negatives. Much larger improvements needed.

  logit.fit = glm(EXPENDOP ~ log10(AGE) + MNHPOOR + INCOME + ANYLIMIT + GENDER + USC + MANAGEDCARE + FAMSIZE +
                    COUNTIP + EDUC + MARISTAT + PHSTAT + INDUSCLASS, data = trainHE1, family = binomial)
  logitStats(logit.fit, testHE1, testHE1$EXPENDOP)
  # Adding MNHPOOR converts one false negative to true negative

  # Due to the statistically insignificant benefits provided by adding AGE, log10(AGE), MNHPOOR, or INCOME
  # to the model, they have been removed to find the best predictive model using the least amount of
  # predictors, which has seemingly been determined already by stepAIC. The following model will remove
  # COUNTIP, which is identical to the factored EXPENDIP in this model, to simulate the situation of not
  # having access to health expenditure history.
  logit.fit = glm(EXPENDOP ~ ANYLIMIT + GENDER + USC + MANAGEDCARE + FAMSIZE +
                    EDUC + MARISTAT + PHSTAT + INDUSCLASS, data = trainHE1, family = binomial)
  logitStats(logit.fit, testHE1, testHE1$EXPENDOP)
  # Removing COUNTIP has a negligible effect on the model, which is the ideal outcome.

  # Attempt fitting current optimal model with interaction terms
  logit.fit = glm(EXPENDOP ~ (ANYLIMIT + GENDER + USC + MANAGEDCARE + FAMSIZE +
                    EDUC + MARISTAT + PHSTAT + INDUSCLASS)^2, data = trainHE1, family = binomial)
  logit.fit = stepAIC(logit.fit, direction = "both", trace = F)
  logit.fit$anova

  # Optimal model achieved via stepAIC with two-term interactions:
  logit.fit = glm(EXPENDOP ~ ANYLIMIT + GENDER + USC + MANAGEDCARE + FAMSIZE +
                    EDUC + MARISTAT + PHSTAT + INDUSCLASS + ANYLIMIT:GENDER +
                    ANYLIMIT:MARISTAT + GENDER:FAMSIZE + GENDER:EDUC + GENDER:MARISTAT +
                    GENDER:PHSTAT + USC:MARISTAT + USC:PHSTAT + MANAGEDCARE:MARISTAT +
                    MANAGEDCARE:PHSTAT + FAMSIZE:INDUSCLASS + EDUC:MARISTAT +
                    MARISTAT:INDUSCLASS, data = trainHE1, family = binomial)
  summary(logit.fit)
  logitStats(logit.fit, testHE1, testHE1$EXPENDOP)

  # Removed aliased term
  logit.fit = update(logit.fit, ~ . -MARISTAT:INDUSCLASS)
  summary(logit.fit)
  logitStats(logit.fit, testHE1, testHE1$EXPENDOP)
  # Marginal improvement. Overall, model worse than simplest optimal model while adding
  # too many predictors.
# END

# Conclusions of logistic regression:
#   Optimal model achieved for EXPENDIP is the baseline model with all predictors for prediction purposes
#   as it has a higher F1 score, which is the more important metric for this dataset, than the optimal
#   model achieved via stepAIC, which has eliminated several insignificant predictors and is the second
#   best predictive model. The best model for inference is the baseline model without COUNTOP and EXPENDOP
#   included as predictors as it simulates a common situation of not having access to medical history.
#     Prediction: EXPENDIP ~ . -COUNTIP -COLLEGE -HIGHSCH
#     Inference:  EXPENDIP ~ . -COUNTIP -COUNTOP -EXPENDOP -COLLEGE -HIGHSCH
#
#   Optimal model achieved for EXPENDOP is the model achieved via stepAIC with the additional exclusion of
#   COUNTIP for both prediction and inference. The model has a high accuracy rate and F1 score while not
#   relying on access to medical history. There are large amounts of false positives and negatives by count
#   in the confusion matrix, but several attempts at improving the model have failed; more continous predictors
#   and survey questions would likely be the only way to improve the model without overfitting.
#     Prediction: EXPENDOP ~ ANYLIMIT + GENDER + USC + MANAGEDCARE + FAMSIZE + EDUC + MARISTAT + PHSTAT + INDUSCLASS
#     Inference:  EXPENDOP ~ ANYLIMIT + GENDER + USC + MANAGEDCARE + FAMSIZE + EDUC + MARISTAT + PHSTAT + INDUSCLASS

logit.fit = glm(EXPENDIP ~ . -COUNTIP -COLLEGE -HIGHSCH, data = trainHE1, family = binomial)
preds = plogis(predict(logit.fit, newdata = testHE1))
plotFactors(f.data = testHE1, f.x = list("AGE", "FAMSIZE"), f.y = preds, f.factors = factors, f.type = "smooth", f.size = 1, f.save = T)

#####################################################################################################

# The following sections are failed attempts at methodologies and mistakes made kept for reference and as a
# reminder to carefully check what could be causing drastic, unexpected, and inexplicable changes in results.

# The following commented-out section was the result of mistakenly passing EXPENDIP to the logitStats
# function instead of EXPENDOP beginning with the analysis of the stepAIC function. None of it is
# accurate after realizing this mistake.
  # # The model has, surprisingly, changed in drastic ways. The number of true negatives has increased from
  # # 102 to 147, but the number of true positives has decreased from 361 to 50. The number of false negatives
  # # has increased from 96 to 403, and the number of false positives has decreased from 41 to 0. The reasons
  # # for these drastic changes are almost inexplicable. In looking at the differences in predictors, EXPENDIP,
  # # AGE, and INCOME have all been removed from the model; these predictors are known to be the most important
  # # predictors based on all previous analysis of the multi-linear and logistic regressions as well as basic
  # # logic. Accordingly, the stepAIC function seems to be completely infeasible for use for logistic regression
  # # of this dataset, so manual determination of predictors will be used instead.
  #
  # # An attempt will be made without the EXPENDIP and COUNTIP predictors first to simulate the situation of
  # # not having access to previous health expenditures as done before.
  # logit.fit = glm(EXPENDOP ~ . -COUNTOP -COUNTIP -EXPENDIP -COLLEGE -HIGHSCH, data = trainHE1, family = binomial)
  # summary(logit.fit)
  # # AIC increased from 1370.5 to 1421.2
  # logitStats(logit.fit, testHE1, testHE1$EXPENDOP)
  # # The model suffers the same issues detailed above in the stepAIC model. It appears that EXPENDIP and COUNTIP
  # # have a significant impact on the EXPENDOP model to the point of accounting for most of the model accuracy
  # # whereas EXPENDOP and COUNTOP had minimal effect on the EXPENDIP model.
  #
  # # The next two models will test if removing only one of EXPENDIP or COUNTIP returns different results
  # logit.fit = glm(EXPENDOP ~ . -COUNTOP -EXPENDIP -COLLEGE -HIGHSCH, data = trainHE1, family = binomial)
  # summary(logit.fit)
  # logitStats(logit.fit, testHE1, testHE1$EXPENDOP)
  #
  # logit.fit = glm(EXPENDOP ~ . -COUNTOP -COUNTIP -COLLEGE -HIGHSCH, data = trainHE1, family = binomial)
  # summary(logit.fit)
  # logitStats(logit.fit, testHE1, testHE1$EXPENDOP)
  # # Both models are identical to each other as well as the model with EXPENDIP and COUNTIP removed, which is
  # # an unexpected outcome. This implies that the model sees both EXPENDIP and COUNTIP as containing the same
  # # information, which means the actual number of inpatient visits has no bearing as it is interpreted as a
  # # binary indicator identical to the factored EXPENDIP. The interesting and baffling part is then that despite
  # # both containing the same information, EXPENDIP and COUNTIP must both be in the model for it to be at all
  # # accurate. Due to the extreme extent to which the model fails without one or both of these predictors
  # # included, there isn't much inference that can be done about the other predictors. Thus, this model will
  # # only be used for prediction purposes under the premise that previous health expenditure history is available.
  #
  # # As EXPENDIP and COUNTIP are so crucial to the model, adding an interaction term between them may improve
  # # the model over the baseline.
  # logit.fit = glm(EXPENDOP ~ . -COUNTOP -COLLEGE -HIGHSCH +EXPENDIP:COUNTIP, data = trainHE1, family = binomial)
  # summary(logit.fit)
  # logitStats(logit.fit, testHE1, testHE1$EXPENDOP)
  # # For some indeterminate reason, adding the EXPNEDIP:COUNTIP interaction term into the model has the same
  # # adverse effects described above.

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