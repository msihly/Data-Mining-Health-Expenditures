HE = read.csv("HealthExpend (Modified).csv")
attach(HE)

summary(HE)
# Determine variable types
str(HE)
# Coerce categorical variables to factors
factors = c("ANYLIMIT", "COLLEGE", "HIGHSCH", "GENDER", "MNHPOOR", "INSURE", "USC", "UNEMPLOY", "MANAGEDCARE")
HE[factors] = lapply(HE[factors], factor)
# Confirm type coercion
str(HE)

# BEGIN - Multi-linear regression of EXPENDIP
  lm.fit = lm(EXPENDIP ~ . -EXPENDOP, data = HE)
  summary(lm.fit)

  # EDUC returning NA values--signifies collinearity; check which variables are conflicting
  alias(lm.fit)

  # Remove EDUC because of perfect collinearity with HIGHSCH and COLLEGE
  lm.fit = update(lm.fit, ~ . -EDUC)
  summary(lm.fit)

  # BEGIN - Backwards selection
    lm.fit = update(lm.fit, ~ . -INDUSCLASS)
    summary(lm.fit)
    lm.fit = update(lm.fit, ~ . -USC)
    summary(lm.fit)
    # Possible issues with RACE - Bin as binary on WHITE
    lm.fit = update(lm.fit, ~ . -RACE)
    summary(lm.fit)
    # Possibly issues with MARISTAT - Bin as binary on MARRIED
    lm.fit = update(lm.fit, ~ . -MARISTAT)
    summary(lm.fit)
    lm.fit = update(lm.fit, ~ . -ANYLIMIT)
    summary(lm.fit)
    lm.fit = update(lm.fit, ~ . -MANAGEDCARE)
    summary(lm.fit)
    # Removing INCOME, FAMSIZE, INSURE, and MNHPOOR each decrease significance
  # END - Backwards selection
  par(mfrow = c(2, 2))
  plot(lm.fit)

  # Observation #733 causing extreme problems based on quad-plot
  HE[733,]

  # Remove outlier
  HE = HE[-733,]

  # Re-fit model to modified dataset
  lm.fit = lm(EXPENDIP ~ . -EXPENDOP -EDUC -INDUSCLASS -USC -RACE -MARISTAT -ANYLIMIT -MANAGEDCARE, data = HE)
  summary(lm.fit)

  # Model has changed drastically; must recreate model
  lm.fit = lm(EXPENDIP ~ . -EXPENDOP -EDUC, data = HE)
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

  # Optimal model reached with backwards selection:
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

# END

# BEGIN - Model selection using stepAIC
  library(MASS)

  fit = lm(EXPENDIP ~ . -EXPENDOP -EDUC, data = HE)
  step = stepAIC(fit, direction = "backward")
  step$anova

  fit = lm(EXPENDIP ~ . -EXPENDOP -EDUC, data = HE)
  step = stepAIC(fit, direction = "both")
  step$anova

  # Optimal model achieved in both backward-stepwise and both-stepwise:
  fit = lm(EXPENDIP ~ AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT, data = HE)
  summary(fit)

  # Refitting the original model with interaction terms causes excessively long processing time in R due to
  # single-core restrictions. Parallel processing not recommended / infeasible for stepwise selection. Instead,
  # interaction terms are fitted to the optimal base model achieved above.
  fit = lm(EXPENDIP ~ (AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT)^2, data = HE)
  step = stepAIC(fit, direction = "backward")
  # Processing takes less than two seconds to find optimal model with interaction terms
  step$anova

  sum = summary(fit)
  sum$adj.r.squared
  sum$fstatistic
  sum$sigma
  # Significant increase in ADJR2

  # StepAIC with direction = "both"
  fit = lm(EXPENDIP ~ (AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT)^2, data = HE)
  step = stepAIC(fit, direction = "both")
  step$anova

  sum = summary(fit)
  sum$adj.r.squared
  sum$fstatistic
  sum$sigma
  # Results are identical to backward-stepwise model with interaction terms

  # Optimal model achieved through two-term interaction stepAIC in both backward and both directions:
  fit = lm(EXPENDIP ~ AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT + AGE:GENDER + AGE:COUNTIP +
             AGE:COUNTOP + GENDER:COUNTIP + COUNTIP:COUNTOP + COUNTIP:INCOME + COUNTIP:PHSTAT +
             COUNTOP:INCOME + COUNTOP:PHSTAT + INCOME:PHSTAT, data = HE)
  summary(fit)

  # Fit model with three-term interactions
  fit = lm(EXPENDIP ~ (AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT)^3, data = HE)
  step = stepAIC(fit, direction = "both")
  step$anova

  sum = summary(fit)
  sum$adj.r.squared
  sum$fstatistic
  sum$sigma
  # ADJR2 significantly increased over two-term interaction model. However, model includes too many interaction
  # terms to feasibly interpret. RSE has also decreased, but so have the degrees of freedom. Model may also be
  # overfitted and F-Stat has decreased (as well as the DF).

  # Optimal model achieved through three-term interaction stepAIC on both directions:
  fit = lm(EXPENDIP ~ AGE + GENDER + COUNTIP + COUNTOP + INCOME + PHSTAT +
             AGE:GENDER + AGE:COUNTIP + AGE:COUNTOP + AGE:INCOME + AGE:PHSTAT +
             GENDER:COUNTIP + GENDER:COUNTOP + GENDER:INCOME + GENDER:PHSTAT +
             COUNTIP:COUNTOP + COUNTIP:INCOME + COUNTIP:PHSTAT + COUNTOP:INCOME +
             COUNTOP:PHSTAT + INCOME:PHSTAT + AGE:GENDER:COUNTIP + AGE:GENDER:COUNTOP +
             AGE:COUNTIP:COUNTOP + AGE:COUNTIP:INCOME + AGE:COUNTIP:PHSTAT +
             AGE:COUNTOP:INCOME + AGE:COUNTOP:PHSTAT + GENDER:COUNTIP:COUNTOP +
             GENDER:COUNTIP:INCOME + GENDER:COUNTIP:PHSTAT + GENDER:COUNTOP:INCOME +
             GENDER:COUNTOP:PHSTAT + COUNTIP:COUNTOP:INCOME + COUNTIP:COUNTOP:PHSTAT +
             COUNTIP:INCOME:PHSTAT + COUNTOP:INCOME:PHSTAT)
  summary(fit)

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
