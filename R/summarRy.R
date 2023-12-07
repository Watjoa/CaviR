#' @title summaRy
#'
#' @description This function plots a correlation table in APA style with descriptive statistics.
#'
#' @param model Your dataset with the variable in order you want to present them
#' @return A html correlation matrix with significance stars, Note and descriptive statistics
#' @export
#' @importFrom effectsize standardize_parameters eta_squared
#' @importFrom gtools stars.pval
#' @importFrom flextable flextable add_footer_lines bold italic autofit
#' @importFrom modelsummary get_gof
#' @importFrom sjstats anova_stats

summaRy<- function(model) { # input is a dataset (x) that includes a grouping variable (group)

  round_df <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

    df[,nums] <- round(df[,nums], digits = digits)

    (df)
  }

  if(grepl('lmer',class(model))== FALSE &&
     grepl('lm',class(model)) == TRUE) {

  summary <- data.frame(pms = names(model$coefficients),
                        coef = format(round(as.numeric(model$coefficients),2), nsmall = 2),
                        stand = format(round(effectsize::standardize_parameters(model, method = "basic")[,2],2), nsmall = 2),
                        stderr =  round(summary(model)$coefficients[,2],2),
                        tval =  round(summary(model)$coefficients[,3],2),
                        pval =  round(summary(model)$coefficients[,4],2),
                        pstar = gtools::stars.pval(round(summary(model)$coefficients[,4],2)))
  colnames(summary) <- c('Predictors','coefficients','β', 'std. error',
                         't-value','p-value',' ')
  summary[which(summary$`p-value`<0.001),'p-value'] <- '<.001'
  rownames(summary) <- NULL

  leeg3 <- data.frame(a = NA, b = NA, c = NA, d = NA, e = NA, f = NA, g = NA)
  colnames(leeg3) <- colnames(summary)

  leeg4 <- data.frame(a = 'ANOVA:', b = NA, c = NA, d = NA, e = NA, f = NA, g = NA)
  colnames(leeg4) <- colnames(summary)

  leeg5 <- data.frame(a = NA, b = 'Sumsq', c = 'Meansq', d = '(df)', e = 'F stat', f = 'partial η2', g = 'VIF')
  colnames(leeg5) <- colnames(summary)

  anovamodel <- as.data.frame(sjstats::anova_stats(model))[c(1:length(rownames(as.data.frame(sjstats::anova_stats(model))))-1),c('term','sumsq','meansq',
                                                                                                       'statistic','p.value','df',
                                                                                                       'etasq','partial.etasq','cohens.f')]
  anovamodelb <- as.data.frame(sjstats::anova_stats(model))[c(length(rownames(as.data.frame(sjstats::anova_stats(model))))),c('term','sumsq','meansq',
                                                                                                       'statistic','p.value','df',
                                                                                                        'etasq','partial.etasq','cohens.f')]
  anovamodel$p.value <- format(round(as.numeric(anovamodel$p.value),2), nsmall = 2)
  anovamodel[which(as.numeric(anovamodel$p.value)<0.001),'p.value'] <- '<.001'

  anovamodel$partial.etasq <- effectsize::eta_squared(model)[,2]

  anovamodell <- data.frame(a = anovamodel$term,
                            b = format(round(as.numeric(anovamodel$sumsq),2), nsmall = 2),
                            c = format(round(as.numeric(anovamodel$meansq),2), nsmall = 2),
                            d = anovamodel$df,
                            e = paste(format(round(as.numeric(anovamodel$statistic),2), nsmall = 2),
                                      ', p = ',
                                      anovamodel$p.value,sep=''),
                            f = format(round(as.numeric(anovamodel$partial.etasq),2), nsmall = 2),
                            g = format(round(as.numeric(multicollinearity(model)$VIF),2), nsmall = 2))
  colnames(anovamodell) <- colnames(summary)

  anovamodellb <- data.frame(a = anovamodelb$term,
                             b = anovamodelb$sumsq,
                             c = anovamodelb$meansq,
                             d = anovamodelb$df,
                             e = NA,
                             f = NA,
                             g = NA)
  colnames(anovamodellb) <- colnames(summary)

  summary <- as.data.frame(rbind(summary,leeg3,leeg4,leeg5,anovamodell,anovamodellb,leeg3))

  ANOVAROW <- as.numeric(rownames(summary[which(summary$Predictors=="ANOVA"),]))

  summary <- flextable::flextable(summary)
  info <- paste('Info: ',length(model$residuals), ' observations, (',
                length(summary(model)$na.action),') missing obs. deleted',sep="")
  summary <- flextable::add_footer_lines(summary, info)

  pvalueFtestmodel <- format(round(pf(summary(model)$fstatistic[1], summary(model)$fstatistic[2], summary(model)$fstatistic[3], lower.tail = FALSE),2), nsmall = 2)
  pvalueFtestmodel <- ifelse(as.numeric(pvalueFtestmodel)<0.001,pvalueFtestmodel <- "<.001",NULL)

  fit <- paste('Fit: F(',
               unname(summary(model)$fstatistic[2]), ',',
               unname(summary(model)$fstatistic[3]), ') = ',
               format(round(unname(summary(model)$fstatistic[1]),2), nsmall = 2), ', p = ',
               pvalueFtestmodel,sep="")
  summary <- flextable::add_footer_lines(summary, fit)

  rsq <- paste('R2= ',
               round(unname(summary(model)$r.squared),2), ', Adj. R2 = ',
               round(unname(summary(model)$adj.r.squared),2)

               ,sep="")
  summary <- flextable::add_footer_lines(summary, rsq)
  summary <- flextable::bold(summary, i = c(ANOVAROW,ANOVAROW+1), bold=TRUE)
  summary <- flextable::italic(summary, i = c(ANOVAROW+1))
  summary <- flextable::bold(summary,  bold=TRUE,part='header')
  summary <- flextable::autofit(summary)


  return( summary)

  } else if ( grepl('lmer',class(model))== TRUE ) {

# MEERDERE LEVELS
      summary <- data.frame(pms = names(summary(model)$coefficients[,1]),
                            coef = format(round(summary(model)$coefficients[,1],2), nsmall = 2),
                            stand = format(round(effectsize::standardize_parameters(model, method = "basic")[,2],2), nsmall = 2),
                            stderr =  round(summary(model)$coefficients[,2],2),
                            tval =  round(summary(model)$coefficients[,4],2),
                            pval =  round(summary(model)$coefficients[,5],2),
                            pstar = gtools::stars.pval(round(summary(model)$coefficients[,5],2)))

      colnames(summary) <- c('Predictors','coefficients','β', 'std. error',
                             't-value','p-value',' ')
      summary[which(summary$`p-value`<0.001),'p-value'] <- '<.001'
      rownames(summary) <- NULL

      randomeffects <- as.data.frame(VarCorr(model))[,c(1,4)]
      randomeffects <- round_df(randomeffects,2)
      randomeffects$vcov <- format(round(as.numeric(sqrt(randomeffects$vcov)),2), nsmall = 2)

      randomNA <- as.data.frame(rep(NA,dim(randomeffects)[2]))
      colnames(randomNA) <- 'NA'
      randomNA <- cbind(randomNA, rep(randomNA,4))

      randomm <- as.data.frame(cbind(randomeffects,randomNA))
      colnames(randomm) <- colnames(summary)

      leeg <- data.frame(a = NA, b = NA, c = NA, d = NA, e = NA, f = NA, g = NA)
      colnames(leeg) <- colnames(summary)

      leeg2 <- data.frame(a = 'Random effects:', b = NA, c = NA, d = NA, e = NA, f = NA, g = NA)
      colnames(leeg2) <- colnames(summary)

      names <- data.frame(a = 'group', b = 'Std. Dev.', c = NA, d = NA, e = NA, f = NA, g = NA)
      colnames(names) <- colnames(summary)

      summary <- as.data.frame(rbind(summary,leeg,leeg2,names,randomm))

      leeg3 <- data.frame(a = NA, b = NA, c = NA, d = NA, e = NA, f = NA, g = NA)
      colnames(leeg3) <- colnames(summary)

      leeg4 <- data.frame(a = 'ANOVA:', b = NA, c = NA, d = NA, e = NA, f = NA, g = NA)
      colnames(leeg4) <- colnames(summary)

      leeg5 <- data.frame(a = NA, b = 'Sumsq', c = 'Meansq', d = '(NumDF, DenDF)', e = 'F stat', f = 'partial η2', g = 'VIF')
      colnames(leeg5) <- colnames(summary)

      anovamodel <- as.data.frame(sjstats::anova_stats(model))[c(1:length(rownames(as.data.frame(sjstats::anova_stats(model))))-1),c('term','sumsq','meansq',
                                                                                                                   'NumDF','DenDF',
                                                                                                             'statistic','p.value','df',
                                                                                                             'etasq','partial.etasq','cohens.f')]
      anovamodelb <- as.data.frame(sjstats::anova_stats(model))[c(length(rownames(as.data.frame(sjstats::anova_stats(model))))),c('term','sumsq','meansq',
                                                                                                                'NumDF','DenDF',
                                                                                                          'statistic','p.value','df',
                                                                                                          'etasq','partial.etasq','cohens.f')]
      anovamodel$p.value <- format(round(as.numeric(anovamodel$p.value),2), nsmall = 2)
      anovamodel[which(as.numeric(anovamodel$p.value)<0.001),'p.value'] <- '<.001'

      anovamodel$partial.etasq <- effectsize::eta_squared(model)[,2]

      anovamodel$df <- paste('(',format(round(as.numeric(anovamodel$NumDF),2), nsmall = 2),', ',format(round(as.numeric(anovamodel$DenDF),2), nsmall = 2),')',sep="")

      anovamodell <- data.frame(a = anovamodel$term,
                                b = format(round(as.numeric(anovamodel$sumsq),2), nsmall = 2),
                                c = format(round(as.numeric(anovamodel$meansq),2), nsmall = 2),
                                d = anovamodel$df,
                                e = paste(format(round(as.numeric(anovamodel$statistic),2), nsmall = 2),
                                          ', p = ',
                                          anovamodel$p.value,sep=''),
                                f = format(round(as.numeric(anovamodel$partial.etasq),2), nsmall = 2),
                                g = format(round(as.numeric(multicollinearity(model)$VIF),2), nsmall = 2))
      colnames(anovamodell) <- colnames(summary)

      anovamodellb <- data.frame(a = anovamodelb$term,
                                 b = anovamodelb$sumsq,
                                 c = anovamodelb$meansq,
                                 d = anovamodelb$df,
                                 e = NA,
                                 f = NA,
                                 g = NA)
      colnames(anovamodellb) <- colnames(summary)


      summary <- as.data.frame(rbind(summary,leeg3,leeg4,leeg5,anovamodell,anovamodellb,leeg))
      ANOVAROW <- as.numeric(rownames(summary[which(summary$Predictors=="ANOVA:"),]))
      RANDOMROW <- as.numeric(rownames(summary[which(summary$Predictors=="Random effects:"),]))

      summary <- flextable::flextable(summary)
      info <- paste('Info: ',length(residuals(model)), ' observations, (',
                    length(summary(model)$na.action),') missing obs. deleted',sep="")
      summary <- flextable::add_footer_lines(summary, info)




      aic <- paste('AIC = ',
                   round(unname(modelsummary::get_gof(model)[,1]),2), ', BIC = ',
                   round(unname(modelsummary::get_gof(model)[,2]),2)
                   ,sep="")
      summary <- flextable::add_footer_lines(summary, aic)

      rsq <- paste('R2 conditional = ',
                   round(unname(modelsummary::get_gof(model)[,3]),2), ', R2 marginal = ',
                   round(unname(modelsummary::get_gof(model)[,4]),2)
                   ,sep="")
      summary <- flextable::add_footer_lines(summary, rsq)

      ICC <- paste('ICC = ',
                   round(unname(modelsummary::get_gof(model)[,5]),2),sep="")
      summary <- flextable::add_footer_lines(summary, ICC)

      summary <- flextable::bold(summary, i = c(RANDOMROW,ANOVAROW,ANOVAROW+1), bold=TRUE)
      summary <- flextable::italic(summary, i = c(ANOVAROW+1))
      summary <- flextable::bold(summary,  bold=TRUE,part='header')
      summary <- flextable::autofit(summary)
      return( summary)


  }

}
