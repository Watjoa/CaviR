#' @title manovaR
#'
#' @description This function plots a correlation table in APA style with descriptive statistics.
#'
#' @param data Your data with the variable in order you want to present them
#' @param stand present standard deviations or not
#' @param tukey Your data with the variable in order you want to present them
#' @param sign significance level of multi comparison
#' @return A html correlation matrix with significance stars, Note and descriptive statistics
#' @export
#' @importFrom stats anova lm reshape aggregate
#' @importFrom emmeans emmeans
#' @importFrom psych describeBy cohen.d
#' @importFrom flextable flextable footnote valign autofit bold as_paragraph align_text_col align
#' @importFrom effectsize eta_squared
#' @importFrom gtools stars.pval
#' @importFrom multcomp cld
#' @importFrom dplyr bind_rows

manovaR<- function(data,tukey = FALSE, stand=TRUE,sign = 0.05) {

 dataset <- as.data.frame(data)

  group <- colnames(dataset)[1]
  names(dataset)[names(dataset) == group] <- 'groupVAR'
  dataset$groupVAR <- as.factor(dataset$groupVAR)

  dfmanova <- dataset[,-1]
  dfmanova <- as.data.frame(sapply(dfmanova,as.numeric))

    models <- lapply(names(dfmanova), function(x) {
      stats::lm(substitute(i~ groupVAR, list(i = as.name(x))), data = dataset)})

  if(tukey==FALSE){

    table3 <- psych::describeBy(dfmanova,
                                group=dataset$groupVAR,
                                mat=TRUE,type=3,digits=2)

    if(stand==FALSE){
    table34 <- table3[,c('group1','mean')]
    table34$mean <- format(round(table34$mean,2), nsmall = 2)
    table35 <- data.frame(split(c(table34$mean),table34$group1))

    }else if (stand==TRUE){
    table34 <- table3[,c('group1','mean','sd')]
    table34$mean <- format(round(table34$mean,2), nsmall = 2)
    table34$sd <- format(round(table34$sd,2), nsmall = 2)
    table34$comb <- paste(table34$mean,' (±',table34$sd,')',sep="")
    table35 <- data.frame(split(c(table34$comb),table34$group1))

    }

    colnames(table35) <- levels(as.factor(table34$group1))
    rownames(table35) <- names(dfmanova)

  }else if (tukey==TRUE & length(levels(dataset$groupVAR))>2){

    descriptives <- list()


    for(i in 1:length(models)) {
      mod_means_contr <- emmeans::emmeans(object = models[[i]],
                                          pairwise ~ groupVAR,
                                          adjust = "tukey")
      means <- as.data.frame(mod_means_contr$emmeans)
      names <- means$groupVAR
      means <- as.data.frame(t(round(means$emmean,2)))
      colnames(means) <- names

                if(stand==FALSE){
                  table3 <- psych::describeBy(dfmanova[,i],
                                              group=dataset$groupVAR,
                                              mat=TRUE,type=3,digits=2)
                  table34 <- table3[,c('group1','mean')]
                  table34$mean <- format(round(table34$mean,2), nsmall = 2)
                  table34$ID <- 1
                  table35 <- stats::reshape(table34,
                                    idvar = "ID",
                                    timevar = "group1",
                                    direction = "wide")
                  table35 <- table35[,-which(colnames(table35)=="ID")]
                  colnames(table35) <- table34$group1
                  rownames(table35) <- NULL
                  table35 <- table35[,names(means)]
                  means <- table35

                }else if (stand==TRUE){
                  table3 <- psych::describeBy(dfmanova[,i],
                                              group=dataset$groupVAR,
                                              mat=TRUE,type=3,digits=2)
                  table34 <- table3[,c('group1','mean','sd')]
                  table34$mean <- format(round(table34$mean,2), nsmall = 2)
                  table34$sd <- format(round(table34$sd,2), nsmall = 2)
                  table34$comb <- paste(table34$mean,' (±',table34$sd,')',sep="")

                  table34$ID <- 1
                  table35 <- stats::reshape(table34[,c('ID','group1','comb')],
                                            idvar = "ID",
                                            timevar = "group1",
                                            direction = "wide")
                  table35 <- table35[,-which(colnames(table35)=="ID")]
                  colnames(table35) <- table34$group1
                  rownames(table35) <- NULL
                  table35<-table35[,names(means)]
                  means <- table35
                }

      letterss <- suppressMessages( suppressWarnings(multcomp::cld(mod_means_contr,sort=TRUE,
                                       level=sign,
                                       adjust = "tukey",Letters=LETTERS)))

      letters <- letterss[,c('.group')]
      letters <- gsub(" ", "", letters)
      letters <- as.data.frame(letters)
      letters <- as.data.frame(t(letters))
      colnames(letters) <- letterss$groupVAR

      totals <- as.data.frame(rbind(means,letters))
      totals <- as.data.frame(t(totals))
      totals$comb <- paste(totals[,1],totals[,2],sep=" ")
      totals <- as.data.frame(t(totals$comb))
      colnames(totals) <- names
      descriptives[[i]] <- totals
    }

    descriptives <- dplyr::bind_rows(descriptives)
    rownames(descriptives) <- names(dfmanova)
    table35 <- descriptives

  }else if (tukey==TRUE & length(levels(dataset$groupVAR))<3){

    descriptives <- list()

    if(stand==FALSE){

      tableagg <- stats::aggregate(dfmanova, by=list(group1=dataset$groupVAR), mean,na.rm=TRUE)
      table3 <- as.data.frame(t(tableagg[,-1]))
      colnames(table3) <- tableagg$group1
      table3 <- round(table3,2)

    }else if (stand==TRUE){


      for(i in 1:length(names(dfmanova))) {

        mod_means_contr <- emmeans::emmeans(object = models[[i]],
                                            pairwise ~ groupVAR,
                                            adjust = "tukey")
        means <- as.data.frame(mod_means_contr$emmeans)
        names <- means$groupVAR
        means <- as.data.frame(t(round(means$emmean,2)))
        colnames(means) <- names

        table3 <- psych::describeBy(dfmanova[,i],
                                    group=dataset$groupVAR,
                                    mat=TRUE,type=3,digits=2)
        table34 <- table3[,c('group1','mean','sd')]
        table34$mean <- format(round(table34$mean,2), nsmall = 2)
        table34$sd <- format(round(table34$sd,2), nsmall = 2)
        table34$comb <- paste(table34$mean,' (±',table34$sd,')',sep="")

        table34$ID <- 1
        table35 <- stats::reshape(table34[,c('ID','group1','comb')],
                                  idvar = "ID",
                                  timevar = "group1",
                                  direction = "wide")
        table35 <- table35[,-which(colnames(table35)=="ID")]
        colnames(table35) <- table34$group1
        rownames(table35) <- NULL
        table35<-table35[,names(means)]
        descriptives[[i]] <- table35
    }

    descriptives <- dplyr::bind_rows(descriptives)
    rownames(descriptives) <- names(dfmanova)
    table3 <- descriptives
    print('Grouping variable has only 2 levels. Tukey not applicable')
    }
  }

  anovatab <- lapply(models, anova)
  anovatabb <- data.frame(matrix(unlist(anovatab),
                                 nrow=length(anovatab), byrow=T))
  fvalue <- anovatabb[,c(7,9)]
  fvalue <- round(fvalue,2)
  colnames(fvalue) <- c('F-value','p-value')
  stars <- gtools::stars.pval(fvalue$`p-value`)
  fvalue$stars <- stars
  fvalue[which(fvalue$`p-value`<.001),'p-value'] <- '<.001'
  colnames(fvalue) <- c('F-value','p-value', ' ')

  etasq <- suppressMessages(lapply(models, suppressPackageStartupMessages(suppressMessages(suppressMessages(effectsize::eta_squared)))))
  anovatabbetasq <- matrix(unlist(etasq), nrow=length(etasq), byrow=T)
  fvalueetasq <- as.numeric(anovatabbetasq[,2])

  fvalueetasq <- round(fvalueetasq,2)
  fvalueetasq <- as.data.frame(fvalueetasq)
  colnames(fvalueetasq) <- c('eta-squared')

  tabelcohen <- psych::cohen.d(dfmanova, dataset$groupVAR ,alpha=.05,std=TRUE)
  tabellcohen <- as.data.frame(tabelcohen[1])
  tabellcohen <- round(tabellcohen[2],2)
  colnames(tabellcohen) <- c("Cohen's D")

  variables <- c(names(dfmanova))
  total <- cbind(variables,table35,fvalue,fvalueetasq)
  total <- total[!sapply(total, function(x) all(x == ""))]

  modelstot <-  stats::lm(as.matrix(dfmanova) ~ dataset$groupVAR)
  anova <- stats::anova(modelstot, test = "Wilks")
  anova <- as.data.frame(anova)
  anova <- round(anova, 3)
  anova[which(anova$`Pr(>F)` <.001),'Pr(>F)'] <- '<.001'

  test.label <- paste0('Wilks Lambda = ',anova$Wilks[2], ',',
                       "F", '(',anova$`num Df`[2],',', anova$`den Df`[2],')',
                       " = ", anova$`approx F`[2],
                       " , p = ", anova$`Pr(>F)`[2]
  )

  table <- flextable::flextable(total)
  table <- flextable::align_text_col(table,align = "center")
  table <- flextable::align(table,j=1,align = "left")
  table <- flextable::align(table,j=1,align = "left",part="header")
  table <- flextable::footnote(table, i = 1, j = 1,
                               value = flextable::as_paragraph(
                                 c(test.label)
                               ),
                               ref_symbols = c(" "),
                               part = "header")
  table <- flextable::valign(table, valign = "bottom", part = "header")
  table <- flextable::bold(table, i = 1, bold = TRUE, part = "header")
  table <- flextable::autofit(table)
  return(table)
}

