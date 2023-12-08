#' @title inteRplot
#'
#' @description informative interaction figure
#'
#' @param model a vector of your variables
#' @param pred a vector of your variables
#' @param mod a vector of your variables
#' @param outcome a vector of your variables
#' @param xaxis a vector of your variables
#' @param moderator a vector of your variables
#' @param miny a vector of your variables
#' @param maxy a vector of your variables
#' @param xlabels a vector of your variables
#' @return interaction figure
#' @export
#' @importFrom ggeffects ggeffect
#' @importFrom ggplot2 ggplot aes element_text
#' @importFrom ggthemes theme_few
#' @importFrom data.table as.data.table
#' @importFrom gtools stars.pval
#' @importFrom stats reshape terms sd coef vcov pt
#' @importFrom lme4 fixef
#' @importFrom dplyr between

inteRplot <- function(model,pred,mod,outcome = 'outcome',xaxis = 'predictor',moderator = 'moderator',miny = 1,maxy = 5,xlabels=c('Low','High')) { # input is a dataset (x) that includes a grouping variable (group)

  model <- model
  pred <- pred
  modx <- mod
  mod <- mod

  if(class(model)=='lmerModLmerTest'){
    data <- model@frame
  }else if(class(model)=='lm'){
    data <- model$model
  }

  lmertest <- grepl('lmer', as.character(class(model)), fixed = TRUE)

   ######### LMER
  if(class(model)=='lmerModLmerTest'){
    dfmodel <- model@frame
    model <-  model
    pred <-  pred
    modx <- modx

    ivs <- labels(stats::terms(model))
    relevantInteractions <- c(paste(pred, ":", modx, sep = ""),
                              paste(modx, ":", pred, sep = ""))

    interactionsIn <- relevantInteractions[which(relevantInteractions %in% ivs)]

    if (!any(relevantInteractions %in% ivs)) {
      cat("There were no interactions in the plotSlopes object, so testSlopes can't offer any advice.\n")
      return(NULL)
    }

    ## Whew. We did not return, so let's get down to business.
    #modxVar <- model@frame[ , modx]#waarden van index_competentie OK
    meanmodx <- mean(dfmodel[,modx],na.rm=TRUE)
    sdmodx <- stats::sd(dfmodel[,modx],na.rm=TRUE)
    highmodx <- meanmodx+sdmodx
    lowmodx <- meanmodx-sdmodx
    names <- c('(m-sd)','(m)','(m+sd)')
    numbers <- c(lowmodx,meanmodx,highmodx)
    names(numbers) <- names
    modxVals <- numbers #waarden voor -SD, M, +SD OK
    bs <- stats::coef(model)
    V <- stats::vcov(model)


    bmodx <- NULL
    bpred <- lme4::fixef(model)[pred]

    bmodx <- lme4::fixef(model)[interactionsIn]
    bsimple <- bpred + bmodx * modxVals
    covbsimple <- cbind(1, modxVals^2, 2 * modxVals) %*%
      c(V[pred, pred], V[names(bmodx), names(bmodx)], V[pred, names(bmodx)])

    tbsimple <- bsimple/sqrt(covbsimple)
    dfresidual <-mean(unique(summary(model)$coefficient[,3]))
    dfresidual <- as.integer(dfresidual)
    testSlopes <- data.frame( modx = modxVals, b = bsimple,
                              se = sqrt(covbsimple), t = tbsimple,
                              p = 2 * stats::pt(abs(tbsimple),
                                         df = dfresidual, lower.tail = FALSE))



    colnames(testSlopes) <- c(deparse(modx), "slope", "Std. Error", "t value", "Pr(>|t|)")
    hey <- round(testSlopes,4)
    heyy <- data.table::as.data.table(hey,keep.rownames=TRUE)
    heyy


    # if (is.factor(modxVar)) {
    #   mcoef <- coef(model)
    #   modxContrastNames <- c(grep(pred,  grep(modx, names(mcoef), value = TRUE),  value=TRUE))
    #   slope <- mcoef[modxContrastNames] + coef(model)[pred]
    #
    #   ## 2013-02-19 Sunthud Pornprasertmanit spots bug here:
    #   ## Problem: diag doesn't work when argument is a single real number.
    #   ## Fix by inserting drop=FALSE (wrote a blog post about the "drop gotcha"
    #   seslope <- sqrt(as.vector(V[pred, pred, drop = FALSE]) +  diag(V[modxContrastNames, modxContrastNames, drop = FALSE]) + 2* V[modxContrastNames, pred, drop = FALSE])
    #
    #   modxContrastNames <- c(pred, modxContrastNames)
    #   slope <- c(mcoef[pred], slope)
    #   seslope <- c(sqrt(V[pred, pred]) , seslope)
    #   tbsimple <- slope/seslope
    #
    #   testSlopes <- data.frame(modx = modxContrastNames, b = slope,
    #                            se = seslope, t = tbsimple, p = 2 *
    #                              pt(abs(tbsimple), df =
    #                                   model$df.residual, lower.tail =
    #                                   FALSE))
    #
    #   row.names(testSlopes) <-  levels(model.frame(model)[ , modx])
    #
    #   colnames(testSlopes) <- c(deparse(modx), "slope",
    #                             "Std. Error", "t value", "Pr(>|t|)")
    #
    #   cat(paste("These are the straight-line \"simple slopes\" of the variable", pred, " \n for the selected moderator values. \n"))
    #   print(testSlopes)
    #   res <- list("hypotests" = testSlopes, pso = object)
    #   class(res) <- "testSlopes"
    #   return(invisible(res))
    # }

    ## Aha! We did not return. So this input is numeric. Continue.

  }else if(class(model)=='lm'){


    dfmodel <- model$model
    model <-  model
    pred <-  pred
    modx <- modx

    ivs <- labels(stats::terms(model))
    relevantInteractions <- c(paste(pred, ":", modx, sep = ""),
                              paste(modx, ":", pred, sep = ""))

    interactionsIn <- relevantInteractions[which(relevantInteractions %in% ivs)]

    if (!any(relevantInteractions %in% ivs)) {
      cat("There were no interactions in the plotSlopes object, so testSlopes can't offer any advice.\n")
      return(NULL)
    }

    ## Whew. We did not return, so let's get down to business.
    #modxVar <- model@frame[ , modx]#waarden van index_competentie OK
    meanmodx <- mean(dfmodel[,modx],na.rm=TRUE)
    sdmodx <- stats::sd(dfmodel[,modx],na.rm=TRUE)
    highmodx <- meanmodx+sdmodx
    lowmodx <- meanmodx-sdmodx
    names <- c('(m-sd)','(m)','(m+sd)')
    numbers <- c(lowmodx,meanmodx,highmodx)
    names(numbers) <- names
    modxVals <- numbers #waarden voor -SD, M, +SD OK
    bs <- stats::coef(model)
    V <- stats::vcov(model)

    bmodx <- NULL
    bpred <- bs[pred]
    bmodx <- bs[interactionsIn]

    bsimple <- bpred + bmodx * modxVals
    covbsimple <- cbind(1, modxVals^2, 2 * modxVals) %*%
      c(V[pred, pred], V[names(bmodx), names(bmodx)], V[pred, names(bmodx)])

    tbsimple <- bsimple/sqrt(covbsimple)
    dfresidual <-mean(unique(summary(model)$coefficient[,3]))
    dfresidual <- as.integer(dfresidual)
    testSlopes <- data.frame( modx = modxVals, b = bsimple,
                              se = sqrt(covbsimple), t = tbsimple,
                              p = 2 * stats::pt(abs(tbsimple),
                                         df = dfresidual, lower.tail = FALSE))



    colnames(testSlopes) <- c(deparse(modx), "slope", "Std. Error", "t value", "Pr(>|t|)")
    hey <- round(testSlopes,4)
    heyy <- data.table::as.data.table(hey,keep.rownames=TRUE)
    heyy


    ## If modx is a factor, only need to test a particular set of lines.
    # if (is.factor(modxVar)) {
    #   mcoef <- coef(model)
    #   modxContrastNames <- c(grep(pred,  grep(modx, names(mcoef), value = TRUE),  value=TRUE))
    #   slope <- mcoef[modxContrastNames] + coef(model)[pred]
    #
    #   ## 2013-02-19 Sunthud Pornprasertmanit spots bug here:
    #   ## Problem: diag doesn't work when argument is a single real number.
    #   ## Fix by inserting drop=FALSE (wrote a blog post about the "drop gotcha"
    #   seslope <- sqrt(as.vector(V[pred, pred, drop = FALSE]) +  diag(V[modxContrastNames, modxContrastNames, drop = FALSE]) + 2* V[modxContrastNames, pred, drop = FALSE])
    #
    #   modxContrastNames <- c(pred, modxContrastNames)
    #   slope <- c(mcoef[pred], slope)
    #   seslope <- c(sqrt(V[pred, pred]) , seslope)
    #   tbsimple <- slope/seslope
    #
    #   testSlopes <- data.frame(modx = modxContrastNames, b = slope,
    #                            se = seslope, t = tbsimple, p = 2 *
    #                              pt(abs(tbsimple), df =
    #                                   model$df.residual, lower.tail =
    #                                   FALSE))
    #
    #   row.names(testSlopes) <-  levels(model.frame(model)[ , modx])
    #
    #   colnames(testSlopes) <- c(deparse(modx), "slope",
    #                             "Std. Error", "t value", "Pr(>|t|)")
    #
    #   cat(paste("These are the straight-line \"simple slopes\" of the variable", pred, " \n for the selected moderator values. \n"))
    #   print(testSlopes)
    #   res <- list("hypotests" = testSlopes, pso = object)
    #   class(res) <- "testSlopes"
    #   return(invisible(res))
    # }

  }

  simslopes <- as.data.frame(heyy)
  simslopes$est <- format(round(simslopes$slope,2), nsmall = 2)
  simslopes$stars <- gtools::stars.pval(simslopes$`Pr(>|t|)`)
  lowslope <- paste('-1 SD (',simslopes[which(simslopes[,2]==min(simslopes[,2])),'est'],
                     simslopes[which(simslopes[,2]==min(simslopes[,2])),'stars'],')',sep="")
  # lowslope <- paste(format(round(simslopes[which(simslopes$`Value of mod`==min(simslopes$`Value of mod`)),'est'],2), nsmall = 2),
  #                   simslopes[which(simslopes$`Value of mod`==min(simslopes$`Value of mod`)),'stars'],sep="")
  lowslope <- sub('0', '', lowslope)

  highslope <- paste('+1 SD (',simslopes[which(simslopes[,2]==max(simslopes[,2])),'est'],
                     simslopes[which(simslopes[,2]==max(simslopes[,2])),'stars'],')',sep="")
  # highslope <- paste(format(round(simslopes[which(simslopes$`Value of mod`==max(simslopes$`Value of mod`)),'est'],2), nsmall = 2),
  #                    simslopes[which(simslopes$`Value of mod`==max(simslopes$`Value of mod`)),'stars'],sep="")
  highslope <- sub('0', '', highslope)


  X <- pred
  m <- modx

  if(class(model)=='lmerModLmerTest'){
    data <- model@frame
  }else if(class(model)=='lm'){
  data <- model$model
  }

  mod.out <- summary(model)
  varcov <- stats::vcov(mod.out)
  coef1 <- mod.out$coefficients[which(rownames(mod.out$coefficients)==X)]

  ivs <- labels(stats::terms(model))
  relevantInteractions <- c(paste(X, ":", m, sep = ""),
                            paste(m, ":", X, sep = ""))
  interactionsIn <- relevantInteractions[which(relevantInteractions %in% ivs)]

  coef3 <- mod.out$coefficients[which(rownames(mod.out$coefficients)==interactionsIn)]
  var_coef1 <- varcov[X, X]
  var_coef3 <- varcov[interactionsIn, interactionsIn]
  cov_coefs1_3 <- varcov[X, interactionsIn]

  a <- (1.96^2) * (var_coef3) - (coef3)^2
  b <- 2 * ((1.96^2) * (cov_coefs1_3) - (coef1 * coef3))
  c <- (1.96^2) * var_coef1 - (coef1)^2
  x1 <- (-b - sqrt((b^2) - (4 * a * c)))/(2 * a)
  x2 <- (-b + sqrt((b^2) - (4 * a * c)))/(2 * a)

  mm <- c("It was not possible to calculate regions of non-significance. The difference between slopes might not be statistically significant")
  if (((b^2) - 4 * a * c) < 0) {
    warning(mm)
  }

  min <- min(data[,mod],na.rm=TRUE)
  minslope <- paste('minimum: ',format(round(min,2), nsmall = 2),sep="")
  max <- max(data[,mod],na.rm=TRUE)
  maxslope <- paste('maxmimum: ',format(round(max,2), nsmall = 2),sep="")
  minsd <- mean(data[,mod],na.rm=TRUE)-stats::sd(data[,mod],na.rm=TRUE)
  maxsd <- mean(data[,mod],na.rm=TRUE)+stats::sd(data[,mod],na.rm=TRUE)
  intervaljn <- c(x1,x2)
  intervaljn <- sort(intervaljn)
  lower <- as.numeric(intervaljn[1])
  higher <- as.numeric(intervaljn[2])

  minjohney <- paste('lower boundary: ]',format(round(lower,2), nsmall = 2),sep="")
  maxjohney <- paste('higher boundary: ',format(round(higher,2), nsmall = 2),'[',sep="")

  numbers <- data.frame(numbers = c(min,max,lower,higher,minsd,maxsd),
                        labels = c(minslope,maxslope,
                                   minjohney,maxjohney,
                                   lowslope,highslope),
                        labelss = c('minimum','maximum',
                                   'lower significance','higher significance',
                                   '-1 SD','+1 SD'),
                        lines = c('solid','solid','solid','solid','longdash','solid'),
                        shape = c(NA,NA,NA,NA,16,16),
                        colors = c('#709AE1','#FD7446',
                                   'darkgrey', 'darkgrey',
                                   'black','black'))

  numbers <- numbers[order(numbers$numbers),]


  x <- c(numbers[which(numbers$labelss=="lower significance"),'numbers'],
         numbers[which(numbers$labelss=="higher significance"),'numbers'])
  intervalcheck <- dplyr::between(x,
          numbers[which(numbers$labelss=="minimum"),'numbers'],
          numbers[which(numbers$labelss=="maximum"),'numbers'])
  intervalcheck <- paste(as.character(intervalcheck)[1],as.character(intervalcheck)[2],sep="")

  if (intervalcheck == "FALSETRUE") {
    numbers[which(numbers$labelss=="lower significance"),'numbers'] <- NA
    numbers <- numbers[!is.na(numbers$numbers),]
    print('lowest values not significant')
  } else if (intervalcheck == "TRUEFALSE"){
    numbers[which(numbers$labelss=="higher significance"),'numbers'] <- NA
    numbers <- numbers[!is.na(numbers$numbers),]
    print('highest values not significant')
  } else if (intervalcheck == "TRUETRUE"){
    numbers <- numbers
  } else if (intervalcheck == "FALSEFALSE"){
    numbers[which(numbers$labelss=="lower significance"),'numbers'] <- NA
    numbers[which(numbers$labelss=="higher significance"),'numbers'] <- NA
    numbers <- numbers[!is.na(numbers$numbers),]
  }

  lines <- as.numeric(numbers$numbers)
  lines <- round(lines,2)

  minsdpred <- mean(data[,pred],na.rm=TRUE)-stats::sd(data[,pred],na.rm=TRUE)
  maxsdpred <- mean(data[,pred],na.rm=TRUE)+stats::sd(data[,pred],na.rm=TRUE)
  linespred <- c(minsdpred,maxsdpred)

  linesvalues <- paste(mod,'[',paste(lines,sep="", collapse=","),']',sep="")
  predvalues <- paste(pred,'[',paste(linespred,sep="", collapse=","),']',sep="")

  ggpredictions_ols3 = data.frame(ggeffects::ggeffect(model,
                                           c(predvalues,
                                             linesvalues)))
  ggpredictions_ols3$group <- as.character(ggpredictions_ols3$group)
  ggpredictions_ols3$group <- as.numeric(ggpredictions_ols3$group)
  ggpredictions_ols3$groupname <- as.factor(ggpredictions_ols3$group)
  levels(ggpredictions_ols3$groupname) <- numbers$labelss
  ggpredictions_ols3$predname <- as.factor(ggpredictions_ols3$x)
  levels(ggpredictions_ols3$predname) <- c('low','high')

  if (intervalcheck == "FALSETRUE") {
    region <- ggpredictions_ols3[which(ggpredictions_ols3$groupname=='minimum'|
                                         ggpredictions_ols3$groupname=='higher significance'),c('x','predicted','groupname')]
    region$ID <- c('low','low','high','high')
    dfwide <- stats::reshape(region, # name of the dataset
                      idvar = "ID",  # name of the grouping variable including dependent variance
                      timevar = "groupname", # time variable
                      direction = "wide")
    ggpredictions_ols3$y1 <- dfwide$`predicted.minimum`[match(ggpredictions_ols3$predname, dfwide$ID)]
    ggpredictions_ols3$y2 <- dfwide$`predicted.higher significance`[match(ggpredictions_ols3$predname, dfwide$ID)]

  } else if (intervalcheck == "TRUEFALSE"){
    region <- ggpredictions_ols3[which(ggpredictions_ols3$groupname=='lower significance'|
                                         ggpredictions_ols3$groupname=='maximum'),c('x','predicted','groupname')]
    region$ID <- c('low','low','high','high')
    dfwide <- stats::reshape(region, # name of the dataset
                      idvar = "ID",  # name of the grouping variable including dependent variance
                      timevar = "groupname", # time variable
                      direction = "wide")
    ggpredictions_ols3$y1 <- dfwide$`predicted.lower significance`[match(ggpredictions_ols3$predname, dfwide$ID)]
    ggpredictions_ols3$y2 <- dfwide$`predicted.maximum`[match(ggpredictions_ols3$predname, dfwide$ID)]

  } else if (intervalcheck == "TRUETRUE"){
    region <- ggpredictions_ols3[which(ggpredictions_ols3$groupname=='lower significance'|
                                         ggpredictions_ols3$groupname=='higher significance'),c('x','predicted','groupname')]
    region$ID <- c('low','low','high','high')
    dfwide <- stats::reshape(region, # name of the dataset
                      idvar = "ID",  # name of the grouping variable including dependent variance
                      timevar = "groupname", # time variable
                      direction = "wide")
    ggpredictions_ols3$y1 <- dfwide$`predicted.lower significance`[match(ggpredictions_ols3$predname, dfwide$ID)]
    ggpredictions_ols3$y2 <- dfwide$`predicted.higher significance`[match(ggpredictions_ols3$predname, dfwide$ID)]
  } else if (intervalcheck == "FALSEFALSE"){
    print('no interval detected within range')
  }

  region <- ggpredictions_ols3[which(ggpredictions_ols3$groupname=='minimum'|
                                       ggpredictions_ols3$groupname=='maximum'),
                               c('x','predicted','groupname')]
  region$ID <- c('low','low','high','high')
  dfwide <- stats::reshape(region, # name of the dataset
                    idvar = "ID",  # name of the grouping variable including dependent variance
                    timevar = "groupname", # time variable
                    direction = "wide")
  ggpredictions_ols3$y3 <- dfwide$`predicted.minimum`[match(ggpredictions_ols3$predname, dfwide$ID)]
  ggpredictions_ols3$y4 <- dfwide$`predicted.maximum`[match(ggpredictions_ols3$predname, dfwide$ID)]


  if (intervalcheck == "FALSEFALSE") {
    xvalues <- c(rep(ggpredictions_ols3[which(ggpredictions_ols3$x == min(ggpredictions_ols3$x)),'x'],length.out = 50),
                 rep(ggpredictions_ols3[which(ggpredictions_ols3$x == max(ggpredictions_ols3$x)),'x'],length.out = 50))


    yvalues <- c(seq(ggpredictions_ols3[which(ggpredictions_ols3$group == min(ggpredictions_ols3$group) &
                                                ggpredictions_ols3$x == min(ggpredictions_ols3$x)  ),'predicted'],
                     ggpredictions_ols3[which(ggpredictions_ols3$group == max(ggpredictions_ols3$group) &
                                                ggpredictions_ols3$x == min(ggpredictions_ols3$x)  ),'predicted'],length.out=50),
                 seq(ggpredictions_ols3[which(ggpredictions_ols3$group == min(ggpredictions_ols3$group) &
                                                ggpredictions_ols3$x == max(ggpredictions_ols3$x)  ),'predicted'],
                     ggpredictions_ols3[which(ggpredictions_ols3$group == max(ggpredictions_ols3$group) &
                                                ggpredictions_ols3$x == max(ggpredictions_ols3$x)  ),'predicted'],length.out=50))
    groupvalues <- c(seq(ggpredictions_ols3[which(ggpredictions_ols3$group == min(ggpredictions_ols3$group) &
                                                    ggpredictions_ols3$x == min(ggpredictions_ols3$x)  ),'group'],
                         ggpredictions_ols3[which(ggpredictions_ols3$group == max(ggpredictions_ols3$group) &
                                                    ggpredictions_ols3$x == min(ggpredictions_ols3$x)  ),'group'],length.out=50),
                     seq(ggpredictions_ols3[which(ggpredictions_ols3$group == min(ggpredictions_ols3$group) &
                                                    ggpredictions_ols3$x == max(ggpredictions_ols3$x)  ),'group'],
                         ggpredictions_ols3[which(ggpredictions_ols3$group == max(ggpredictions_ols3$group) &
                                                    ggpredictions_ols3$x == max(ggpredictions_ols3$x)  ),'group'],length.out=50))
    values <- as.data.frame(cbind(xvalues,yvalues,groupvalues))



     fig <- ggplot2::ggplot(data = ggpredictions_ols3,ggplot2::aes(x = x,
                                          y = predicted,
                                          group = groupname,
                                          linetype=groupname,
                                          color = groupname)) +

      # geom_path(data = values, aes(x = xvalues,
      #                              y = yvalues,
      #                              color = groupvalues,
      #                              group = groupvalues) ,linewidth=2) +
      # guides(color = "none") +
      #  scale_color_gradient2(low = '#709AE1', high = '#FD7446', mid = 'white',
      #                        midpoint = mean(ggpredictions_ols3$group,na.rm=TRUE),
      #                        limits=c(min(ggpredictions_ols3$group,na.rm=TRUE),
      #                                 max(ggpredictions_ols3$group,na.rm=TRUE))
      #  ) +
     ggplot2::geom_ribbon(ggplot2::aes(x = x,
                     ymin = y3,
                     ymax = y4),
                 fill = "#ebedf0",
                 show.legend = FALSE,
                 colour = NA)+

       ggplot2::geom_line(linewidth=0.5)+

       ggplot2::scale_linetype_manual(moderator,
                            labels= c(numbers$labels),
                            values = c(numbers$lines))+
       ggplot2::scale_color_manual(moderator,
                             labels= c(numbers$labels),
                             values = c(numbers$colors))+

     ggthemes::theme_few()+

       ggplot2::scale_x_continuous(breaks=c(minsdpred,maxsdpred),
                         labels=xlabels,
                         expand = c(0.2, 0))+
       ggplot2::coord_cartesian(ylim=c(miny,maxy))+
       ggplot2::theme(legend.position="right",
            # plot.subtitle=element_text(size=9, color="black"),
            plot.caption = ggplot2::element_text(hjust = 0))+
       ggplot2::labs(
        #tag=moderator,
        #subtitle = paste(xaxis," by ",moderator," in prediction of ",outcome,sep=""),
        y = outcome,
        x = paste(xaxis,'',sep='\n'),
        caption = paste('grey zone = range of values',
                        paste('dark grey zone = insignificant slopes of moderator [',round(lower,2),', ',round(higher,2),']',sep=""),
                        sep="\n")
      )

  } else{


    xvalues <- c(rep(ggpredictions_ols3[which(ggpredictions_ols3$x == min(ggpredictions_ols3$x)),'x'],length.out = 50),
                 rep(ggpredictions_ols3[which(ggpredictions_ols3$x == max(ggpredictions_ols3$x)),'x'],length.out = 50))


    yvalues <- c(seq(ggpredictions_ols3[which(ggpredictions_ols3$group == min(ggpredictions_ols3$group) &
                               ggpredictions_ols3$x == min(ggpredictions_ols3$x)  ),'predicted'],
        ggpredictions_ols3[which(ggpredictions_ols3$group == max(ggpredictions_ols3$group) &
                                   ggpredictions_ols3$x == min(ggpredictions_ols3$x)  ),'predicted'],length.out=50),
        seq(ggpredictions_ols3[which(ggpredictions_ols3$group == min(ggpredictions_ols3$group) &
                                       ggpredictions_ols3$x == max(ggpredictions_ols3$x)  ),'predicted'],
            ggpredictions_ols3[which(ggpredictions_ols3$group == max(ggpredictions_ols3$group) &
                                       ggpredictions_ols3$x == max(ggpredictions_ols3$x)  ),'predicted'],length.out=50))
    groupvalues <- c(seq(ggpredictions_ols3[which(ggpredictions_ols3$group == min(ggpredictions_ols3$group) &
                                                ggpredictions_ols3$x == min(ggpredictions_ols3$x)  ),'group'],
                     ggpredictions_ols3[which(ggpredictions_ols3$group == max(ggpredictions_ols3$group) &
                                                ggpredictions_ols3$x == min(ggpredictions_ols3$x)  ),'group'],length.out=50),
                 seq(ggpredictions_ols3[which(ggpredictions_ols3$group == min(ggpredictions_ols3$group) &
                                                ggpredictions_ols3$x == max(ggpredictions_ols3$x)  ),'group'],
                     ggpredictions_ols3[which(ggpredictions_ols3$group == max(ggpredictions_ols3$group) &
                                                ggpredictions_ols3$x == max(ggpredictions_ols3$x)  ),'group'],length.out=50))
    values <- as.data.frame(cbind(xvalues,yvalues,groupvalues))


   fig <- ggplot2::ggplot(data = ggpredictions_ols3,
                          ggplot2::aes(x = x,
                         y = predicted,
                         group = groupname,
                         color = groupname,
                         linetype = groupname)) +

      # geom_path(data = values, aes(x = xvalues,
      #                              y = yvalues,
      #                              color = groupvalues,
      #                              group = groupvalues) ,linewidth=2) +
      # guides(color = "none") +
      #
      # scale_color_gradient2(low = '#709AE1', high = '#FD7446', mid = 'white',
      #                       midpoint = mean(ggpredictions_ols3$group,na.rm=TRUE),
      #                       limits=c(min(ggpredictions_ols3$group,na.rm=TRUE),
      #                                max(ggpredictions_ols3$group,na.rm=TRUE))
      #                       ) +
   ggplot2::geom_ribbon(aes(x = x,
                   ymin = y3,
                   ymax = y4),
               fill = "#ebedf0",
               show.legend = FALSE,
               colour = NA)+
     ggplot2::geom_ribbon(aes(x = x,
                     ymin = y1,
                     ymax = y2),
                 fill = "darkgrey",
                 show.legend = FALSE,
                 colour = NA)+
     ggplot2::geom_line(linewidth=0.5)+
      #geom_point(size = 1)+
     ggplot2::scale_linetype_manual(moderator,
                            labels= c(numbers$labels),
                            values = c(numbers$lines))+
     ggplot2::scale_color_manual(moderator,
                        labels=c(numbers$labels),
                        values = c(numbers$colors))+


      ggthemes::theme_few()+
     ggplot2::scale_x_continuous(breaks=c(minsdpred,maxsdpred),
                         labels=xlabels,
                         expand = c(0.2, 0))+
     ggplot2::coord_cartesian(ylim=c(miny,maxy))+
     ggplot2::theme(legend.position="right",
           # plot.subtitle=element_text(size=9, color="black"),
            plot.caption = ggplot2::element_text(hjust = 0))+
     ggplot2::labs(
        #tag=moderator,
        #subtitle = paste(xaxis," by ",moderator," in prediction of ",outcome,sep=""),
        y = outcome,
        x = paste(xaxis,'',sep='\n'),
        caption = paste('grey zone = range of values',
                        paste('dark grey zone = insignificant slopes of moderator [',round(lower,2),', ',round(higher,2),']',sep=""),
                        sep="\n")
      )
  }

  return(fig)
}

