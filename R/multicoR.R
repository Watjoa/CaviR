#' @title multicoR
#'
#' @description This function plots a correlation table in APA style with descriptive statistics.
#'
#' @param dataset Your dataset with the variable in order you want to present them
#' @return A html correlation matrix with significance stars, Note and descriptive statistics
#' @export
#' @importFrom multilevelTools iccMixed
#' @importFrom psych statsBy describe
#' @importFrom papaja printnum
#' @importFrom flextable flextable add_footer_lines theme_zebra autofit

multicoR<- function(dataset) { # input is a dataset (x) that includes a grouping variable (group)

  dataset <- as.data.frame(dataset)

  group <- colnames(dataset)[1]
  names(dataset)[names(dataset) == group] <- 'groupVAR'

  corr <- dataset[,-1]
  corr <- as.data.frame(sapply(corr,as.numeric))

  corr$Group <- dataset[,'groupVAR']
  sat.stats <- suppressWarnings(psych::statsBy(corr, "Group", alpha=.05,
                       cors = TRUE, method="pearson"))
  sat.stats.between <- as.matrix(round(sat.stats$rbg,2))
  sat.stats.between.p <- as.matrix(round(sat.stats$pbg,2))

  sat.stats.within <- as.matrix(round(sat.stats$rwg,2))
  sat.stats.within.p <- as.matrix(round(sat.stats$pwg,2))

  sat.stats.between[upper.tri(sat.stats.between)] <- sat.stats.within[upper.tri(sat.stats.within)]
  rownames(sat.stats.between) = gsub(".bg", "", rownames(sat.stats.between))

  #define notions for between-level significance levels
  mystarspbg <- ifelse(sat.stats.between.p < .001, "***"
                       , ifelse(sat.stats.between.p < .01, "**"
                                , ifelse(sat.stats.between.p < .05, "*"
                                         , ifelse(sat.stats.between.p < .10, "", ""))))
  mystarspwg <- ifelse(sat.stats.within.p < .001, "***"
                       , ifelse(sat.stats.within.p < .01, "**"
                                , ifelse(sat.stats.within.p < .05, "*"
                                         , ifelse(sat.stats.within.p < .10, "", ""))))

  #round r, define new matrix Rnewbg with the between-level correlations from rnd and paste mystars
  rnbtw  <- papaja::printnum(sat.stats.between, gt1 = FALSE, digits = 2)  #round, drop leading 0 - Thanks CRSH!
  rnwht  <- papaja::printnum(sat.stats.within, gt1 = FALSE, digits = 2)  #round, drop leading 0 - Thanks CRSH!

  Rnewbtw <- matrix(paste(rnbtw, mystarspbg, sep=""), ncol=ncol(rnbtw))
  Rnewwth <- matrix(paste(rnwht, mystarspwg, sep=""), ncol=ncol(rnwht))
  Rnew <- matrix(nrow = nrow(Rnewbtw), ncol = ncol(Rnewbtw))

  #remove 1.0 correlations from diagonal  and set the strings
  diag(Rnewbtw) <- ""
  Rnew[lower.tri(Rnew)] <- Rnewbtw[lower.tri(Rnewbtw)] #between-level correlations above diagonal
  Rnew[upper.tri(Rnew)] <- Rnewwth[upper.tri(Rnewwth)] #between-level correlations above diagonal

  rownames(Rnew) <- paste(1:ncol(rnbtw), ". ",colnames(rnbtw), sep="") #Row names are numbers and variable names
  colnames(Rnew) <- paste(1:ncol(rnbtw), ".", sep="") #Column names are just

  vars <- colnames(corr)[1:(length(corr)-1)]
  y <- NULL
  for(i in 1:length(vars)){
  dataset <- as.data.frame(dataset)
  icclistt <- multilevelTools::iccMixed(dv = vars[i],id = "groupVAR", dataset)
  icclistt <- round(icclistt[which(icclistt$Var=="groupVAR"),'ICC'],2)
 # icclistt <- format(round(icclistt,2), nsmall = 2)
  y <- rbind(y, icclistt)
  }

  Rnew <- (cbind(round(psych::describe(corr)$mean,2),
                                 round(psych::describe(corr)$sd,2),
                                 y$ICC,
                                 Rnew)) #describe x, M sD - put them in the matrix
  colnames(Rnew)[1:3] <- c("M","SD","ICC") #New column headers

  names <- rownames(Rnew)
  names <- gsub('.bg','',names)

  Rnew[Rnew == "NANA" | is.na(Rnew)] <- ""

  Rnew <- as.data.frame(Rnew)
  Rnew <- as.data.frame(cbind(names,Rnew))
  names(Rnew)[1] <- ' '
  Rnew <- flextable::flextable(Rnew)
  info <- paste('Note. *** p <.001, ** p <. 01, * p < .05')
  Rnew <- flextable::add_footer_lines(Rnew, info)
  Rnew <- flextable::theme_zebra(Rnew)
  Rnew <- flextable::autofit(Rnew)
  return( Rnew)

}
