#' @title multicoR
#'
#' @description This function plots a correlation table in APA style with descriptive statistics.
#'
#' @param dataset Your dataset with the variable in order you want to present them
#' @return A html correlation matrix with significance stars, Note and descriptive statistics
#' @export
#' @importFrom psych statsBy describe
#' @importFrom papaja printnum
#' @importFrom performance icc
#' @importFrom lme4 lmer
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

  vars <- colnames(corr)[1:(length(corr)-1)]
  y <- NULL

for(i in 1:length(vars)){
    dataset <- as.data.frame(dataset)
    modeldata <- dataset[,c('groupVAR',vars[i])]
    colnames(modeldata) <- c('groupVAR','outcome')
    modeldata$outcome <- scale(modeldata$outcome)
    modeldata <- na.omit(modeldata)
    tryCatch({
      iccmodel <- suppressWarnings(lme4::lmer(outcome ~ 1 + (1|groupVAR), data=modeldata))
      icclistt <- performance::icc(iccmodel)
      icclistt <- round(icclistt$ICC_adjusted,2)
    }, error=function(e){icclistt <- NULL})
    y <- rbind(y, icclistt)
  }

  y <- as.data.frame(y)
  colnames(y) <- 'ICC'
  rownames(y) <- vars

  Rnewbtw <- matrix(paste(rnbtw, mystarspbg, sep=""), ncol=ncol(rnbtw))
  Rnewwth <- matrix(paste(rnwht, mystarspwg, sep=""), ncol=ncol(rnwht))
  Rnew <- matrix(nrow = nrow(Rnewbtw), ncol = ncol(Rnewbtw))

  #remove 1.0 correlations from diagonal  and set the strings
  diag(Rnewbtw) <- ""

  if(length(which(y$ICC==1.00))>0){
    Rnewwth[,which(y$ICC==1.00)] <- NA
  } else if(length(which(y$ICC==0.00))>0) {
    Rnewbtw[which(y$ICC==0.00),] <- NA
  } else {y <- y}

  Rnew[upper.tri(Rnew)] <- Rnewwth[upper.tri(Rnewwth)] #between-level correlations above diagonal
  Rnew[lower.tri(Rnew)] <- Rnewbtw[lower.tri(Rnewbtw)] #within-level correlations above diagonal

  rownames(Rnew) <- paste(1:ncol(rnbtw), ". ",colnames(rnbtw), sep="") #Row names are numbers and variable names
  colnames(Rnew) <- paste(1:ncol(rnbtw), ".", sep="") #Column names are just

  Rnew <- suppressWarnings(cbind(round(psych::describe(corr)$mean,2),
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
