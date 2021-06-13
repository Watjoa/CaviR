#' @title coRtable
#'
#' @description This function plots a correlation table in APA style with descriptive statistics.
#'
#' @param dataset Your dataset with the variable in order you want to present them
#' @return A html correlation matrix with significance stars, Note and descriptive statistics
#' @export
#' @importFrom psych corr.test
#' @importFrom psych describe
#' @importFrom knitr kable
#' @importFrom dplyr "%>%"


coRtable<- function(dataset) { # input is a dataset (x) that includes a grouping variable (group)

  rbg <- psych::corr.test(x)$r
  pbg <- psych::corr.test(x)$p

  # variable names are variable names from dataset excluding grouping variable
  varnames <- colnames(x)

  #define notions for between-level significance levels
  mystarsbg <- ifelse(pbg < .001, "***"
                      , ifelse(pbg < .01, "**"
                               , ifelse(pbg < .05, "*"
                                        , ifelse(pbg < .10, "", ""))))

  #round r, define new matrix Rnewbg with the between-level correlations from rnd and paste mystars
  rndbg  <- papaja::printnum(rbg, gt1 = FALSE, digits = 2)  #round, drop leading 0 - Thanks CRSH!
  Rnewbg <- matrix(paste(rndbg, mystarsbg, sep=""), ncol=ncol(rndbg))

  Rnew <- matrix(nrow = nrow(Rnewbg), ncol = ncol(Rnewbg))

  #remove 1.0 correlations from diagonal  and set the strings
  diag(Rnew) <- ""
  Rnew[lower.tri(Rnew)] <- Rnewbg[lower.tri(Rnewbg)] #between-level correlations above diagonal

  rownames(Rnew) <- paste(1:ncol(rndbg), ". ",varnames, sep="") #Row names are numbers and variable names
  colnames(Rnew) <- paste(1:ncol(rndbg), ".", sep="") #Column names are just

  Rnew <- cbind(round(psych::describe(x)$mean,2),round(psych::describe(x)$sd,2), Rnew) #describe x, M sD - put them in the matrix
  colnames(Rnew)[1:2] <- c("M","SD") #New column headers

  Rnew[Rnew == "NANA" | is.na(Rnew)] <- ""

  result <- kable(Rnew, "html") %>%
    kable_paper(full_width = F) %>%
    kable_styling(bootstrap_options = c("hover","condensed", "responsive"),
                  fixed_thead = T,font_size = 15) %>%
    kable_classic_2()%>%
    column_spec(2, background = "lightgrey")%>%
    column_spec(3, background = "lightgrey") %>%
    footnote(general = "",
             threeparttable = F,
             general_title = "Note. *** p <.001, ** p <. 01, * p < .05", title_format = "italic")
  return(result)

}
