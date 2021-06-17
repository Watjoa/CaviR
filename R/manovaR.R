#' @title manovaR
#'
#' @description manova table with statistics
#'
#' @param model your lm model
#' @return Six plot with each a different method to check the model diagnostics
#' @export
#' @importFrom psych describeBy
#' @importFrom gtools stars.pval
#' @importFrom psych cohen.d
#' @importFrom knitr kable
#' @importFrom kableExtra footnote
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra kable_classic_2
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra kable_paper
#' @importFrom dplyr "%>%"



manovaR<- function(variables,group,dataset) { # input is a dataset (x) that includes a grouping variable (group)

groupmanova <- as.factor(dataset[,c(group)])
dfmanova <- dataset[,variables]
table3 <- psych::describeBy(dfmanova,
                            group=groupmanova,
                            mat=TRUE,type=3,digits=2)

table34 <- table3[,c('group1','mean')]

table35 <- data.frame(split(c(table34$mean),table34$group1))

colnames(table35) <- levels(groupmanova)
rownames(table35) <- names(dfmanova)



models <- lapply(names(dfmanova), function(x) {
  lm(substitute(i~ groupmanova, list(i = as.name(x))), data = df)})

anovatab <- lapply(models, anova)
anovatabb <- data.frame(matrix(unlist(anovatab),
                               nrow=length(anovatab), byrow=T))
fvalue <- anovatabb[,c(7,9)]
fvalue <- round(fvalue,2)
colnames(fvalue) <- c('F-value','p-value')
stars <- gtools::stars.pval(fvalue$`p-value`)
fvalue$stars <- stars
colnames(fvalue) <- c('F-value','p-value', ' ')

etasq <- lapply(models, sjstats::eta_sq)
anovatabbetasq <- matrix(unlist(etasq), nrow=length(etasq), byrow=T)
fvalueetasq <- as.numeric(anovatabbetasq[,2])

fvalueetasq <- round(fvalueetasq,2)
fvalueetasq <- as.data.frame(fvalueetasq)
colnames(fvalueetasq) <- c('eta-squared')

tabelcohen <- cohen.d(dfmanova, groupmanova ,alpha=.05,std=TRUE)
tabellcohen <- as.data.frame(tabelcohen[1])
tabellcohen <- round(tabellcohen[2],2)
colnames(tabellcohen) <- c("Cohen's D")


total <- cbind(table35,fvalue,fvalueetasq,tabellcohen)
total <- total[!sapply(total, function(x) all(x == ""))]


test.label <- paste0('Wilks Lambda = ',anova$Wilks[2], ',',
                     "F", '(',anova$`num Df`[2],',', anova$`den Df`[2],')',
                     " = ", anova$`approx F`[2],
                     " , p = ", anova$`Pr(>F)`[2]
)

test.label <- paste('Wilks Lambda = ',anova$Wilks[2], ',',
                    "F", '(',anova$`num Df`[2],',', anova$`den Df`[2],')',
                    " = ", anova$`approx F`[2],
                    " , p = ", anova$`Pr(>F)`[2]
)

result <- kable(total, "html") %>%
  kable_paper(full_width = F) %>%
  kable_styling(bootstrap_options = c("hover","condensed", "responsive"),
                fixed_thead = T,font_size = 15) %>%
  kable_classic_2()%>%
  footnote(general = test.label,
           footnote_as_chunk = T, title_format = c("italic")
  )
return(result)
}
