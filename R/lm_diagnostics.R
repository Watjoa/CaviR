#' @title coRtable
#'
#' @description This function plots a correlation table in APA style with descriptive statistics.
#'
#' @param dataset Your dataset with the variable in order you want to present them
#' @return A html correlation matrix with significance stars, Note and descriptive statistics
#' @export
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 stat_smooth
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 geom_abline
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 labs
#' @importFrom car vif
#' @importFrom cowplot plot_grid

lm.diagnostics <- function(model) {

  # Multicollinearity
  vif <- as.data.frame(car::vif(model))
  vif$variables <- rownames(vif)
  colnames(vif)<-c('vif','variables')

  p <- ggplot(data=vif, aes(x=variables, y=vif)) +
    geom_bar(stat="identity",fill='#2F3C4D')+theme_classic()+
    geom_hline(yintercept=10,linetype = 'dashed',color='darkred')+
    geom_text(data=data.frame(x=0,y=11), aes(x, y), label='  tolerable',hjust=0,color='darkred')+
    geom_hline(yintercept=5,linetype = 'dashed',color='darkgreen')+
    geom_text(data=data.frame(x=0,y=6), aes(x, y), label='  good',hjust=0,color='darkgreen')+
    ggtitle("1. Multicollinearity")

  # Residuals vs Fitted Plot
  p1 <- ggplot(model, aes(.fitted, .resid))+
    geom_point()+
    stat_smooth(method="loess")+
    geom_hline(yintercept=0, col="red", linetype="dashed")+
    xlab("Fitted values")+
    ylab("Residuals")+
    theme_bw()+
    ggtitle("2. Residual vs Fitted Plot")

  # Normal Q-Q plot
  p2 <- ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+
    geom_point(na.rm = TRUE)+
    theme_bw()+
    ggtitle("3. Normal Q-Q")

  # Scale Location
  p3 <- ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+
    geom_point(na.rm=TRUE)+
    stat_smooth(method="loess", na.rm = TRUE)+
    xlab("Fitted Value")+
    ylab(expression(sqrt("|Standardized residuals|")))+
    theme_bw()+
    ggtitle('4. Scale-Location')

  # Cook's distance
  p4 <- ggplot(model, aes(seq_along(.cooksd), .cooksd))+
    geom_bar(stat="identity", position="identity")+
    xlab("Obs. Number")+
    ylab("Cook's distance")+
    theme_bw()+
    ggtitle("5. Cook's distance")

  p5 <- ggplot(model, aes(.hat, .stdresid))+
    geom_point(aes(size=.cooksd), na.rm=TRUE)+
    stat_smooth(method="loess", na.rm=TRUE)+
    xlab("Leverage")+
    ylab("Standardized Residuals")+
    scale_size_continuous("Cook's Distance", range=c(1,5))+
    theme_bw()+
    theme(legend.position="bottom")+
    ggtitle("6. Residual vs Leverage Plot")

  # Cook's distance versus Leverage
  p6 <- ggplot(model, aes(.hat, .cooksd))+
    geom_point(na.rm=TRUE)+
    stat_smooth(method="loess", na.rm=TRUE)+
    xlab("Leverage hii")+
    ylab("Cook's Distance")+
    geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")+
    theme_bw()+
    ggtitle("7. Cook's dist vs Leverage hii/(1-hii)")

  # Residuals distribution
  df <- as.data.frame(residuals(model))
  colnames(df) <- 'residuals'
  p7 <- ggplot(df, aes(x = residuals)) +
    geom_density(color="darkblue", fill="lightblue")+
    stat_function(fun = dnorm, args = list(mean = mean(df$residuals), sd = sd(df$residuals)))+
    labs(x='Residuals',y="Residuals")+
    ggtitle("8. Distribution residuals")+
    theme_bw()

  plot <- plot_grid(p,p1,p2,p3,p4,p5,p6,p7, nrow = 4)
  return(plot)

}

