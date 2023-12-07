#' @title clusteRs
#'
#' @description overview of indices for number of clusters
#' @param dataset dataset
#' @return overview of indices for number of clusters
#' @export
#' @importFrom factoextra get_dist get_clust_tendency hkmeans fviz_nbclust
#' @importFrom stats hclust dist
#' @importFrom cluster agnes silhouette
#' @importFrom purrr map_dbl
#' @importFrom ggplot2 ggplot position_dodge unit element_blank
#' @importFrom NbClust NbClust
#' @importFrom cowplot plot_grid
#' @importFrom rstatix gather

clusteRs <- function(dataset){
  # functie print: STILL MISSING VALuES IN CLUsTER VARIABLES
  dff <- dataset
  if(sum(is.na(dff))>0){
    print('There are still missing values in the dataset')
    dff <- NULL
  } else{
    dff <- scale(dff,scale=TRUE)
  }

  dff <- as.data.frame(dff)

  # dismatrix <- factoextra::get_dist(dff, method= "euclidean")
  # linkage.average  <- stats::hclust(d= dismatrix, method = "average")
  # linkage.single   <- stats::hclust(d= dismatrix, method = "single")
  # linkage.complete <- stats::hclust(d= dismatrix, method = "complete")
  # linkage.ward     <- stats::hclust(d= dismatrix, method = "ward.D2")
  #
  # m <- c( "average", "single", "complete", "ward" )
  # names(m) <- c( "average", "single", "complete", "ward")
  # ac <- function(x) {
  #   cluster::agnes(dff, method = x)$ac
  # }
  # ac.table <- round(as.data.frame(purrr::map_dbl(m, ac)),3)
  # ac.tablenames <- rownames(ac.table)
  # ac.table <- cbind(ac.tablenames,ac.table)
  # rownames(ac.table) <- NULL
  # colnames(ac.table) <- c("Linkage method", "ac")
  #
  # res.o <- get_clust_tendency(dff,
  #                             n = nrow(dff)-1,
  #                             graph=FALSE)
  # H_dataset <- round(res.o$hopkins_stat,3)
  #
  #
  # random_dff <- as.data.frame(apply(dff,2,
  #                                      function(x){runif(length(x),
  #                                                        min(x),
  #                                                        max(x))}
  # ))
  #
  # res.r <- get_clust_tendency(random_dff, n =
  #                               nrow(random_dff)-1,
  #                             graph=FALSE)
  # H_random <- round(res.r$hopkins_stat,3)
  # table_H <- cbind(H_dataset,H_random)
  # colnames(table_H) <- c('dff','random dataset')
  # rownames(table_H) <- 'Hopkin statistic'
  # table_H

  wss <- function(k) {
    factoextra::hkmeans(dff, k,
            hc.metric="euclidian",
            hc.method='ward.D2',
            iter.max = 10,
            km.algorithm = "Hartigan-Wong")$tot.withinss
  }
  # Compute and plot wss for k = 1 to k = 15
  k.values <- 1:10

  # extract wss for 2-15 clusters
  wss_values <- purrr::map_dbl(k.values, wss)
  elbowmethod <- as.data.frame(t(rbind(k.values,wss_values)))

  avg_sil <- function(k) {
    km.res <- factoextra::hkmeans(dff, k,
                      hc.metric="euclidian",
                      hc.method='ward.D2',
                      iter.max = 10,
                      km.algorithm = "Hartigan-Wong")
    ss <- cluster::silhouette(km.res$cluster, stats::dist(dff, method = "euclidean"))
    mean(ss[, 3])
  }

  hkm.res <-factoextra:: hkmeans(dff, k=3,
                     hc.metric="euclidian",
                     hc.method='ward.D2',
                     iter.max = 10,
                     km.algorithm = "Hartigan-Wong")
  ss <- cluster::silhouette(hkm.res$cluster, stats::dist(dff, method = "euclidean"))
  ss <- cbind(ss[,1],ss[,2],ss[,3])
  ss <- as.data.frame(ss)
  colnames(ss) <- c('cluster','neighbor','silhouette')
  ss <- with(ss, ss[order(cluster, silhouette,decreasing = TRUE),])
  ss$cluster <- as.factor(ss$cluster)
  ss$ppnr <- 1:dim(ss)[1]
  ss <- ss[order(match(ss$cluster, c("1", "2", "3"))),]

  averagesilhplotplot <- ggplot2::ggplot(data=ss, aes(x=ppnr, y=silhouette,fill=cluster, order=cluster)) +
    ggplot2::geom_bar(stat="identity",width = 0.7,
             position=ggplot2::position_dodge())+
    ggplot2::scale_fill_manual("Cluster", values = c("1"="#2f3c4d","2"="#ad131b","3"="#cc6a0e"))+
    ggplot2::theme(
      legend.position = 'right',
      legend.spacing.x = ggplot2::unit(1, 'mm'),
      axis.title.x = ggplot2::element_blank(),
      plot.caption = element_text(color = "black")
    )+
    ggplot2::theme_minimal()+
    ggplot2::ylab("Silhouette width")+
    ggplot2::labs(subtitle = "Silhouette coefficients by cases" ) +
    ggplot2::xlab("Cases")

  minnumber <- 1
  maxnumber <- 10
  dataset <- dff

  uncertacompytable<-c()
  for (i in minnumber:maxnumber) {
    km.res <- factoextra::hkmeans(dataset, i,
                      hc.metric="euclidian",
                      hc.method='ward.D2',
                      iter.max = 10,
                      km.algorithm = "Hartigan-Wong")
    sum_i <- km.res$betweenss + km.res$tot.withinss
    number_i <- i
    ss_i <- cbind(km.res$tot.withinss, km.res$betweenss,sum_i,number_i)
    ss_i <- as.data.frame(ss_i)
    ss_i <- round(ss_i,2)
    colnames(ss_i) <- c('within','between','sum',"clusters")
    uncertacompytable <- rbind(uncertacompytable,ss_i)
  }

  uncertacompytable$within <- (uncertacompytable[,1]/uncertacompytable[,3])*100
  uncertacompytable$between <- (uncertacompytable[,2]/uncertacompytable[,3])*100
  uncertacompytablee <- uncertacompytable[,c("clusters",'within','between')]
  uncertacompytablee <- round(uncertacompytablee,2)
  data_longtable <- rstatix::gather(uncertacompytablee, type, variance, c('within','between'),
                           factor_key=TRUE)

  varianceplot <-ggplot2::ggplot(data=data_longtable, aes(x=clusters, y=variance, fill=type)) +
    ggplot2::geom_bar(stat="identity",width = 0.7,

             position=ggplot2::position_dodge(),color='black')+
    ggplot2::scale_fill_manual("Variance",
                      values = c("within" = "#2f3c4d",
                                 "between" = "#ad131b"))+
    ggplot2::theme(

      legend.spacing.x = ggplot2::unit(1, 'mm'),
      axis.title.x = ggplot2::element_blank(),
      plot.caption = element_text(color = "black")
    )+
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))+
    ggplot2::scale_x_continuous(name="K clusters", breaks=c(1:15))+
    ggplot2::theme_minimal()+
    ggplot2::theme(legend.position = 'top')+
    ggplot2::labs(subtitle = "Between- and Within variance")


  # Compute and plot wss for k = 2 to k = 15
  k.values <- 2:10

  # extract avg silhouette for 2-15 clusters
  avg_sil_values <- purrr::map_dbl(k.values, avg_sil)

  averagesilh <- as.data.frame(t(rbind(k.values,avg_sil_values)))

  averagesilhplot <- ggplot2::ggplot(data=averagesilh, aes(x=k.values, y=avg_sil_values)) +
    ggplot2::geom_line(stat="identity",width = 0.7,
              position=ggplot2::position_dodge(),color='black')+
    ggplot2::geom_point()+
    ggplot2::theme(
      legend.position = 'right',
      legend.spacing.x = ggplot2::unit(1, 'mm'),
      axis.title.x = ggplot2::element_blank(),
      plot.caption = element_text(color = "black")
    )+
    ggplot2::scale_x_continuous(name="K clusters", breaks=c(1:10))+
    ggplot2::theme_minimal()+
    ggplot2::ylab("Average Silhouettes")+
    ggplot2::labs(subtitle = "Average silhouette method")

  gapstatisticplot <- factoextra::fviz_nbclust(dff,hkmeans,  k.max = 10, linecolor = "black", method="gap_stat", nboot=50) +
    ggplot2::labs(title=NULL, subtitle = "Gap statistic", xlab='K clusters') + ggplot2::theme_minimal()

  otherind <- NbClust::NbClust(dff, distance = "euclidean",
                      min.nc = 2, max.nc = 10,
                      method = "ward.D2", index ="all")


  bestnc <- as.data.frame(t(otherind$Best.nc))
  bestnc$Number_clusters <- as.factor(bestnc$Number_clusters)
  summ.bestnc <- as.data.frame(summary(bestnc$Number_clusters))
  clusters <- rownames(summ.bestnc)
  summ.bestnc <- cbind(summ.bestnc,clusters)
  colnames(summ.bestnc) <- c('Frequency',"clusters")
  summ.bestnc$clusters <- as.numeric(summ.bestnc$clusters)
  summaryplot <- ggplot2::ggplot(data=summ.bestnc, aes(x=clusters, y=Frequency)) +
    ggplot2::geom_bar(stat="identity",width = 0.7,
             position=ggplot2::position_dodge(),fill="#2f3c4d")+
    ggplot2::theme(
      legend.position = 'right',
      legend.spacing.x = ggplot2::unit(1, 'mm'),
      axis.title.x = ggplot2::element_blank(),
      plot.caption = element_text(color = "black")
    )+
    ggplot2::theme_minimal()+
    ggplot2::ylab("Frequency of indices")+
    ggplot2::scale_y_continuous(breaks = seq(0, max(summ.bestnc$Frequency), by = 1))+
    ggplot2::scale_x_continuous(name="Clusters", breaks=c(0:10))+
    ggplot2::xlab("Clusters")+
    ggplot2::ggtitle('Summary frequency 30 indices')

  plot <- cowplot::plot_grid(varianceplot,averagesilhplot,
                             gapstatisticplot,summaryplot,nrow = 2)

  return(plot)
}
