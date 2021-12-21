#######################################################################################
#                                                                                     #
#           An R Routine for Item Analysis based on the itemanalysis package          #
#            https://cran.r-project.org/web/packages/itemanalysis/index.html          #
#                                                                                     #
#######################################################################################


itemanalysis_dich <- function (data, 
                               key, 
                               options, 
                               ngroup = ncol(data), 
                               correction=TRUE) {
  
  require("ggplot2")
  require("polycor")
  
  #########################################################################################
  # data, a data frame with N rows and n columns, where N denotes the number of subjects 
  # and n denotes the number of items. All items should be scored using nominal 
  # response categories. All variables (columns) must be "character". 
  # Missing values ("NA") are allowed and scored as incorrect in item analysis
  # key, a character vector of length n, where n denotes the number of items. 
  # options, number of possible nominal options for items (e.g., "A","B","C","D")
  # make sure each item is consistent, and includes the same response options
  # ngroup, number of score groups
  # correction, TRUE or FALSE, if TRUE item and distractor discrimination is corrected for
  # spuriousnes by removing the item score from the total score
  #########################################################################################
  
  for (i in 1:ncol(data)) {
    if (is.character(data[, i]) != TRUE) {
      data[,i]=as.character(data[,i])
    }
  }
  
  scored.data <- as.data.frame(matrix(nrow = nrow(data), ncol = ncol(data)))
  
  for (i in 1:ncol(scored.data)) {
    scored.data[,i] <- ifelse(data[,i] == key[i],1,0)
    if(length(which(is.na(scored.data[,i])))!=0) {
      scored.data[which(is.na(scored.data[,i])==TRUE),i]=0
    }
  }
  
  total.score <- rowSums(scored.data)
  ybar <- mean(total.score)
  sdt <- sd(total.score)
  p <- colMeans(scored.data)
  
  pbis <- c()
  pbis.corrected <- c()
  bis  <- c()
  bis.corrected <- c()
  
  for(k in 1:ncol(data)) {  
    pbis[k]=cor(scored.data[,k],total.score,use="pairwise.complete.obs")
    pbis.corrected[k]=cor(scored.data[,k],
                          rowMeans(scored.data[,-k],na.rm=TRUE)*(ncol(scored.data)-1),
                          use="pairwise.complete.obs")
    bis[k]=polyserial(total.score,scored.data[,k])
    bis.corrected[k]=polyserial(rowMeans(scored.data[,-k],na.rm=TRUE)*(ncol(scored.data)-1),scored.data[,k])
  }
  
  
  item.stat <- matrix(nrow=ncol(data),ncol=4)
  colnames(item.stat) <- c("Item Difficulty","Item Threshold","Point-Biserial","Biserial")
  
  rnames <- ("Item 1")
  for(i in 2:ncol(data)){ rnames <- c(rnames,paste("Item ",i,sep=""))}
  rownames(item.stat) <- rnames	
  item.stat[,1]=p
  item.stat[,2]=qnorm(1-p)
  if(correction==TRUE){ item.stat[,3]=pbis.corrected } else { item.stat[,3]=pbis }
  if(correction==TRUE){ item.stat[,4]=bis.corrected} else {item.stat[,4]=bis}
  
  
  sgroups <- cut(total.score,breaks=ngroup)
  slevels <- levels(sgroups)
  
  sgnum <- rowMeans(cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", slevels) ),
                          upper = as.numeric( sub("[^,]*,([^]]*)\\]","\\1",slevels))))
  
  
  SG <- vector("list",ngroup)
  
  for(j in 1:ngroup){
    SG[[j]]=which(sgroups==slevels[j])
  }
  
  prop <- vector("list",ncol(data))
  names(prop) <- rnames
  
  for(i in 1:ncol(data)) {
    
    dist <- matrix(nrow=length(options),ncol=ngroup)
    colnames(dist) <- slevels
    rownames(dist) <- options 
    
    for(g in 1:ngroup){
      for(o in 1:length(options)){
        dist[o,g]=length(which(data[SG[[g]],i]==options[o]))/length(SG[[g]])
      }
    }
    
    prop[[i]]=dist
    
  }
  
  dist.sel <- matrix(nrow=ncol(data),ncol=length(options))  
  dist.disc <- matrix(nrow=ncol(data),ncol=length(options))
  dist.disc2 <- matrix(nrow=ncol(data),ncol=length(options))
  colnames(dist.disc) <- options
  rownames(dist.disc) <- rnames
  colnames(dist.disc2) <- options
  rownames(dist.disc2) <- rnames
  colnames(dist.sel) <- options
  rownames(dist.sel) <- rnames
  
  for(i in 1:ncol(data)){
    for(o in 1:length(options)) {
      temp <- ifelse(data[,i]==options[o],1,0)
      temp[is.na(temp)]=0
      dist.sel[i,o]=mean(temp,na.rm=TRUE)
      if(correction==FALSE){
        dist.disc[i,o]=cor(temp,total.score,use="pairwise.complete.obs")
        dist.disc2[i,o]=polyserial(total.score,temp)
      } else {
        dist.disc[i,o]=cor(temp,rowMeans(scored.data[,-i],na.rm=TRUE)*(ncol(scored.data)-1),use="pairwise.complete.obs")
        dist.disc2[i,o]=polyserial(rowMeans(scored.data[,-i],na.rm=TRUE)*(ncol(scored.data)-1),temp)
      }
    }
  }
  
  plots <- vector("list",ncol(data))
  
  for(i in 1:ncol(data)) {
    
    options.d <- c()
    for(u in 1:length(options)){ 
      options.d[u] <- paste(options[u]," (",round(dist.disc2[i,u],2),") ",sep="")
    }
    
    d <- as.data.frame(cbind(sg=sgnum,p=prop[[i]][1,]))
    for(u in 2:length(options)){ d <- rbind(d,cbind(sg=sgnum,p=prop[[i]][u,]))}
    optt <- c()
    for(u in 1:length(options)){ optt <- c(optt,rep(options.d[u],ngroup))}
    d$opt <- optt
    
    
    pp <- ggplot(data=d,aes_string(x="sg",y="p",group="opt",shape="opt"))+
      geom_line()+
      geom_point(size=3)+
      ggtitle(paste("Item ",i,sep=""))+
      theme(panel.background = element_blank(),
            #legend.title=element_blank(),
            legend.key = element_blank(),
            axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
            legend.justification=c(0,1),legend.position=c(0,1),
            legend.text=element_text(size=12,face="bold"))+
      scale_shape_discrete(name = "Response Categories")+
      scale_x_continuous(limits = c(0,ncol(data)),breaks=seq(0,ncol(data),ceiling(ncol(data)/10)))+
      scale_y_continuous(limits = c(0,1))+xlab("Score Groups")+ylab("Proportion of Response Categories")
    
    plots[[i]] <- pp
  }
  
  results <- list(
    item.stat=item.stat,
    dist.sel=dist.sel,
    dist.disc=dist.disc,
    dist.disc2=dist.disc2,
    plots=plots)
  
  class(results) <- "dich"
  
  return(results)
}


print.dich <- function(x) {
  
  cat("*************************************************************************","\n")
  cat("","\n")
  cat("Processing Date: ",date(),"\n")
  cat("","\n")
  cat(sprintf("%50s","ITEM STATISTICS"),"\n")
  cat("","\n")
  print(round(x$item.stat,3))
  cat("","\n")
  cat("","\n")
  cat("","\n")
  
  cat(sprintf("%50s","DISTRACTOR SELECTION PROPORTIONS"),"\n")
  cat("","\n")
  print(round(x$dist.sel,3))
  cat("","\n")
  cat("","\n")
  cat("","\n")
  
  cat(sprintf("%50s","DISTRACTOR Point-Biserial"),"\n")
  cat("","\n")
  print(round(x$dist.disc,3))
  cat("","\n")
  cat("","\n")
  cat("","\n")
  
  cat(sprintf("%50s","DISTRACTOR Biserial"),"\n")
  cat("","\n")
  print(round(x$dist.disc2,3))	
  cat("","\n")
  cat("","\n")
  cat("","\n")
  invisible(x)
}


##########################################################################################

itemanalysis_poly <- function(data, 
                              options, 
                              ngroup = ncol(data), 
                              correction=TRUE,
                              rename.items = FALSE) 
{
  
  require("ggplot2")
  require("polycor")
  
  #########################################################################################
  # data, a data frame with N rows and n columns, where N denotes the number of subjects 
  # and n denotes the number of items. All items should be scored using nominal/ordinal 
  # response categories. All variables (columns) must be "numeric". 
  # options, numbers representing the response categories (e.g.,0,1,2,3)
  # make sure each item is consistent, and includes the same response options
  # Recommend that the numerical codes are recoded such that the minimum score is 0
  # ngroup, number of score groups
  # correction, TRUE or FALSE, if TRUE item and distractor discrimination is corrected for
  # spuriousnes by removing the item score from the total score
  #########################################################################################
  
  total.score <- rowMeans(data, na.rm=TRUE)*ncol(data)
  
  pbis <- c()
  pbis.corrected <- c()
  bis  <- c()
  bis.corrected <- c()
  
  for(k in 1:ncol(data)) { 
    pbis[k]=cor(data[,k],total.score,use="pairwise.complete.obs")
    pbis.corrected[k]=cor(data[,k],
                          rowMeans(data[,-k],na.rm=TRUE)*(ncol(data)-1),
                          use="pairwise.complete.obs")
    bis[k]=polyserial(total.score,data[,k])
    bis.corrected[k]=polyserial(rowMeans(data[,-k],na.rm=TRUE)*(ncol(data)-1),data[,k])
  }
  
  
  item.stat <- matrix(nrow=ncol(data),ncol=4)
  colnames(item.stat) <- c("Mean Score","Item Difficulty","Point-Biserial","Polyserial")
  
  if(rename.items) {
    rnames <- ("Item 1")
    for(i in 2:ncol(data)){rnames <- c(rnames,paste("Item ", i, sep=""))}
  } else {
    rnames <- colnames(data)
  }
  
  rownames(item.stat) <- rnames	
  item.stat[,1]=colMeans(data,na.rm=TRUE)
  item.stat[,2]=colMeans(data,na.rm=TRUE)/max(options)
  if(correction==TRUE){ item.stat[,3]=pbis.corrected } else { item.stat[,3]=pbis }
  if(correction==TRUE){ item.stat[,4]=bis.corrected } else { item.stat[,4]=bis }
  
  sgroups <- cut(total.score,breaks=ngroup)
  slevels <- levels(sgroups)
  
  sgnum <- rowMeans(cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", slevels) ),
                          upper = as.numeric( sub("[^,]*,([^]]*)\\]","\\1",slevels))))
  
  SG <- vector("list", ngroup)
  
  for(j in 1:ngroup){
    SG[[j]]=which(sgroups==slevels[j])
  }
  
  prop <- vector("list",ncol(data))
  names(prop) <- rnames
  
  for(i in 1:ncol(data)) {
    
    dist <- matrix(nrow=length(options),ncol=ngroup)
    colnames(dist) <- slevels
    rownames(dist) <- options 
    
    for(g in 1:ngroup){
      for(o in 1:length(options)){
        dist[o,g]=length(which(data[SG[[g]],i]==options[o]))/length(SG[[g]])
      }
    }
    
    prop[[i]]=dist
    
  }
  
  dist.sel <- matrix(nrow=ncol(data),ncol=length(options))  
  dist.disc <- matrix(nrow=ncol(data),ncol=length(options))
  dist.disc2 <- matrix(nrow=ncol(data),ncol=length(options))
  colnames(dist.disc) <- options
  rownames(dist.disc) <- rnames
  colnames(dist.disc2) <- options
  rownames(dist.disc2) <- rnames
  colnames(dist.sel) <- options
  rownames(dist.sel) <- rnames
  
  for(i in 1:ncol(data)){
    for(o in 1:length(options)) {      
      temp <- ifelse(data[,i]==options[o],1,0)
      temp[is.na(temp)]=0
      dist.sel[i,o]=mean(temp,na.rm=TRUE)
      if(correction==FALSE){
        dist.disc[i,o]=cor(temp,total.score,use="pairwise.complete.obs")
        dist.disc2[i,o]=polyserial(total.score,temp)
      } else {
        dist.disc[i,o]=cor(temp,rowMeans(data[,-i],na.rm=TRUE)*(ncol(data)-1),use="pairwise.complete.obs")
        dist.disc2[i,o]=polyserial(rowMeans(data[,-i],na.rm=TRUE)*(ncol(data)-1),temp)
      }
    }
  }
  
  
  plots <- vector("list", ncol(data))
  
  for(i in 1:ncol(data)) {
    
    options.d <- c()
    for(u in 1:length(options)){ 
      if(correction==TRUE){
        options.d[u] <- paste(options[u]," (",round(dist.disc2[i,u],2),") ", sep="")
      } else {options.d[u] <- paste(options[u]," (",round(dist.disc[i,u],2),") ", sep="") }
    }
    
    d <- as.data.frame(cbind(sg=sgnum, p=prop[[i]][1,]))
    for(u in 2:length(options)){ d <- rbind(d,cbind(sg=sgnum,p=prop[[i]][u,]))}
    
    optt <- c()
    for(u in 1:length(options)){ optt <- c(optt,rep(options.d[u], ngroup))}
    d$opt <- optt
    
    # Specify shape options: ggplot2 doesn't like more than 6 categories
    shape_options <- c(16, 17, 15, 18, 3, 7, 8, 9, 10, 7, 12, 13)
    
    pp <- ggplot(data=d, aes_string(x="sg", y="p", group="opt"))+
      geom_line() +
      geom_point(aes_string(shape="opt"), size=3)+
      scale_shape_manual(values = shape_options[1:length(options)],
                         name  = "Response Categories") + 
      #ggtitle(paste("Item ", i, sep=""))+
      ggtitle(rnames[i]) +
      theme(panel.background = element_blank(),
            legend.key = element_blank(),
            axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
      scale_x_continuous(limits = c(0,ncol(data)*max(options)),breaks=seq(0,ncol(data)*max(options),ceiling(ncol(data)/10)))+
      scale_y_continuous(limits = c(0,1))+
      xlab("Score Groups")+
      ylab("Proportion of Response Categories")
    
    
    plots[[i]] <- pp
  }
  
  results <- list(
    item.stat=item.stat,
    dist.sel=dist.sel,
    dist.disc=dist.disc,
    dist.disc2=dist.disc2,
    plots=plots)
  
  class(results) <- "poly"
  
  return(results)
}


print.poly <- function(x) {
  
  cat("*************************************************************************","\n")
  cat("","\n")
  cat("Processing Date: ", date(),"\n")
  cat("","\n")
  cat(sprintf("%50s","ITEM STATISTICS"),"\n")
  cat("","\n")
  print(round(x$item.stat,3))
  cat("","\n")
  cat("* Item difficulty is the ratio of mean score to possible maximum","\n")
  cat(" score and assumes the minimum score is 0.","\n")
  cat("","\n")
  cat("","\n")
  
  cat(sprintf("%50s","RESPONSE CATEGORY SELECTION PROPORTIONS"),"\n")
  cat("","\n")
  print(round(x$dist.sel,3))
  cat("","\n")
  cat("","\n")
  cat("","\n")
  
  cat(sprintf("%50s","RESPONSE CATEGORY POINT-BISERIAL CORRELATION"),"\n")
  cat("","\n")
  print(round(x$dist.disc,3))
  cat("","\n")
  cat("","\n")
  
  cat(sprintf("%50s","RESPONSE CATEGORY BISERIAL CORRELATION"),"\n")
  cat("","\n")
  print(round(x$dist.disc2,3))
  cat("","\n")
  cat("","\n")
  cat("","\n")
  invisible(x)

}

















