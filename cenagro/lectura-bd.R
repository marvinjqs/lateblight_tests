### IV CENSO NACIONAL AGROPECUARIO 2012 - LECTURA DE DATOS ###
##############################################################

ClustClassf<-function(Dataset1,NClusters=2,NameOut=NamesAllVariables[i])
{
  
  sink(paste(NameOut,"-ResumenData.txt",sep=""))
  print(summary(Dataset1))
  sink()
  
  hc <- hclust(dist(na.omit(Dataset1)), "ward") # aglomeration method "ave"
  png(paste(NameOut,"-ClusterDendogram.png",sep=""), width = 18, height = 15, units = 'in', res = 300)
  plot(hc, hang = -1)
  dev.off()
  ############
  
  estand=function(vec)
  {
    vect= (vec-mean(vec, na.rm = TRUE))/sd(vec, na.rm = TRUE)
    return(vect)
  }
  datt= apply(Dataset1,2,estand)
  datt=data.frame(datt)
  
  mod=prcomp(~., data = datt,scale=TRUE)
  #mod
  #print(mod$x)
  
  sink(paste(NameOut,"-ResumenCompPrinc.txt",sep=""))
  print(summary(mod))
  sink()
  
  
  png(paste(NameOut,"-BiplotCompPrinc.png",sep=""), width = 18, height = 15, units = 'in', res = 300)
  biplot(mod,cex=1)
  abline(h = 0, v = 0, lty = 2.5, col = "green", lwd = 1.5)
  dev.off()
  
  ###############
  library(agricolae)
  
  dat2=data.frame(na.omit(Dataset1),Cluster=cutree(hc, k=NClusters))
  #dat2=data.frame(Cluster=cutree(hc, k=2:6))  ## todos los tamaños de cluster
  
  png(paste(NameOut,"-ScaterPlotClust.png",sep=""), width = 20, height = 20, units = 'in', res = 300)
  plot(dat2)
  dev.off()
  
  #dat2[order(dat2[,(ncol(Dataset1)+1)]),]
  validNclusts<-table(dat2[,ncol(dat2)])<2
  
  if(length((1:NClusters)[!validNclusts])==NClusters)
  {
    A <- data.frame(dat2[,(ncol(Dataset1)+1)])
    vars <- data.frame(dat2[,-(ncol(Dataset1)+1)])
    A <- as.factor(A[,1])
    
    n1=ncol(vars)
    nombres=colnames(vars)
    
    pvals=c(1)
    for(k in 1:ncol(vars))
    {
      #k<-5
      model <- aov(vars[,k]~ A)
      anva=anova(model)  
      pvals[k]=anva[1,5]
    }
    
    mms=t(aggregate(dat2[,-(ncol(Dataset1)+1)],list(Cluster=dat2[,(ncol(Dataset1)+1)]),mean,na.rm = TRUE))
    mms1=mms[-1,];colnames(mms1)=paste("Cluster",mms[1,])
    result=data.frame(mms1,p_value=round(pvals,6))
    sink(paste(NameOut,"-AnvaClust.txt",sep=""))
    print(result)
    sink()
    
    #########
    require(partykit)
    
    #mod <- ctree(factor(Cluster) ~ .,data = dat2,controls = ctree_control(mincriterion = 0.95, minsplit = 1, minbucket = 2))
    mod <- ctree(factor(Cluster) ~ .,data = dat2)
    png(paste(NameOut,"-Tree1.png",sep=""), width = 18, height = 15, units = 'in', res = 300)
    plot(mod)
    dev.off()
    
    sink(paste(NameOut,"-ClassifactionClust.txt",sep=""))
    print(mod)
    tabla=table(predict(mod),dat2$Cluster);(tabla)
    
    cat("\n","Matriz de confucion","\n")
    print(tabla)
    cat("\n","\n","Porcentaje de buena prediccion","\n")
    print(sum(diag(tabla))/sum(tabla))
    sink()
    return(list(result,dat2))
    
  }else{
    print("Hay menos de 2 observaciones para alguno de los clusters")
    result<-NA
    return(list(result,dat2))
  }
}

###########################################################################################
# Run application

library(cluster)
library(maptools)
library(raster)
library(rasterVis)
library(colorspace)
library(RColorBrewer)
library(rgeos)

setwd("C:/Users/Asus/Documents/R/lateblight_tests/cenagro")
ff <- list.files("C:/Users/Asus/Documents/R/lateblight_tests/cenagro/combined/", 
                 pattern=".rds", full=TRUE)

d <- readRDS(ff[1])


# Dataset1<-d[d$WCUENCA=="Cuenca Utcubamba",c(25:48)]
# ListVarImport<-ClustClassf(Dataset1,NClusters=3,NameOut="Seguridad Alimentaria")
# 
# 
#  shape<-getData("GADM", country="PER", level=1)
#  xy <- data.frame(as.matrix(unique(d[, c("LONG_DECI", "LAT_DECI", "WALTITUD")])))
#  r = raster(shape, res=1/12)
#  cap<-raster::rasterize(xy[,1:2], r, field=xy[,3], fun=mean)
# 
#  rng <- c(minValue(cap), maxValue(cap))
#  print(rng)
#  brks <- c(0,999,1999,2999,5200) # definiendo los niveles
#  cols <- c("gray70",brewer.pal(9, "Greens")[c(6)],"darkorange2",brewer.pal(8, "YlOrRd")[7])
#  png(paste("Map-Altitude",".png",sep=""), width = 10, height = 8, units = 'in', res = 300)
#  plot(cap, breaks=brks, col=cols,zlim=c(0,rng[2]),axis.args=list(cex.axis=1.2),legend=FALSE)
#  plot(shape, lwd=0.8, col="transparent",add=TRUE)
#  LABs=c("0 - 999", "1000 - 1999","2000 - 2999","3000 - 5200") ## ERI
#  legend(-87,-15.4, legend=LABs, fill=cols, cex=0.8, bty="o",bg = "white",title="Altitud")
#  dev.off()


# 2da Base de datos otras variables
dd <- readRDS(ff[3])
# Codigos de Cultivos
cc <- read.csv("code.csv",sep = ";", header = T)

#x = merge(d[,c("P001","P002","P003","P007X","P008","WALTITUD","LONG_DECI","LAT_DECI")], dd, by=c("P001","P002","P003","P007X","P008"))
x = merge(d[,c(2:6,25:49,53:54)], dd[,c("P001","P002","P003","P007X","P008","P024_03")], by=c("P001","P002","P003","P007X","P008"))
# liberando memoria
rm(d);rm(dd)
# eliminando codigos de cultivos vacios
x <- x[!is.na(x$P024_03), ] 
# agregando el codigo de cultivo (nombre verdadero)
x <- merge(x, cc, by.x="P024_03", by.y="code")


# Configurando zona y tipo de cultivo para la obtencion de datos

Dataset1<-x[x$P024_03 == "2612" & as.numeric(x$WALTITUD) > 2000 & as.numeric(x$WALTITUD) < 2500, c(31:34)]

write.csv(Dataset1, "data-blanca-intand.csv", row.names = F)



dat<-ClustClassf(Dataset1,NClusters=2,NameOut="Papaya")
dat2 <- dat[[2]]

shape<-getData("GADM", country="PER", level=1)
xy <- data.frame(as.matrix(unique(Dataset1[, c("LONG_DECI", "LAT_DECI", "WSUP03")])))
r = raster(shape, res=1/12)
projection(r) <- "+proj=longlat +datum=WGS84 +no_defs"
cap<-raster::rasterize(xy[,1:2], r, field=xy[,3], fun=mean)

ggplot() +
    geom_polygon(data = shape, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, colour = "black") + 
  
    geom_point(data = xy,  # Add and plot species data
               aes(x = LONG_DECI, y = LAT_DECI)) +
    coord_quickmap() +  # Prevents stretching when resizing
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") + 
    guides(colour=guide_legend(title="Potato"))




rng <- c(minValue(cap), maxValue(cap))
print(rng)
brks <- c(0.5,1.5,2) # definiendo los niveles
cols <- c(brewer.pal(9, "Greens")[c(6)],"darkorange2")
png(paste("Map-Clust_Papaya",".png",sep=""), width = 10, height = 8, units = 'in', res = 300)
plot(cap, breaks=brks, col=cols,zlim=c(0,rng[2]),axis.args=list(cex.axis=1.2),legend=FALSE)
plot(shape, lwd=0.8, col="transparent",add=TRUE)
LABs=c("1","2") ## ERI
legend(-87,-13.4, legend=LABs, fill=cols, cex=0.8, bty="o",bg = "white",title="Cluster - Papaya")
dev.off()

