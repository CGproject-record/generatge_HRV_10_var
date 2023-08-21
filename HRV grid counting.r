ECGgrid<-function(data,timelag1=600,timelag2=1200,nrows=40,ncols=40){

  library(latex2exp)
  library(nonlinearTseries)
  library(plot3D)
  library(raster)
  library(dplyr)
  library(lubridate) 


mydata=data
mydata=as.data.frame(mydata)
mydata_std <- mydata %>% mutate_at(c('ECG'), ~(scale(.) %>% as.vector))

#plot(ts(mydata$ECG[1:3000]), t='l' )
nrows=nrows
ncols=ncols

## Grid counting
takens_GC = buildTakens(mydata$ECG, embedding.dim=2, time.lag=timelag1)
#plot(takens_GC, t="o", col="blue", asp=1)
xmn_GC <- min(takens_GC[,2])
xmx_GC <- max(takens_GC[,2])
ymn_GC <- min(takens_GC[,1])
ymx_GC <- max(takens_GC[,1])
r_GC <- raster(extent(xmn_GC, xmx_GC, ymn_GC, ymx_GC), nrow = nrows, ncol = ncols)
xy_GC <- data.frame(x=takens_GC[,1], y=takens_GC[,2])
## classify point by cell
xy_GC$cell <- cellFromXY(r_GC, cbind(xy_GC$x, xy_GC$y))
## count point by cell
summ_GC <- xy_GC %>%
  group_by(cell) %>%
  tally()
summ_GC0=summ_GC[which(summ_GC$n!=0),]
grid_count_values=nrow(summ_GC0)/(nrows*ncols)*100
grid_count=grid_count_values

### sgridTAU
takens_sTAU = buildTakens(mydata$ECG, embedding.dim=2, time.lag=timelag2) ## lag 60 seconds
xmn_sTAU <- min(takens_sTAU[,1])
xmx_sTAU <- max(takens_sTAU[,1])
ymn_sTAU <- min(takens_sTAU[,2])
ymx_sTAU <- max(takens_sTAU[,2])
r_sTAU <- raster(extent(xmn_sTAU, xmx_sTAU, ymn_sTAU, ymx_sTAU), nrow = nrows, ncol = ncols)
xy_sTAU <- data.frame(x1=takens_sTAU[,1], y1=takens_sTAU[,2])
## classify point by cell
xy_sTAU$cell <- cellFromXY(r_sTAU, cbind(xy_sTAU$x1, xy_sTAU$y1))
## count point by cell
summ_sTAU <- xy_sTAU %>%
  group_by(cell) %>%
  tally()
summ_sTAU0=summ_sTAU[which(summ_sTAU$n!=0),]
grid_count1=nrow(summ_sTAU0)/(nrows*ncols)*100
grid_count1

sgridTAU_values=grid_count1-grid_count
sgridTAU=sgridTAU_values

results=cbind(grid_count,sgridTAU)
colnames(results)=c("Grid Conuting","SgridTAU")
return(results)
}


