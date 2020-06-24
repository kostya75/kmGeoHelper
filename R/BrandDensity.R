#'Simple convert value by net_coord to density
#'@import data.table
#'@importFrom stats as.formula
#'@export
#'@param df dataframe with net coord and some value, 2 columns
#'@param value_col value, most likely transactions summed by net_coord
#'@param resolution how many categories for density will be created. default=10
#'@param h comes from MASS::kde2d. kernel parameters. See MASS::kde2d for details
#'@param n comes from MASS::kde2d. soze of the grid.See MASS::kde2d for details
valueByNet2Density<-function(df,value_col,resolution=10,h=2,n=50){
  df<-data.table(df)
  df[eval(value_col)>0,value_cat:=cut(eval(as.symbol(value_col)),resolution,labels = FALSE)][eval(value_col)==0,value_cat:=0][,value_cat:=value_cat+1]
  df.value<-df[rep(seq_len(nrow(df)),df$value_cat)]
  df.value<-merge(df.value,net_coord_centers,by="net_counter")

  # compute density
  df.value.density<-kde2d_2_df(df.value$long,df.value$lat,h=h,n=n)
  names(df.value.density)<-c("long","lat","density")

  df.value.density<-coord2net2(net_coord_SP_base,"ID",df.value.density,"long","lat")
  df.value.density<-subset(df.value.density,!is.na(ID))
  setnames(df.value.density,"ID","net_counter")

  df.value.density$density_cat<-cut(df.value.density$density,resolution,labels = FALSE)

  return(df.value.density)

}

#' #valueByNet2Density(df,"customer_value")
#'
#'
#'
#' #
#' # ggmap(box_custom2)+
#' #   #stat_density2d(data=df.value,aes(x=long,y=lat,colour = ..level.., fill = ..level..), geom = "polygon",bins=9)+
#' #   geom_contour(data=df.value.density,aes(x=long,y=lat,z=density),color="red")+
#' #   geom_point(data=df.value.density,aes(x=long,y=lat,color=as.factor(density_cat)),size=3)+
#' #   scale_color_brewer(palette="Spectral", na.value="white", direction=-1)
#' #
#' #
#'
#' ###############################################
#' # Convert density matrix into a data frame
#'
#' kde2d_2_df<-function(x,y,h,n){
#'   out<-NULL
#'   temp_2d<-MASS::kde2d(x,y,h,n)
#'   for(i in seq_along(temp_2d$x)){
#'     for(j in seq_along(temp_2d$y)){
#'       temp<-cbind(x=temp_2d$x[i],y=temp_2d$y[j],z=temp_2d$z[i,j])
#'       out<-as.data.frame(rbind(out,temp))
#'
#'     }
#'   }
#'   out
#' }
#'
#' #' Add polygon ID to object with long and lat
#' #'
#' #'Overlay spatial polygon dataframe object over object with lon and lat. The function outputs coordFile with SpFile2ID added
#' #' @param SpFile2 a spatial polygon dataframe object
#' #' @param SpFile2ID a text field that identifies a varibale that defines polygon in the sp dataframe
#' #' @param coordFile an object with long and lat
#' #' @param lonName name of longitude variable in coordFile
#' #' @param latName name of latitude variable in coordFile
#' #'
#' #' @import sp
#' #' @export
#' #' @examples
#' #' \dontrun{
#' #' coord2net2(net_coord_SP_base,"ID",coordFile=someObject,lonName=long,latName=lat)
#' #' }
#' coord2net2<-function(SpFile2,SpFile2ID,coordFile,lonName,latName){
#'   temp<-coordFile
#'   SpFile<-SpFile2
#'   temp_formula<-paste0("~",paste(c(lonName,latName),collapse ="+"))
#'   coordinates(temp)<-as.formula(temp_formula)
#'   n=nrow(temp)
#'   Value=vector(length=n,mode="integer")
#'   step.inc=100
#'
#'
#'
#'   for (i in seq(1, n, step.inc)){
#'     UB<-i+step.inc-1
#'     UB<-ifelse(UB<n,UB,n)
#'
#'     IndivCoord<-temp[i:UB,]
#'     proj4string(IndivCoord)<-proj4string(SpFile)
#'     testMatch<-over(IndivCoord, SpFile)
#'     Value[i:UB]<-as.character(testMatch[,SpFile2ID])
#'
#'
#'   }
#'
#'   return(data.table(temp@data,temp@coords,ID=as.integer(Value),File=deparse(substitute(coordFile))))
#' }
