#'Convert value by net_coord to density
#'
#'Function outputs netcoord with density and density ranges that can be used to produce consisted densities year over year
#'@importFrom stats as.formula
#'@export
#'@param df dataframe with net coord and some value, 2 columns
#'@param value_col value, most likely transactions summed by net_coord
#'@param resolution how many categories for density will be created. default=10
#'@param df_ranges character vector that is simialar to the output of *cut* function. Each element represents a range closed on the right. NULL for current season, adjustment for PY
#'@param dens_ranges character vector that is simialar to the output of *cut* function. Each element represents a range closed on the right. NULL for current season, adjustment for PY
#'@param h comes from MASS::kde2d. kernel parameters. See MASS::kde2d for details
#'@param n comes from MASS::kde2d. size of the grid.See MASS::kde2d for details
#'@param tracker an optional string that can be supplied to be added to df_ranges and dens_ranges
#'@param PY an adjustment factor. use 1 for CY, adjust to decline CY/PY for PY
valueByNet2Density2<-function(df,value_col,resolution=10,df_ranges=NULL,dens_ranges=NULL,h=2,n=50,tracker="Some Brand",PY=1){
  resolution<-resolution-1
  if(is.null(df_ranges)){
    df_ranges<-attr(cut(df[value_col][df[value_col]>0],resolution),
                    "level")
  }

  df<-cutRange(df,value_col,df_ranges,T)
  df$value_cat[is.na(df$value_cat)]<-resolution
  df.value<-df[rep(seq_len(nrow(df)),df$value_cat),]
  df.value<-merge(df.value,net_coord_centers,by="net_counter")

  # compute density
  df.value.density<-kde2d_2_df(df.value$long,df.value$lat,h=h,n=n)
  names(df.value.density)<-c("long","lat","density")

  df.value.density<-coord2net2(net_coord_SP_base,"ID",df.value.density,"long","lat")
  df.value.density<-subset(df.value.density,!is.na(ID))

  names(df.value.density)[names(df.value.density)=="ID"]<-"net_counter"

  if(is.null(dens_ranges)){
    dens_ranges<-attr(cut(df.value.density$density,resolution+1),
                      "level")
  }

  df.value.density$density<-df.value.density$density/PY
  df.value.density<-cutRange(df.value.density,"density",dens_ranges,F)
  df.value.density$value_cat[is.na(df.value.density$value_cat)]<-resolution+1
  cache<-list(df_ranges,dens_ranges)
  names(cache)<-c(sprintf("df%s",tracker),sprintf("dens%s",tracker))

  return(list(df.value.density=df.value.density,cache=cache))

}
###############################################
# Convert density matrix into a data frame

kde2d_2_df<-function(x,y,h,n){
  out<-NULL
  temp_2d<-MASS::kde2d(x,y,h,n)
  for(i in seq_along(temp_2d$x)){
    for(j in seq_along(temp_2d$y)){
      temp<-cbind(x=temp_2d$x[i],y=temp_2d$y[j],z=temp_2d$z[i,j])
      out<-as.data.frame(rbind(out,temp))

    }
  }
  out
}

#' Add polygon ID to object with long and lat
#'
#'Overlay spatial polygon dataframe object over object with lon and lat. The function outputs coordFile with SpFile2ID added
#' @param SpFile2 a spatial polygon dataframe object
#' @param SpFile2ID a text field that identifies a varibale that defines polygon in the sp dataframe
#' @param coordFile an object with long and lat
#' @param lonName name of longitude variable in coordFile
#' @param latName name of latitude variable in coordFile
#'
#' @import sp
#' @export
#' @examples
#' \dontrun{
#' coord2net2(net_coord_SP_base,"ID",coordFile=someObject,lonName=long,latName=lat)
#' }
coord2net2<-function(SpFile2,SpFile2ID,coordFile,lonName,latName){
  temp<-coordFile
  SpFile<-SpFile2
  temp_formula<-paste0("~",paste(c(lonName,latName),collapse ="+"))
  coordinates(temp)<-as.formula(temp_formula)
  n=nrow(temp)
  Value=vector(length=n,mode="integer")
  step.inc=100



  for (i in seq(1, n, step.inc)){
    UB<-i+step.inc-1
    UB<-ifelse(UB<n,UB,n)

    IndivCoord<-temp[i:UB,]
    proj4string(IndivCoord)<-proj4string(SpFile)
    testMatch<-over(IndivCoord, SpFile)
    Value[i:UB]<-as.character(testMatch[,SpFile2ID])


  }

  return(data.frame(temp@data,temp@coords,ID=as.integer(Value),File=deparse(substitute(coordFile))))
}

#' Convert Numeric to Factor based on supplied ranges
#'
#'cutRanges divides the range of *value_col* and codes the values according to which interval they fall in based on the provided
#'a new column value_cat to the returned dataframe
#' @param df a data frame
#' @param value_col a name of numeric column to be converted
#' @param cut_range character vector that is simialar to the output of *cut* function. Each element represents a range closed on the right
#' @param adapt logical, if TRUE add o zero range to put zero values into it's own range
#'
#' @export
#' @examples
#' \dontrun{
#' df2<-cutRange(df,"customer_value",df_ranges,F)
#' }
cutRange<-function(df,value_col,cut_range,adapt=T){
  #determine max to override supplied range
  max_temp<-max(df[value_col])

  df_ranges_num<-sapply(cut_range, function(x){
    as.numeric(strsplit(gsub("\\(|\\]|\\)|\\[","",x),",")[[1]])
  })
  df_ranges_num<-t(unname(df_ranges_num))

  # if adapt = T, add Zero row, else use ranges as is but still adjusting for max
  if(adapt){
    df_ranges_num[1,1]<-1
    df_ranges_num<-rbind(c(-0.1,1),df_ranges_num)
  }

  # how many groups in the supplied range+1 for zero
  L<-dim(df_ranges_num)[1]
  if(df_ranges_num[L,2]>max_temp){
    df_ranges_num[L,2]<-max_temp
  }

  for(i in 1:L){
    df$value_cat[df[value_col]>df_ranges_num[i,1] & df[value_col]<=df_ranges_num[i,2]]<-i
  }
  return(df)
}
