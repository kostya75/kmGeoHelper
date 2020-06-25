#'net_coord_SP_base: Spatial polygons dataframe.
#'the data is build around agricultural regions of Canada/Western.
#'Each polygon in the datasource is approx 11 by 11 km square.
#'
#'@source generated data
#'@format 'sp' object
"net_coord_SP_base"
load("data/net_coord_SP_base.rda")

#'net_coord_centers: geographical centres of polygon squares.
#'somewhat redundant data as centers can be recovered from a slot on net_coord_SP_base.
#'The data covers areas outside agri regions as well.
#'@source generated data
#'@format dataframe
"net_coord_centers"
load("data/net_coord_centers.rda")


#'Net_Postal_base: connect net_coord to Postal Code. The data is for agricultural regions of Canada/Western.
#'
#'@source generated data
#'@format dataframe
"Net_Postal_base"
load("data/Net_Postal_base.rda")
