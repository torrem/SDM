# Bubble Map
#
#'
#'@title Bubble Map
#'
#'@description Take a standard dataset describing the spatial distribution of a species and produce a Bubble map
#'
#'@return Bubble map showing species distribution based on survey data
#'
#'@export
#'
#'


require(RColorBrewer)
require(maptools)
require(repmis)
require(automap)
require(gstat)
require(raster)



source_data("https://raw.githubusercontent.com/torrem/SDM/master/Data/USCoast.RData?raw=true")
source_data("https://raw.githubusercontent.com/torrem/SDM/master/Data/USDepth.RData?raw=true")



# x=scallop
# years = c(2010,2014)
# xlim = c(-75, -71);ylim = c(39, 41.5); add=FALSE

BubbleMap <-function(x, years,transform=c(sqrt, log, none),xlim,ylim, res=0.03, add=FALSE){

        d = x
        d = subset(d, year>=years[1] & year<= years[2]) # take just the most recent years
        
       
        
        
       # coordinates(d) = ~lon + lat
        if (add==FALSE){
        
        windows(width = 12, height = 12)
        
        ocean.pal <- colorRampPalette(
                c("blue4",    "#005E8C",
                  "#0096C8", "#45BCBB", "#8AE2AE", "#BCF8B9", "#DBFBDC"),bias=0.05)
        image(ddd, col=ocean.pal(100), xlim = xlim, ylim = ylim)
        
        plot(COAST, col='grey', add=TRUE)
        
        }
        

        
        # cc = brewer.pal(n = 5, name = "PuBu")
        # col <- colorRampPalette(c(cc[5], cc[4], cc[3],cc[2], cc[1]), alpha=0.2,bias=1)
        
        col <- colorRampPalette(c("blue", "cyan", "yellow", "red", "darkred"), bias = 1)
       # col <- colorRampPalette(c("darkblue", "blue", "yellow", "red", "darkred"), bias = 1)
        
        d$colcode = ((sqrt(d$num)/15)/max(sqrt(d$num)/15)*100)
        d$colcode = ifelse(d$colcode==0,1,d$colcode)
        d$col = col(100)[d$colcode]
        
        
        points(d$lat ~ d$lon, cex = sqrt(d$num)/15, pch=19, 
               col=d$col)
        
 
        
        

        dseq = (max(sqrt(d$num)/15)-min(sqrt(d$num)/15))/6
        dbreaks = round(seq(max(sqrt(d$num)/15), min(sqrt(d$num)/15), by=(-dseq/2)),1)
        
        c1 <- colorRampPalette(c("darkred", "red", "yellow", "cyan", "blue"), bias = 1)
        
        legend("bottomright", legend = dbreaks,
               bg = "white", title = '   Species catch (sqrt/15 transformed)',
               cex =0.9, fill = c1(length(dbreaks)))



}