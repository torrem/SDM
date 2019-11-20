# Interpolate Map
#
#'
#'@title Interpolate Map
#'
#'@description Take a standard dataset describing the spatial distribution of a species and produce an interpolated map
#'
#'@return interpolated map showing species distribution based on survey data
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

 IntMap <-function(x, years,transform=c(sqrt, log, none),xlim,ylim, res=0.03){

     d = x
d = subset(d, year>=years[1] & year<= years[2]) # take just the most recent years

if(transform=='sqrt'){
d$num = sqrt(d$num)
}

if(transform=='log'){
        d$num = log(d$num)
}


## interpolate between points using ordinary kriging to produce a smoothed surface
coordinates(d) = ~lon + lat
variogram = suppressWarnings(autofitVariogram(num ~ 1, d))
plot(variogram)


g = gstat(formula = num ~ 1, model = variogram$var_model, data = d, maxdist = 0.1)

xrange = range(d$lon)
yrange = range(d$lat)

grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = res),
                  latitude = seq(from = yrange[1], to = yrange[2], by = res))
gridded(grid) = ~longitude + latitude

p = predict(g, newdata = grid)




windows(width = 12, height = 12)


ocean.pal <- colorRampPalette(
        c("blue4",    "#005E8C",
          "#0096C8", "#45BCBB", "#8AE2AE", "#BCF8B9", "#DBFBDC"),bias=0.05)
image(ddd, col=ocean.pal(100), xlim = xlim, ylim = ylim)



col <- colorRampPalette(c("black", "blue", "purple","yellow", 'Yellow'), bias=2)
#col <- colorRampPalette(c("blue", "cyan", "yellow", "red", "darkred"), bias = 1.3)
col_pal = col(100)
col_pal_transp<-paste(col_pal, "70", sep="")
image(p, col=col_pal_transp, add=TRUE)


# cc = brewer.pal(n = 5, name = "YlGnBu")
# col <- colorRampPalette(c(cc[5], cc[4], cc[3],cc[2], cc[1]), alpha=0.2,bias=2)


plot(COAST, col='grey', add=TRUE)


# #####legend stuff ###

dseq = (max(d$num)-min(d$num))/10
dbreaks = round(seq(max(d$num), min(d$num), by=-dseq))
c1 <- colorRampPalette(rev(c("black", "blue", "purple","yellow", 'Yellow')), bias=2)
legend("bottomright", legend = dbreaks,
       bg = "white", title = 'Species density (m2)',
       cex =0.9, fill = c1(length(dbreaks)))

zbreaks = round(seq(0, 3000, by=300))
c2 <- colorRampPalette(rev(c("blue4",    "#005E8C",
    "#0096C8", "#45BCBB", "#8AE2AE", "#BCF8B9", "#DBFBDC")),bias=2)
legend(x = -71.848, y = 39.580, legend = zbreaks, fill = c2(length(zbreaks)), 
       title = 'depth (m)', bg = "white",cex = 0.9)





}















