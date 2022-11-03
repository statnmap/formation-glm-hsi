# Libraries
require(sp)
require(rgdal)
require(maps)
require(mapdata)
require(raster)
require(geosphere) # for scale estimation if longlat projection
require(rgeos)
  
#' A function to divide continous values into nice bins
#'
#' @param x Vector to be divided. May work with a Raster object directly on one layer.
#' @param layer Number of the layer on which to calculate bins if x is a raster.
#' @param bins Number of bins into which divided the continuous scale
#' @param brk User defined breaks. Only usefull if you want to simplify your breaks with the "precision" procedure.
#' In this case, only brk and prec are usefull.
#' @param quantiles Logical. Whether to divide through quantiles (TRUE) or regularly spaced bins (FALSE). Default to FALSE. See Details.
#' @param prec precision relative to difference between min and max. Decrease for higher precision. Default to 2.
#'
#' @details prec allow for nice cut values. Values are rounded using : 10 ^ (nchar(max - min) - prec).
#'  A value of prec = 2 transforms 2235 into 2200 and 0.02235 into 0.022.
#'  Set to zero if you want to keep original cuts. In case of quantile = TRUE, prec = 0 keeps real quantiles.
#'  
#'  The function sets breaks to be unique. Don't be surprise if the final number of bins is not what you asked for.
#' @export

brk.bins <- function(x = NULL, layer = 1, bins = 10, brk = NULL, quantiles = FALSE, prec = 2) {
  if (is.null(brk)) {
    if (is.null(x)) {stop("x should be specified if you want to find bins on x !")}
    if (is(x)[2] == "Raster") {if (nlayers(x) > 1) {x <- raster(x, layer)}}
    if (!quantiles) {
      if (is(x)[2] == "Raster") { # Special raster case
        # Use already calculated raster Min/Max avoids putting values(x) in RAM
        if (is.na(minValue(x)) | is.na(maxValue(x))) {x <- setMinMax(x)}
          minx <- minValue(x)
          maxx <- maxValue(x)
      } else {
        minx <- min(x, na.rm = TRUE)
        maxx <- max(x, na.rm = TRUE)
      }
      brk <- seq(minx, maxx, length.out = (bins + 1))
    } else {
      # quantiles works directly with raster objects
      brk <- quantile(x, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
    }
  }
  # Simplify brk with low number of digits for nice breaks
  diff.brk <- max(brk) - min(brk)
  if (diff.brk > 1) {
    # precision defined by length of the number
    n <- nchar(round(diff.brk))
  } else {
    # precision defined by number of zero after comma
    n.w0 <- (nchar(diff.brk) - 2) # nchar with zero after comma
    n.wo0 <- nchar(diff.brk * 10 ^ n.w0) # nchar without zero 
    n <- -n.w0 + n.wo0 # diff of nchar
  }
  plus <- minus <- 10 ^ (n - prec)
  if (min(brk) >= 0 & (min(brk) - minus) <= 0) {minus <- 0} 
  brk <- round(brk / 10 ^ (n - prec)) * 10 ^ (n - prec)
  brk[1] <- brk[1] - minus
  brk[length(brk)] <- brk[length(brk)] + plus
  unique(brk)
}

#' A function to get a square (sp-)Polygon delimiting the contours of a Spatial object (Raster* or Spatial*)
#' 
#' @param x Spatial dataset to get extent from (RasterLayer or SpatialPolygonsDataFrame) or an Extent.
#' @param crs An object of class CRS
#'
#' @output A SpatialPolygons as from sp.
#' @export

Sp_Extent <- function(x, crs) {
  mpol1 <- rbind(c(xmin(x), ymin(x)),
    c(xmin(x), ymax(x)),
    c(xmax(x), ymax(x)),
    c(xmax(x), ymin(x)),
    c(xmin(x), ymin(x)))
  if (missing(crs)) {
    crs <- CRS(projection(x))
  }
  SpatialPolygonsDataFrame(
  SpatialPolygons(list(Polygons(list(Polygon(mpol1)), "Extent")),
    proj4string = crs), data = data.frame("Extent"), match.ID = FALSE)
}

#' Add alpha transparency channel to any type of color
#' Output is color in HEX format
#'
#' @param x color or vector of colors in format supported by col2rgb
#' @param alpha unique numeric value or vector of values with the same length as x
#' @param maxColorValue number giving the maximum of the color values range for alpha. Default to 255.
#'
#' @example addalpha(x = c("#FFFF00", rgb(0.6,0.5,0),"cyan"), alpha= c(0,10, 200))
#' @export

addalpha <- function(x, alpha, maxColorValue = 255) {
  # alpha between 0 and 255
  if (maxColorValue != 255) {alpha <- alpha * 255 / maxColorValue}
  x.rgb <- col2rgb(x)
  if (length(alpha) == 1) {alpha <- rep(alpha, ncol(x.rgb))
  } else {
    if (length(alpha) != ncol(x.rgb)) {
      stop("alpha should be a unique value or a vector of same length as colors in x")
    }
  }
  x.alpha <- apply(t(1:ncol(x.rgb)), 2, function(y) {
    z <- c(x.rgb[,y])
    rgb(z[1], z[2], z[3], alpha[y], maxColorValue = 255)
  })
  x.alpha
}

#' Calculate the position of the sun according to
#' date and geographical position in wgs84
#' Found here:
#' http://stackoverflow.com/questions/8708048/position-of-the-sun-given-time-of-day-latitude-and-longitude
#' @param year
#' @export

sunPosition <- function(year, month, day, hour=12, min=0, sec=0,
                    lat=46.5, long=6.5) {

    twopi <- 2 * pi
    deg2rad <- pi / 180

    # Get day of the year, e.g. Feb 1 = 32, Mar 1 = 61 on leap years
    month.days <- c(0,31,28,31,30,31,30,31,31,30,31,30)
    day <- day + cumsum(month.days)[month]
    leapdays <- year %% 4 == 0 & (year %% 400 == 0 | year %% 100 != 0) & 
                day >= 60 & !(month==2 & day==60)
    day[leapdays] <- day[leapdays] + 1

    # Get Julian date - 2400000
    hour <- hour + min / 60 + sec / 3600 # hour plus fraction
    delta <- year - 1949
    leap <- trunc(delta / 4) # former leapyears
    jd <- 32916.5 + delta * 365 + leap + day + hour / 24

    # The input to the Atronomer's almanach is the difference between
    # the Julian date and JD 2451545.0 (noon, 1 January 2000)
    time <- jd - 51545.

    # Ecliptic coordinates

    # Mean longitude
    mnlong <- 280.460 + .9856474 * time
    mnlong <- mnlong %% 360
    mnlong[mnlong < 0] <- mnlong[mnlong < 0] + 360

    # Mean anomaly
    mnanom <- 357.528 + .9856003 * time
    mnanom <- mnanom %% 360
    mnanom[mnanom < 0] <- mnanom[mnanom < 0] + 360
    mnanom <- mnanom * deg2rad

    # Ecliptic longitude and obliquity of ecliptic
    eclong <- mnlong + 1.915 * sin(mnanom) + 0.020 * sin(2 * mnanom)
    eclong <- eclong %% 360
    eclong[eclong < 0] <- eclong[eclong < 0] + 360
    oblqec <- 23.439 - 0.0000004 * time
    eclong <- eclong * deg2rad
    oblqec <- oblqec * deg2rad

    # Celestial coordinates
    # Right ascension and declination
    num <- cos(oblqec) * sin(eclong)
    den <- cos(eclong)
    ra <- atan(num / den)
    ra[den < 0] <- ra[den < 0] + pi
    ra[den >= 0 & num < 0] <- ra[den >= 0 & num < 0] + twopi
    dec <- asin(sin(oblqec) * sin(eclong))

    # Local coordinates
    # Greenwich mean sidereal time
    gmst <- 6.697375 + .0657098242 * time + hour
    gmst <- gmst %% 24
    gmst[gmst < 0] <- gmst[gmst < 0] + 24.

    # Local mean sidereal time
    lmst <- gmst + long / 15.
    lmst <- lmst %% 24.
    lmst[lmst < 0] <- lmst[lmst < 0] + 24.
    lmst <- lmst * 15. * deg2rad

    # Hour angle
    ha <- lmst - ra
    ha[ha < -pi] <- ha[ha < -pi] + twopi
    ha[ha > pi] <- ha[ha > pi] - twopi

    # Latitude to radians
    lat <- lat * deg2rad

    # Azimuth and elevation
    el <- asin(sin(dec) * sin(lat) + cos(dec) * cos(lat) * cos(ha))
    az <- asin(-cos(dec) * sin(ha) / cos(el))

    # For logic and names, see Spencer, J.W. 1989. Solar Energy. 42(4):353
    cosAzPos <- (0 <= sin(dec) - sin(el) * sin(lat))
    sinAzNeg <- (sin(az) < 0)
    az[cosAzPos & sinAzNeg] <- az[cosAzPos & sinAzNeg] + twopi
    az[!cosAzPos] <- pi - az[!cosAzPos]

    # if (0 < sin(dec) - sin(el) * sin(lat)) {
    #     if(sin(az) < 0) az <- az + twopi
    # } else {
    #     az <- pi - az
    # }


    el <- el / deg2rad
    az <- az / deg2rad
    lat <- lat / deg2rad

    return(list(elevation=el, azimuth=az))
}

#' A function to create a map output with legend, north, scale and metadata from a spatial dataset with a continous data scale
#' 
#' @param x Spatial dataset to draw. A RasterLayer or SpatialPolygonsDataFrame with a continuous numeric scale
#' @param y Layer number or name to be drawn
#' @param map.title A title for the final map
#' @param map.subtitle A (not required) subtitle for the final map
#' @param bins Integer. Number of bins into which to divide the continuous values of the spatial dataset. Default to 5. Only if breaks is NULL.
#' @param breaks User defined breaks. Replace bins.
#' @param prec precision relative to difference between min and max for bins. Decrease for higher precision. Default to 2.
#' @param zero Is zero the lowest value that should be separated from bins ? Only work if minimum value of breaks (or of automatic breaks issued from bins) is zero.
#' @param myPal color palette as issued from colorRampPalette. Default to rainbow function.
#' @param quantiles Logical. Whether to divide through quantiles (TRUE) or regular bins (FALSE). Default to FALSE.
#' @param ground SpatialPolygons of coastal countries. Issued from 'worldHire' of library(maps) by default if projection x is Lonlat.
#' @param ground.pos 1 or 2. Whether the ground is over (1) or hinter (2) the map.
#' @param fig.sup A string to be evaluated as Rcode to add overlay figure (Example : fig.sup = 'points(0,0); points(1,1, col = \"red\")'). Do not forget add = TRUE if needed. Default to NULL.
#' @param zoom.out.main Numeric vector of 1, 2, or 4 elements. If zoom.out.main > 0, this extends the spatial extent of the main view. Value is in degrees for longlat projection. Default to 0.1.
#' @param zoom.out.panel Numeric vector of 1, 2, or 4 elements. If zoom.out.panel > 0, this extends the spatial extent of the panel 3 view (situation large view). Value is in degrees for longlat projection. Default to 0.5 * spatial extent.
#' @param length.sc Numeric. Length (in meters) of the scale bar to be drawn. Default to 20000 (20km).
#' @param legend.title String. Title of the legend. Default to "Legend"
#' @param legend.type Choose between "seqgradient" (for a bins style legend), "gradient" (for a unique palette gradient) or "class" for class factors. Only "seqgradient" is working for now.
#' @param metadata Logical. Whether or not metadata are implemented. Highly recommended to set to TRUE...
#' @param legend.comments String. Special comments to add to the legend. Data type, campaign details, ... Use '\n' for new line.
#' @param legend.source String. Source of maps used if any. Use '\n' for new line.
#' @param legend.prod String. Producers of the map. Date may be added manually. Use '\n' for new line.
#' @param legend.proj String. Coordinate reference system used to map. Default to longlat.
#' @param cex.title Title text size. Subtitle is proportional.
#' @param cex.legend Text size for the graduated color legend.
#'
#' @example map.output(x, map.title = "Title")
#' @export
map.output <- function(x, y = NULL, map.title, map.subtitle = NULL, data.type = "rast", 
  bins = 5, breaks = NULL, zero = FALSE, prec = 2,
  myPal = NULL, quantiles = FALSE, ground = NULL, ground.pos = 2, fig.sup = NULL,
  zoom.out.main = 0, zoom.out.panel,  #c(100, 50),
  length.sc = 20000, legend.title = "Legend", legend.type = "seqgradient", metadata = TRUE,
  legend.comments = NULL, legend.source = NULL,
  legend.prod = paste("S. Rochette,", format(Sys.time(), "%b %Y")), legend.proj = proj4string(x),
  cex.title = 5, cex.legend = 2.5) {
  
  if (missing(map.title)) {stop("The map should have a title !")}
  if (is.null(myPal)) {
    myPal <- rainbow
  }
  if (missing(zoom.out.panel)) {
    zoom.out.panel <- c(xmax(x) - xmin(x), ymax(x) - ymin(x)) * 0.5
  }
  # Bins should be integer
  bins <- round(bins)
  
  if (data.type == "rast") {
    if (!is.null(y)) {
      x <- raster(x, y)
    }
    brk <- brk.bins(x = x, bins = bins, quantiles = quantiles, prec = prec)
  }
  if (data.type == "shp") {
    if (is.null(y)) {y <- 1}
    brk <- brk.bins(x = data.frame(x)[,y], bins = bins, quantiles = quantiles,
      prec = prec)
    lev.y <- as.numeric(as.character(cut(x[,y],
      breaks = brk, include.lowest = TRUE, right = TRUE,
      labels = 1:(nb.lev))))   
  }
  if (!is.null(breaks)) {
    brk <- breaks 
  }
  
  # Define layout
  # mat <- matrix(1:4, ncol = 2, nrow = 2)
  mat <- matrix(1:4, ncol = 2, nrow = 2)
  layout(mat, widths = c(5, 2), heights = c(1, 3),
    respect = TRUE)
  
  par(mai = c(0.1, 0.1, 0.1, 0.1),
    oma = c(1.5, 1.5, 1, 1), bty = "o")
  # panel 1: Title
  plot(1, 1, col = NA, xaxt = "n", yaxt = "n",
    xlab = "", ylab = "")

  if (is.null(map.subtitle)) {
    text(1, 1, map.title, cex = cex.title, col = 1)
  } else {
    text(1, 1, map.title, cex = cex.title, col = 1,
      pos = 3)
    text(1, 0.9, map.subtitle, cex = cex.title * 0.8,
      col = 1, pos = 1)
  }
  
  # panel 2: Map
  # plot larger scale area
  if (length(zoom.out.main) == 1) {zoom.out.main <- rep(zoom.out.main, 4)}
  if (length(zoom.out.main) == 2) {zoom.out.main <- c(rep(zoom.out.main, each = 2))}
  # if (zoom.out.main >= 0) {
    # Zoom out
  #  Ext.out <- extent(extend(x, round(zoom.out.main)))
  #} else {
    # Zoom in
    Ext.out <- Ext.tmp <- extent(x)
    Ext.out[1] <- Ext.tmp[1] - res(x)[1] * round(zoom.out.main[1])
    Ext.out[2] <- Ext.tmp[2] + res(x)[1] * round(zoom.out.main[2])
    Ext.out[3] <- Ext.tmp[3] - res(x)[2] * round(zoom.out.main[3])
    Ext.out[4] <- Ext.tmp[4] + res(x)[2] * round(zoom.out.main[4])
  #}
  plot(Ext.out, col = NA, cex.axis = 1.5)
  
  ground.m <- cbind(c(1,2), c(2,1))
  for (pos in ground.m[,ground.pos]) {
    if (pos == 1) {
      par(mfg = c(1,2))
      # plot data
      if (data.type == "shp") {
        plot(x, col = myPal(length(brk) - 1)[lev.y],
          add = TRUE)
      }
      if (data.type == "rast") {
        if (!zero | (zero & min(brk) < 0)) {
          image(x, col = myPal(length(brk) - 1), breaks = brk,
            add = TRUE)
        } else {
          image(x, col = c("white", myPal(length(brk) - 1)), breaks = c(-1, brk),
            add = TRUE)
        }
      }
    }
    if (pos == 2) {
      par(mfg = c(1,2))
      # plot coastlines
      if (!is.null(ground)) {
        # test <- tryCatch(GDALinfo(ground@file@name), error = function(e) "not.raster")
        # if (test[1] == "not.raster") {
        if (is(ground)[2] != "Raster") {
          plot(ground, col = "grey80", add = TRUE)
        } else {
          if (nlayers(ground) == 3) {
            plotRGB(ground, add = TRUE)
          } else {
            plot(ground, legend = FALSE, add = TRUE)
            # layout is modified after plot.raster
            layout(mat, widths = c(5, 2), heights = c(1, 3),
              respect = TRUE)   
          }           
        }
      } else {
        if (isLonLat(x)) {
          map("worldHires",
            xlim = c(xmin(Ext.out), xmax(Ext.out)),
            ylim = c(ymin(Ext.out), ymax(Ext.out)),
            add = TRUE, fill = TRUE, col = "grey80")
        }
      }
    }
  }
  if (!is.null(fig.sup)) {
    eval(parse(text = fig.sup))
  }
  # plot area extent
  par(mfg = c(1,2))
  plot(Sp_Extent(x), col = NA, border = "black",
    add = TRUE)
  # add km scale
  # Calculate scale in meters if proj in wgs84
  # Position of origin of scale
  pos.orig.x <-
    par("usr")[1] + 0.05 * (par("usr")[2] - par("usr")[1])
  pos.orig.y <-
    par("usr")[3] + 0.05 * (par("usr")[4] - par("usr")[3])
  pos.0.x <- pos.orig.x
  pos.0.y <-
    par("usr")[3] + 0.04 * (par("usr")[4] - par("usr")[3])
  
  if (isLonLat(x)) {
    p <- c(pos.0.x, pos.0.y)
    scale.l <- destPoint(p, b = 89.9, d = length.sc)[1] - pos.0.x
  } else {
    scale.l <- length.sc
  }
  # position of end of scale
  pos.l.x <- pos.0.x + scale.l
  pos.l.y <- pos.0.y
  # double scale bar
  for (i in c(0,scale.l / 2)) {
    SpatialPolygonsRescale(
      layout.scale.bar(0.1),
      offset = c(pos.orig.x + i, pos.orig.y),
      scale = c(scale.l / 2, scale.l / 5),
      plot.grid = F,
      fill = c("white", "black")
    )
  }
  # scale bar legend
  text(pos.0.x, pos.0.y, "0", cex = 1.8, pos = 1)
  text(pos.l.x, pos.l.y, paste0(length.sc / 1000, "km"), cex = 1.8, pos = 1)
  
  box()
  
  # panel 3: Larger map view
  if (isLonLat(x)) {
    x.wgs84 <- extent(x)
  } else {
    warning("Map is reprojected to longlat for large view panel")
    x.wgs84 <-
      extent(projectExtent(x, crs = "+proj=longlat +datum=WGS84"))
  }
  
  Ext.out.large <- extend(x.wgs84, zoom.out.panel)
  # Fake plot to get plot size
  # par()
  par(mfg = c(2,1))
  par(mai = c(0.1, 0.1, 0.1, 0.1), bty = "o")
  plot(Ext.out.large, col = NULL, 
    xlab = "", xaxt = "n", ylab = "", yaxt = "n")

  map("worldHires",
    xlim = c(par("usr")[1], par("usr")[2]),
    ylim = c(par("usr")[3], par("usr")[4]),
    fill = TRUE, col = "ivory", add = TRUE,
    xlab = "", xaxt = "n", ylab = "", yaxt = "n") 
    
  plot(extent(x.wgs84), col = "red",
    lwd = 2, add = TRUE)
  # North arrow
  SpatialPolygonsRescale(
    layout.north.arrow(1),
    offset = c(par("usr")[1] + 0.05 * (par("usr")[2] - par("usr")[1]),
      par("usr")[3] + 0.05 * (par("usr")[4] - par("usr")[3])),
    scale = c((par("usr")[2] - par("usr")[1]) / 2, (par("usr")[4] - par("usr")[3])) / 4,
    plot.grid = F
  )
  box(bty = "o")
  
  # Panel 4: Legend
  par(mai = c(0.1, 0.1, 0.1, 0.1), bty = "o")
  # empty plot
  plot(1, 1, col = NA, xaxt = "n", yaxt = "n",
    xlab = "", ylab = "", xlim = c(0, 1),
    ylim = c(0, 1), xaxs = "i"
  )
  # xc <- par("cex") * xinch(par("cin")[1L], warn.log = FALSE)
  # letters height  
  yc <- par("cex") * yinch(par("cin")[2L], warn.log = FALSE)  
  txt.h <- -yc
  
  if (metadata) {
    par(lheight = 1)
    cex.item <- 2.5
    cex.txt <- 2
    pos <- 3

    # Metadata from bottom to top
    par(lheight = 0.8)
    # Projection
    for (meta in 1:4) {
      if (meta == 1) {
        item.txt <- "Projection:"
        if (!is.null(legend.proj) | legend.proj == "") {
          l.txt <- legend.proj
        } else {
          l.txt <- projection(x)
        }
      }
      if (meta == 2) {
        item.txt <- "Production:"
        l.txt <- legend.prod
      }
      if (meta == 3) {
        item.txt <- "Source:"
        l.txt <- legend.source 
      }
      if (meta == 4) {
        item.txt <- NULL
        l.txt <- legend.comments
      }
      
      if (!is.null(l.txt) & l.txt != "") {
        if (txt.h > -yc) {
          lines(x = c(0.3, 0.7), y = c(txt.h, txt.h) + 0.6 * yc, lty = "dashed")
          txt.h <- txt.h + 1.6 * yc
        }
        text(0.5, txt.h, l.txt, pos = pos, cex = cex.txt)
        txt.h <- txt.h + strheight(l.txt, units = "user", cex = cex.txt) + 0.6 * yc  
        text(0.5, txt.h, item.txt, pos = pos, cex = cex.item)
        txt.h <- txt.h + strheight(item.txt, units = "user", cex = cex.item) + 0.6 * yc 
      }
    }
    par(lheight = 1)
  }
  
  # Legend
  yc.legend <- yc * cex.legend
  if (legend.type == "gradient") {
    warning("gradient legend is not implemented for now")
  }
  if (legend.type == "seqgradient") {
    legend.brk <- c(paste(brk[1:(length(brk) - 1)], brk[2:length(brk)], sep = "-"))
    fill <- c(myPal(length(brk) - 1))
    if (zero & min(brk) >= 0) {
      legend.brk <- c("0", legend.brk)
      fill <- c("white", fill)  
    }
    ncol.legend <- 1
    # Double column if too big compared to metadata
    if (((2 + length(legend.brk)) * yc.legend) > (1 - txt.h)) {
     ncol.legend <- 2 
    }
    leg.dim <- legend("top",
      legend = legend.brk,
      fill = fill,
      bty = "n", cex = cex.legend, title = legend.title,
      x.intersp = 0.5, ncol = ncol.legend
    )
  }
  if (legend.type == "class") {
    warning("class legend for factors is not implemented for now")
  }
  
  if (metadata) {
    lines(c(0.05,0.95), rep(leg.dim$rect$top - leg.dim$rect$h,2))
  }

}
#### ------------
