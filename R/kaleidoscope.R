#-------------------------------------------------------------------------------
# Copyright © 2016 the Alliance for Sustainable Energy, LLC, All Rights Reserved
#------------------------------------------------------------------------------- 

#----------------------------------------------------------------------------
#' Return a transformed generation data
#'
#' @description Return transformed generation data where generation csv data
#'              file are in the form: "name", "YYYY-MM-DD hh:mm:ss", ...
#'
#' @param filename the generation csv
#' @export load_generation
load_generation <- function(filename, scenario)
{
    generation = read.csv(filename, header=T)
    generation=tidyr::gather(generation, time, power, -name)
    names(generation)[names(generation)=='name'] = 'Generator_Name'
    generation$time = as.POSIXlt(generation$time, format='X%Y.%m.%d.%H.%M.%S', tz="EST")
    generation$scenario = scenario
    
    generation
}

#----------------------------------------------------------------------------
#' Return dispatch color based on type string
#'
#' @description Return dispatch color based on type string
#'
#' @param type the generation type
#' @param colors data table with correlating generation type with color
#' @param a opacity value (default=1, opaque). 
#' @export dispatch_color
dispatch_color <- function(type, colors, a=1)
{
    scales::alpha(colors$color[colors$type==type], a)
}

#----------------------------------------------------------------------------
#' Strip small polygons from complex SpatialPolygonsData
#'
#' @description Simplifies SpatialPolygonsData by removing small polygons
#' @export stripSmallPolys
stripSmallPolys <- function(poly, minarea=0.0)
{
  # Get the areas
  areas <- lapply(poly@polygons, 
                  function(x) sapply(x@Polygons, function(y) y@area))

  # Quick summary of the areas
  print(quantile(unlist(areas)))

  # Which are the big polygons?
  bigpolys <- lapply(areas, function(x) which(x > minarea))
  length(unlist(bigpolys))

  # Get only the big polygons
  for(i in 1:length(bigpolys)){
    if(length(bigpolys[[i]])){
      poly@polygons[[i]]@Polygons <- poly@polygons[[i]]@Polygons[bigpolys[[i]]]
      poly@polygons[[i]]@plotOrder <- 1:length(poly@polygons[[i]]@Polygons)
    }
  }
  return(poly)
}

#----------------------------------------------------------------------------
#' Draw day/night shadow
#'
#' @description Draw day/night shadow
#'
#' @param t time (EST) to plot
#' @export draw_shadow
draw_shadow <- function(t)
{
    # TBD convert t from posix
    x = NightDay::NightDay(t, -5)
    
    yy <- x$Latitude
    GHA <- x$GHA
    x <- round(GHA)
    x0 <- 360
    
    if (x < 180)
    {
        x <- x * (-1)
    }
    else
    {
        x <- x0 - x
    }
    
    lines(setdiff((x-length(yy)):(x+length(yy)),0), yy[c(length(yy):1, 1:length(yy))], lwd=0.3, col='yellow')
    polygon(setdiff((x+length(yy)):(x-length(yy)),0), yy[c(length(yy):1, 1:length(yy))], col=alpha("black",0.1), border=NA)
}

#----------------------------------------------------------------------------
#' Draw background map
#'
#' @description Draw background map
#'
#' @param shape underlying shapefile (defaults to ISO regions)
#' @export draw_map
draw_map <- function()
{
    par(mar=c(0,0,0,0),oma=c(0,0,0,0))
    plot(regions, border='#00000080', col="#808080", lwd=0.5)
}

#----------------------------------------------------------------------------
#' Draw indivdiual generators
#'
#' @description Draws individual generators as bubbles sized by generation
#'              and colored by generation type.
#'
#' @param t timestep to plot
#' @param types vector of generation type strings to plot
#' @param generators generator data frame with "Generator_Name","Node_Region","Type","lat","lon"
#' @param generation generation time-series as transformed by load_generation
#' @param colors generation type color table
#' @param scaling bubble scaling factor (default=0.001)
#' @param fill fill the generator bubble (default=TRUE)
#' @param lx x position of legend
#' @param ly y position of legend
#' @export draw_generators
draw_generators <- function(t, types, generators, generation, colors, scenario='c_RT_R30P', scaling=0.002, fill=TRUE, lx=-72, ly=39.9, annontation_color='white', legend_color='white')
{
    p <- generation[generation$time == t & generation$scenario==scenario,]

    g <- generators[generators$Type %in% types,]
    g <- merge(g, p, by='Generator_Name')
    g <- g[!is.na(g$power),]
    g <- g[g$power > 0,]
    g <- g[order(-g$power),]

    if (dim(g)[1] > 0)
    {
        if (fill)
        {
            symbols(g$lon, g$lat, circles=sqrt(scaling*g$power/pi), inches=FALSE, bg=sapply(g$Type, dispatch_color, colors=colors, a=0.75), lwd=0.2, xaxt='n', yaxt='n', xlab='', ylab='', add=TRUE, fg=annotation_color)
            
            legend(lx, ly, legend=colors$type, col=legend_color, text.col=ifelse(colors$type %in% types, par("fg"), '#777777'), lwd=0.25, pt.bg=colors$color, pch=21, pt.cex=1, bty='n', cex=0.5)
        }
        else
        {
            symbols(g$lon, g$lat, circles=sqrt(scaling*g$power/pi), inches=FALSE, lwd=0.5, xaxt='n', yaxt='n', xlab='', ylab='', add=TRUE, fg=sapply(g$Type, dispatch_color, a=0.9))
            legend(lx, ly, legend=types, bg='#737373', col=sapply(types, dispatch_color, colors=colors, a=0.9), pch=21, text.col=par("fg"))
        }

        gen_legend = data.frame(power=c(1000, 2000, 4000), lat=c(25, 26.5, 28.5), lon=c(lx+1.5, lx+1.5, lx+1.5))
        symbols(gen_legend$lon, gen_legend$lat, circles=sqrt(0.002*gen_legend$power/pi), inches=FALSE, bg='#656565', fg='white', lwd=0.4, xaxt='n', yaxt='n', xlab='', ylab='', add=TRUE)
        text(gen_legend$lon, gen_legend$lat-0.1, col=par("fg"), labels=c("1.0 GW", "2.0 GW", "4.0 GW"), cex=0.5, pos=4, offset=c(0.95))
    }
}


#----------------------------------------------------------------------------
#' Two-Dimensional Weighted Kernel Density Estimation
#'
#' @description A weighted two-dimensional kernel density estimation with an
#' axis-aligned bivariate normal kernel, evaluated on a square grid. This is
#' a weighted-version of MASS::kde2d.
#'
#' @param x x coordinate of data
#' @param y y coordinate of data
#' @param w data weights
#' @param h vector of bandwidths for x and y directions.  Defaults to
#'          normal reference bandwidth (see ‘bandwidth.nrd’). A scalar
#'          value will be taken to apply to both directions.
#' @param n Number of grid points in each direction.  Can be scalar or a
#'          length-2 integer vector.
#' @param lims The limits of the rectangle covered by the grid as ‘c(xl, xu,
#'             yl, yu)’.
#' @export kde2dw
kde2dw <- function (x, y, w, h, n = 25, lims = c(range(x), range(y)))
{
    nx <- length(x)
    if (length(y) != nx) 
        stop("data vectors must be the same length")
    if (length(w) != nx & length(w) != 1)
        stop("weight vectors must be 1 or length of data")
    gx <- seq(lims[1], lims[2], length = n) # gridpoints x
    gy <- seq(lims[3], lims[4], length = n) # gridpoints y
    if (missing(h)) 
        h <- c(bandwidth.nrd(x), bandwidth.nrd(y));
    if (missing(w)) 
        w <- numeric(nx)+1;
    h <- h/4
    ax <- outer(gx, x, "-")/h[1] # distance of each point to each grid point in x-direction
    ay <- outer(gy, y, "-")/h[2] # distance of each point to each grid point in y-direction
    z <- (matrix(rep(w,n), nrow=n, ncol=nx, byrow=TRUE)*matrix(dnorm(ax), n, nx)) %*% t(matrix(dnorm(ay), n, nx))/(sum(w) * h[1] * h[2]) # z is the density
    z[is.na(z)] <- 0

    z <- z / sum(z) # normalize
    
    return(list(x = gx, y = gy, z = z))
}

#----------------------------------------------------------------------------
#' Draw heatmap of a generation type
#'
#' @description Computes a raster from generation point sources and draws
#'              a heatmap of that density.
#'
#' @param t timestep of interest
#' @param type generation type string (e.g., "Wind")
#' @param generators generator data frame with "Generator_Name","Node_Region","Type","lat","lon"
#' @param generation generation time-series as transformed by load_generation
#' @param colors generation type color table
#' @param ramp color map (defaults to rev(brewer.Greys))
#' @param shape underlying shapefile (defaults to ISO regions)
#' @export draw_density
draw_density <- function(t, type, generators, generation, colors, scenario='c_RT_R30P', legend_color='white', ...)
{
    par(mar=c(0,0,0,0),oma=c(0,0,0,0))
    f = par("fig")

    if (type == "None") # plot the map without density heatmap
    {
        plot(regions, col="#9F9F9F", lwd=0.5)
        return()
    }
    
    p <- generation[generation$time == t & generation$scenario==scenario,]
    
    g <- generators[generators$Type == type,]
    g <- merge(g, p, by='Generator_Name')
    g <- g[!is.na(g$power),]

    p <- sum(g$power)

    d <- kde2dw(x=g$lon, y=g$lat, w=g$power, n=500, lims=as.vector(extent(regions)))

    d$z <- d$z * p
    
    if (!hasArg(ramp)) ramp = colorRampPalette(brewer.pal(8,colors$pal[colors$type==type])[1:8])(128)
    
    r <- mask(raster(d), regions) # slow - investigate canning a raster mask

    if (minValue(r) == maxValue(r))
    {
        plot(regions, col="#FFFFFF", lwd=0.5)#, xlim=c(-110,-10))
        values(r) = abs(rnorm(ncell(r), 0,1e-9)) # kludge: give raster some values to force the legend
    }
    else
    {
        dlim = density_limits$lim[density_limits==type]

        if (max(d$z) > dlim)
        {
            message(t, " Maximum ", type, " density ", max(d$z), " is larger than estimate ", dlim, ". Colormap normalization will be broken.")
            dlim = max(d$z)
        }
        
        plot(r, legend=F, axes=F, box=F, col=ramp, breaks=seq(0, dlim, length=128))#, xlim=c(-110,-10))
        par(fig=f) # raster.plot seems to break par("fig")
        plot(regions, lwd=0.5, add=T)#, xlim=c(-110,-10))
    }

    plot(r, legend.only=TRUE, col=ramp, legend.width=2,
         legend.args=list(text=paste(type, 'Generation'), side=2, line=0.5, col=legend_color),
         axis.args=list(at=c(min(0,minValue(r),na.rm=T), max(1e-9,maxValue(r),na.rm=T)), labels=c('0', 'High'), col=legend_color, col.axis=legend_color), smallplot=c(0.9, 0.95, 0.6, 0.9))
    par(fig=f) # raster.plot seems to break par("fig")

}

#----------------------------------------------------------------------------
#' Plot a group igraph edges 
#'
#' @description Plots a group of igraph edges with a given weight. Only a
#'              single weight can be used, a limitation of igraph. Plot
#'              edges of multiple weights with multiple calls
#'              (see: draw_interchange)
#'
#' @param df Data frame with a vector of "edge" strings (e.g., "FRCC SERC")
#'           that specify the source and destination, and a vector of
#'           "weights" that specify the size of the interchange from source
#'           to destination.
#' @param verts vector of vertex labels
#' @param layout vector of vertex positions
#' @export draw_edge_group
draw_edge_group <- function(df, verts, layout, arrow.scaling=2.0, edge_color='#FFFFFF8F')
{
    edges = unlist(lapply(strsplit(as.character(df$edge), ' '), function(l) { c(l[1], l[3]) }))

    g = make_empty_graph()
    g = g + vertices(verts)
    g = g + edges(edges)
    
    plot(g, layout=layout, edge.curved=0.2, rescale=F, add=T, vertex.size=250, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label=NA, edge.width=df$weight, edge.color=edge_color, edge.arrow.size=df$weight/arrow.scaling)    
}

#----------------------------------------------------------------------------
#' Plot ISO interchange data
#'
#' @param t timestep of interest
#' @param verts vector of vertex labels
#' @param layout vector of vertex positions
#' @param netinterchange net interchange data frame with
#'                       "time", "scenario", "Source2Sink", "value"
#' @param dispatch regional dispatch data frame with
#'                 "Type", "time", "zone", "value", "scenario"
#' @export draw_interchange
draw_interchange <- function(t, verts, layout, netinterchange, dispatch, scenario='c_RT_R30P', arrow.scaling=2.0, annontation_color='white', edge_color='white')
{
    # igraph does not support arrows of different weight/size on the same graph
    # so we're going to go through some contortions here. 

    # plot the vertices
    g = make_empty_graph()
    g = g + vertices(verts)

    plot(g, layout=layout, rescale=F, add=T, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label.color=annotation_color, vertex.label.cex=0.75)

    # parse the edges
    edges  = data.frame(edge=netinterchange$Source2Sink[netinterchange$time==t & netinterchange$scenario==scenario & netinterchange$value <= 0],
                        weight = abs(netinterchange$value[netinterchange$time==t & netinterchange$scenario==scenario & netinterchange$value <= 0]))

    edges$weight = ifelse(edges$weight/1000 < 0.1, 0.1, edges$weight/1000)

    edge_groups = split(edges, edges$weight)

    lapply(edge_groups, draw_edge_group, arrow.scaling=arrow.scaling, verts=verts, layout=layout)

    plot(g, layout=layout, rescale=F, add=T, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label.color=annotation_color, vertex.label.cex=0.75)

    # Curtailment label
    index = dispatch$time==t & dispatch$scenario==scenario 
    df = data.frame(zone=dispatch$zone[index], type=dispatch$Type[index], value=dispatch$value[index])
    c_label <- function(z,df) { ifelse(z %in% df$zone, ifelse(df$value[df$zone==z & df$type=='Curtailment'] / (sum(df$value[df$zone==z]) - df$value[df$zone==z & df$type=='Curtailment']) > 0.01, paste('\n\n(', format(100 * df$value[df$zone==z & df$type=='Curtailment'] / (sum(df$value[df$zone==z]) - df$value[df$zone==z & df$type=='Curtailment']), digits=1), '%)', sep=''), ''), '') }
    c_verts <- unlist(lapply(verts, FUN=c_label, df))
    
    g = make_empty_graph()
    g = g + vertices(c_verts)
    plot(g, layout=layout, rescale=F, add=T, vertex.label.font=2, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label.color=annotation_color, vertex.label.cex=0.5)

    # draw legend
    x = -70.5
    alayout = as.matrix(data.frame(lon=c(x, x+.1, x, x+.1, x, x+.1), lat=c(25, 25, 26.5, 26.5, 28.5, 28.5)))
    averts = c(1,2,3,4,5,6)
    g = make_empty_graph()
    g = g + vertices(averts)
    g = g + edges(c(1,2))
    plot(g, layout=alayout, rescale=F, add=T, vertex.size=1, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label=NA, edge.width=1, edge.color=edge_color, edge.width=0.5, edge.arrow.size=1.0/arrow.scaling)

    g = make_empty_graph()
    g = g + vertices(averts)
    g = g + edges(c(3,4))
    plot(g, layout=alayout, rescale=F, add=T, vertex.size=1, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label=NA, edge.width=1, edge.color=edge_color, edge.width=1.0, edge.arrow.size=2.0/arrow.scaling)

    g = make_empty_graph()
    g = g + vertices(averts)
    g = g + edges(c(5,6))
    plot(g, layout=alayout, rescale=F, add=T, vertex.size=1, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label=NA, edge.width=1, edge.color=edge_color, edge.width=2.0, edge.arrow.size=4.0/arrow.scaling)

}

#----------------------------------------------------------------------------
#' Plot a chord diagram showing net interchange
#'
#' @param t  timestep 
#' @param netinterchange net interchange data frame with
#'                       "time", "scenario", "Source2Sink", "value"
#' @param scenario scenario
#' @export draw_chord_interchange
draw_chord_interchange = function(t, iso, netinterchange, scenario='c_RT_R30P', link.size=1)
{
    col = adjustcolor(brewer.pal(12, 'Paired'), red.f=.75, green.f=.75, blue.f=.75)
    
    index = netinterchange$time==t & netinterchange$scenario==scenario;
    interchange = data.frame(source2sink = netinterchange$Source2Sink[index], value=netinterchange$value[index])

    interchange$source = unlist(lapply(strsplit(as.character(interchange$source2sink),' '), function(l) { l[1] }))
    interchange$sink = unlist(lapply(strsplit(as.character(interchange$source2sink),' '), function(l) { l[3] }))

    tmp = interchange$source[interchange$value<0]
    interchange$source[interchange$value<0] = interchange$sink[interchange$value<0]
    interchange$sink[interchange$value<0] = tmp
    interchange$value = abs(interchange$value)
    
    mat = matrix(0, nrow=length(iso), ncol=length(iso))
    rownames(mat) = iso
    colnames(mat) = iso
    
    for (i in seq_along(interchange$sink))
    {
        # build adjacency matrix and scale MW to GW 
        mat[as.character(interchange$sink[i]), as.character(interchange$source[i])] = interchange$value[i]/1000
    }
    
    chordDiagram(mat, directional=1, grid.col=col, direction.type="arrows",
                 link.border=1, link.lwd=0.25, link.arr.lwd=link.size,
                 link.arr.length=link.size/4, link.arr.lty=2, reduce=-1,
                 transparency=0.4)
}

