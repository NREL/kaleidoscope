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
load_generation = function(filename, scenario)
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
dispatch_color = function(type, colors, a=1)
{
    scales::alpha(colors$color[colors$type==type], a)
}

#----------------------------------------------------------------------------
#' Strip small polygons from complex SpatialPolygonsData
#'
#' @description Simplifies SpatialPolygonsData by removing small polygons
#' @export stripSmallPolys
stripSmallPolys = function(poly, minarea=0.0)
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
draw_shadow = function(t)
{
    # TBD convert t from posix
    tdiff = as.numeric(difftime(as.POSIXct(as.character(t),
        origin=as.POSIXct("1970-01-01"),tz="UTC"),t,units = "hours"))
    x = NightDay::NightDay(t, tdiff)
    
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
    polygon(setdiff((x+length(yy)):(x-length(yy)),0), yy[c(length(yy):1, 1:length(yy))], col=scales::alpha("black",0.1), border=NA)
}

#----------------------------------------------------------------------------
#' Draw background map
#'
#' @description Draw background map
#'
#' @param shape underlying shapefile (defaults to ISO regions)
#' @export draw_map
draw_map = function()
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
#' @param timestep timestep to plot
#' @param types vector of generation type strings to plot
#' @param generators generator data frame with "Generator_Name","Node_Region","Type","lat","lon"
#' @param generation generation time-series as transformed by load_generation
#' @param colors generation type color table
#' @param scaling bubble scaling factor (default=0.001)
#' @param fill fill the generator bubble (default=TRUE)
#' @param lx x position of legend
#' @param ly y position of legend
#' @param draw_legend flag for drawing legend on map
#' @export draw_generators
draw_generators = function(timestep, types, generators, generation, colors, scenario.name='c_RT_R30P', scaling=0.002, fill=TRUE, lx=-72, ly=39.9, annotation_color='white', legend_color='white', draw_legend=FALSE)
{
    p <- filter(generation,time == timestep)
    p <- filter(p,scenario==scenario.name)
    p <- p[,names(p)[names(p)!='Type']]

    g <- generators[generators$Type %in% types,]
    g <- merge(p,g, by='Generator_Name')
    g <- g[!is.na(g$power),]
    g <- g[g$power > 0,]
    g <- g[order(-g$power),]

    if (dim(g)[1] > 0)
    {
        if (fill)
        {
            symbols(g$lon, g$lat, circles=sqrt(scaling*g$power/pi), inches=FALSE, bg=sapply(g$Type, dispatch_color, colors=colors, a=0.75), lwd=0.2, xaxt='n', yaxt='n', xlab='', ylab='', add=TRUE, fg=annotation_color)
            if(draw_legend){
                legend(lx, ly, legend=colors$type, col=legend_color, text.col=ifelse(colors$type %in% types, par("fg"), '#777777'), lwd=0.25, pt.bg=colors$color, pch=21, pt.cex=1, bty='n', cex=0.5)
            }
        }
        else
        {
            symbols(g$lon, g$lat, circles=sqrt(scaling*g$power/pi), inches=FALSE, lwd=0.5, xaxt='n', yaxt='n', xlab='', ylab='', add=TRUE, fg=sapply(g$Type, dispatch_color, a=0.9))
            if(draw_legend){
                legend(lx, ly, legend=types, bg='#737373', col=sapply(types, dispatch_color, colors=colors, a=0.9), pch=21, text.col=par("fg"))
            }
        }

        if (draw_legend){
            gen_legend = data.frame(power=c(1000, 2000, 4000), lat=c(25, 26.5, 28.5), lon=c(lx+1.5, lx+1.5, lx+1.5))
            symbols(gen_legend$lon, gen_legend$lat, circles=sqrt(0.002*gen_legend$power/pi), inches=FALSE, bg='#656565', fg='white', lwd=0.4, xaxt='n', yaxt='n', xlab='', ylab='', add=TRUE)
            text(gen_legend$lon, gen_legend$lat-0.1, col=par("fg"), labels=c("1.0 GW", "2.0 GW", "4.0 GW"), cex=0.5, pos=4, offset=c(0.95))
        }
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
kde2dw = function (x, y, w, h, n = 25, lims = c(range(x), range(y)))
{
    nx <- length(x)
    if (length(y) != nx) 
        stop("data vectors must be the same length")
    if (length(w) != nx & length(w) != 1)
        stop("weight vectors must be 1 or length of data")
    gx <- seq(lims[1], lims[2], length = n) # gridpoints x
    gy <- seq(lims[3], lims[4], length = n) # gridpoints y
    if (missing(h)) 
        h <- c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y));
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
#' @param density
#' @param scenario
#' @param legend_color
#' @param type
#' @param density_limits
#' @param type generation type string (e.g., "Wind")
#' @param generators generator data frame with "Generator_Name","Node_Region","Type","lat","lon"
#' @param generation generation time-series as transformed by load_generation
#' @param colors generation type color table
#' @param ramp color map (defaults to rev(brewer.Greys))
#' @param shape underlying shapefile (defaults to ISO regions)
#' @export draw_density
draw_density = function(t, density, generators, generation, colors, scenario='c_RT_R30P', legend_color='white', type='None', density_limits = 'Null', ...)
{
    par(mar=c(0,0,0,0),oma=c(0,0,0,0))
    f = par("fig")

    if (type == "None") # plot the map without density heatmap
    {
        raster::plot(density, col="#9F9F9F", lwd=0.5)
        return()
    }
    
    p <- generation[generation$time == t & generation$scenario==scenario,]
    
    g <- generators[generators$Type == type,]
    g <- merge(g, p, by='Generator_Name')
    g <- g[!is.na(g$power),]

    p <- sum(g$power)

    d <- kde2dw(x=g$lon, y=g$lat, w=g$power, n=500, lims=as.vector(extent(density)))

    d$z <- d$z * p
    
    if (!hasArg(ramp)) ramp = colorRampPalette(brewer.pal(8,colors$pal[colors$type==type])[1:8])(128)
    
    r <- mask(raster(d), density) # slow - investigate canning a raster mask

    if (minValue(r) == maxValue(r))
    {
        plot(density, col="#FFFFFF", lwd=0.5)#, xlim=c(-110,-10))
        values(r) = abs(rnorm(ncell(r), 0,1e-9)) # kludge: give raster some values to force the legend
    }
    else
    {
        if (density_limits == 'Null') {
          dlim = max(d$z)
        } else {
          dlim = density_limits$lim[density_limits==type] #?density_limits not defined
        }

        if (max(d$z) > dlim)
        {
            message(t, " Maximum ", type, " density ", max(d$z), " is larger than estimate ", dlim, ". Colormap normalization will be broken.")
            dlim = max(d$z)
        }
        
        plot(r, legend=F, axes=F, box=F, col=ramp, breaks=seq(0, dlim, length=128))#, xlim=c(-110,-10))
        par(fig=f) # raster.plot seems to break par("fig")
        plot(density, lwd=0.5, add=T)#, xlim=c(-110,-10))
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
draw_edge_group = function(df, verts, layout, arrow.scaling=2.0, edge_color='#FFFFFF8F')
{
    edges = unlist(lapply(strsplit(as.character(df$edge), ' '), function(l) { c(l[1], l[3]) }))

    g = igraph::make_empty_graph()
    g = g + igraph::vertices(verts)
    g = g + igraph::edges(edges)
    
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
#' @export draw_interchange
draw_interchange = function(t, verts, layout, netinterchange, dispatch, scenario='c_RT_R30P', arrow.scaling=2.0, annotation_color='white', edge_color='white')
{
    # igraph does not support arrows of different weight/size on the same graph
    # so we're going to go through some contortions here. 

    # plot the vertices
    g = igraph::make_empty_graph()
    g = g + igraph::vertices(verts)

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
    
    g = igraph::make_empty_graph()
    g = g + igraph::vertices(c_verts)
    plot(g, layout=layout, rescale=F, add=T, vertex.label.font=2, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label.color=annotation_color, vertex.label.cex=0.5)

    # draw legend
    x = -70.5
    alayout = as.matrix(data.frame(lon=c(x, x+.1, x, x+.1, x, x+.1), lat=c(25, 25, 26.5, 26.5, 28.5, 28.5)))
    averts = c(1,2,3,4,5,6)
    g =igraph:: make_empty_graph()
    g = g + igraph::vertices(averts)
    g = g + igraph::edges(c(1,2))
    plot(g, layout=alayout, rescale=F, add=T, vertex.size=1, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label=NA, edge.width=1, edge.color=edge_color, edge.width=0.5, edge.arrow.size=1.0/arrow.scaling)

    g = igraph::make_empty_graph()
    g = g + igraph::vertices(averts)
    g = g + igraph::edges(c(3,4))
    plot(g, layout=alayout, rescale=F, add=T, vertex.size=1, vertex.color='#00000000', vertex.frame.color='#00000000', vertex.label=NA, edge.width=1, edge.color=edge_color, edge.width=1.0, edge.arrow.size=2.0/arrow.scaling)

    g = igraph::make_empty_graph()
    g = g + igraph::vertices(averts)
    g = g + igraph::edges(c(5,6))
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
    
    mat = mat[rowSums(mat)!=0,colSums(mat)!=0]
    
    col.len = length(unique(c(rownames(mat),colnames(mat))))
    col.copies = ifelse(col.len>12,ceiling(col.len/12), 1)
    
    col = adjustcolor(rep(RColorBrewer::brewer.pal(12, 'Paired'),col.copies)[1:col.len],
                      red.f=.75, green.f=.75, blue.f=.75)
    
    circlize::chordDiagram(mat, directional=1, grid.col=col, direction.type="arrows",
                 link.border=1, link.lwd=0.25, link.arr.lwd=link.size,
                 link.arr.length=link.size/4, link.arr.lty=2, reduce=-1,
                 transparency=0.4)
}

#----------------------------------------------------------------------------
#' Stacked bar plot of ISO generation for each scenario
#'
#' @description Plot a stacked bar graph of dispatch of each  ISO for
#'              a given scenario
#'
#' @param t timestep
#' @param scenario
#' @param dispatch
#' @param types
#' @param verts
#' @param drawing.args list of drawing arguments
#'              x0, the left edge of the barplot (1080/1920)
#'              x1, the right edge of the barplot (1)
#'              weight, the relative size of the bars (3)
#'              xmax, the maximum value of the x axis (200)
#'              lpos, the legend position string ('topright')
#'              l.pt.cex, the relative scaling of the legend points (0.75)
#'              l.cex, the relative scaling of the legend(0.35)
#'
#' @export draw_bars
draw_bars = function(t, scenario='c_RT_R30P', dispatch, types, verts, drawing.args = NULL)
{

  def.drawing.args = list(x0=0.5, weight=3, xmax = 200, lpos = 'topright', l.pt.cex = 1.5, l.cex = 0.7)    
  
  for(i in 1:length(def.drawing.args)){tempobj = def.drawing.args[[i]];eval(parse(text=paste(names(def.drawing.args)[[i]],"=tempobj")))}

  if(!is.null(drawing.args)){
    for(i in 1:length(drawing.args)){tempobj = drawing.args[[i]];eval(parse(text=paste(names(drawing.args)[[i]],"=tempobj")))}
  }

  index = dispatch$time==t & dispatch$scenario==scenario
  
  df = data.frame(zone=dispatch$zone[index],
                  type=dispatch$Type[index],
                  value=dispatch$value[index])
  
  
  s = tidyr::spread(df, zone, value, fill=0)
  m = as.matrix(s[,2:ncol(s)])
  rownames(m) = s$type
  
  missing = types$type[!types$type %in% rownames(m)]
  for (i in missing) 
  {
    m = rbind(m, 0)
    rownames(m)[dim(m)[1]] = i
  }
  
  m=m[types$type,,drop=FALSE]
  m=m[,rev(verts[verts %in% colnames(m)])]
  
  
  # plot
  #types = rbind(types,data.frame(type = 'VG Curtailment', color = 'red', pal = 'reds'))
  par(lwd=x0)
  b=barplot(m/1000, col=types$color, horiz=T, xlab='GW', xlim=c(0, xmax), col.lab=par("fg"), col.axis=par("fg"))
  if(!is.null(lpos)){
    legend(lpos, bty='o', legend=c(types$type,'Load'), col=par("fg"), pt.bg=c(types$color,par('fg')), pch=c(rep(22, length(types$type)),45), pt.cex=l.pt.cex, cex=l.cex)
  }
  x = as.matrix(s[s$type=='Load',2:ncol(s)])
  x=x[,rev(verts[verts %in% colnames(x)])]
  
  for (i in 1:length(x)) lines(rep(x[i]/1000,2), rep(b[i],2)+c(-0.5,0.5), type='l', lty=2, lwd=weight, col=par('fg'))
}

#----------------------------------------------------------------------------
#' Stacked bar plot of ISO generation for each region
#'
#' @description Plot a stacked bar graph of dispatch of each  ISO for
#'              a given scenario
#'
#' @param t timestep
#' @param verts vector of vertex labels
#' @param types generation type color table
#' @param dispatch regional dispatch data frame with
#' @param drawing.args list of drawing arguments
#'              x0, the left edge of the barplot (1080/1920)
#'              x1, the right edge of the barplot (1)
#'              weight, the relative size of the bars (3)
#'              xmax, the maximum value of the x axis (200)
#'              lpos, the legend position string ('topright')
#'              l.pt.cex, the relative scaling of the legend points (0.75)
#'              l.cex, the relative scaling of the legend(0.35)
#'                      
#' @export draw_comparative_bars
draw_comparative_bars = function(t, dispatch, types, verts, drawing.args = NULL)
{
  def.drawing.args = list(x0=1080/1920, x1=1, weight=3, xmax = 200, lpos = 'topright', l.pt.cex = 0.75, l.cex = 0.35)    
  
  for(i in 1:length(def.drawing.args)){tempobj = def.drawing.args[[i]];eval(parse(text=paste(names(def.drawing.args)[[i]],"=tempobj")))}

  if(!is.null(drawing.args)){
    for(i in 1:length(drawing.args)){tempobj = drawing.args[[i]];eval(parse(text=paste(names(drawing.args)[[i]],"=tempobj")))}
  }

  index = dispatch$time==t
  
  df = data.frame(zone=dispatch$zone[index],
                  type=dispatch$Type[index],
                  value=dispatch$value[index]/1000, #convert to GW
                  scenario=dispatch$scenario[index])
  
  s = tidyr::spread(df, scenario, value, fill=0)[,c('zone','type',as.character(unique(df$scenario)))]
  
  zone = rev(verts[verts %in% unique(s$zone)])
  
  for(i in length(zone):1)
  {
    m=as.matrix(s[s$zone==zone[i],3:ncol(s)])
    rownames(m) = s$type[s$zone==zone[i]]
    
    missing = types$type[!types$type %in% rownames(m)]
    for (j in missing) 
    {
      m = rbind(m, 0)
      rownames(m)[dim(m)[1]] = j
    }
    
    m=m[types$type,,drop=FALSE]
    
    par(mar=c(0.1,10.1,0.1,0.1), oma=c(6,8,2,2), fig=c(x0, x1, (i-1)/length(zone), i/length(zone)), lwd=0.5, new=TRUE)
    
    if (i==1)
      b=barplot(m, col=types$color, horiz=T, xlab='MW', space=0, xlim=c(0,xmax), col.lab=par("fg"), col.axis=par("fg"), las=1, cex.names=0.5)
    else
      b=barplot(m, col=types$color, horiz=T, space=0, xlim=c(0,xmax), xaxt='n', col.lab=par("fg"), col.axis=par("fg"), las=1, cex.names=0.5)
    
    y = rep(b,each=2)+c(-0.5,0.5)
    x = rep(as.matrix(s[s$type=='Load' & s$zone==zone[i],3:ncol(s)]), each=2)
    
    lines(x, y, type='l', lty=2, lwd=1, col=par('fg'))
    
    mtext(zone[i], 2, line=4, las=1)
  }
  mtext("GW", 1, 2.5)
  
  par(fig=c(x0,x1,0,1), new=TRUE)
  if(!is.null(lpos)){
    legend(lpos, bty='n', legend=c(types$type,'Load'), col=par("fg"), pt.bg=c(types$color,par('fg')), pch=c(rep(22, length(types$type)),45), pt.cex=l.pt.cex, cex=l.cex)    
  }

}


#----------------------------------------------------------------------------
#' Comparitave maps with regional stacked bars 
#'
#' @description Plot a set of maps and stacked bar graph of dispatch of each  ISO for
#'              a given set of scenarios
#'
#' @param t timestep
#' @param verts vector of vertex labels
#' @param types generation type color table
#' @param dispatch regional dispatch data frame with
#'                 "Type", "time", "zone", "value", "scenario"
#' @param generation nodal generation dataframe
#' @param colors generation-colors datafraem
#' @param interchange regional interchange dataframe
#' @param generators to node/zone mapping
#' @param layout 
#' @param scenarios list of scenarios
#' @param m.drawing.args list of map drawing args 
#'              scaling, the size of the generator dots (0.002)
#'              map_coords, list of coordinates for each map image (list(c(0, 540/1920, 0, 0.5),c(540/1920, 1080/1920, 0, 0.5),c(0, 540/1920, 0.5, 1),c(540/1920, 1080/1920, 0.5, 1))))
#'              arrow.scaling, the relative scaling of the flow arrows (2)
#' @param b.drawing.args list of barplot drawing args (see draw_comparative_bars)
#' @export draw_comparative_map
draw_comparative_map = function(t,
                                density='None',
                                types=c("Hydro", "Coal", "Gas CC", "Wind", "CT/Gas boiler", "Other", "Pumped Storage", "PV"),
                                studyname = 'ERGIS',
                                generators,
                                gen,
                                colors,
                                scenarios,
                                verts,
                                layout,
                                interchange,
                                dispatch,
                                m.drawing.args = NULL,
                                b.drawing.args = NULL,
                                ...)
{
  def.m.drawing.args = list(scaling=0.002, 
    map_coords = list(c(0, 540/1920, 0, 0.5),c(540/1920, 1080/1920, 0, 0.5),c(0, 540/1920, 0.5, 1),c(540/1920, 1080/1920, 0.5, 1)),
    arrow.scaling = 2,
    draw_legend = F,
    fig_sep = 1080/1920)    
  
  for(i in 1:length(def.m.drawing.args)){tempobj = def.m.drawing.args[[i]];eval(parse(text=paste(names(def.m.drawing.args)[[i]],"=tempobj")))}

  if(!is.null(m.drawing.args)){
    for(i in 1:length(m.drawing.args)){tempobj = m.drawing.args[[i]];eval(parse(text=paste(names(m.drawing.args)[[i]],"=tempobj")))}
  }

  print(format(t, "%m-%d-%Y %H:%M %Z"))
  par(cex=0.65, bg='black', fg='white')
  
  for (i in 1:length(scenarios)){
    new = ifelse(i>1,T,F)
    par(fig=map_coords[[i]], mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0),new = new)
    
    draw_density(t, density, generators, dispatch, colors,scenario = scenarios[i])#, type = 'Wind-Wind')
    draw_generators(t, types = types, generators, gen, colors = colors, scenario.name=scenarios[i], scaling=scaling, lx = as.numeric(quantile(layout[,1])[1]-1),ly = as.numeric(quantile(layout[,2])[2]),draw_legend = draw_legend)
    draw_interchange(t, verts, layout, interchange, dispatch, scenario=scenarios[i], arrow.scaling=arrow.scaling)
    draw_shadow(t)
    text(x = max(layout[,1]),y = max(layout[,2]), labels = scenarios[i], cex=1.5, col = 'white')
  }
  
  
  par(cex=1)
  par(fig=c(0, fig_sep, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
  mtext("Generation & Flow", 1)
  
  par(fig=c(fig_sep, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
  if(is.null(b.drawing.args)){b.drawing.args = list(xmax = 5, weight=3)}
  draw_comparative_bars(t, dispatch = dispatch, types = PH_colors, verts = verts,drawing.args = b.drawing.args)
  mtext("Regional dispatch", 1, 4)
  
  par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2))
  mtext(studyname, 3, -1.25, font=2, cex=1.5, outer=TRUE)
  mtext(format(t, "%m-%d-2026 %H:%M %Z"), 3, -2.25, outer=TRUE)
}

