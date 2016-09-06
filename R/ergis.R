#-------------------------------------------------------------------------------
# Copyright © 2016 the Alliance for Sustainable Energy, LLC, All Rights Reserved
#------------------------------------------------------------------------------- 
#
# Convience functions for plotting ERGIS data
#
#
# Load and transform time-series data
#
#generation = load_generation('~/ergis/data/final_report/generation_highNetLoad_R30P.csv', 'c_RT_R30P') # Not in repo
#generation = rbind(generation, load_generation('~/ergis/data/final_report/generation_highNetLoad_N30P.csv', 'c_RT_N30P')) # Not in rep
#generation = rbind(generation, load_generation('~/ergis/data/final_report/generation_highNetLoad_SRPS.csv', 'c_RT_SRPS')) # Not in repo
#generation = rbind(generation, load_generation('~/ergis/data/final_report/generation_highNetLoad_loVG.csv', 'c_RT_loVG')) # Not in repo
#load("~/ergis/data/final_report/c_RT_netinterchange.RData") # Not in repo
#c_RT_netinterchange$time = as.POSIXlt(format(c_RT_netinterchange$time, "%Y-%m-%d %H:%M"), tz="EST")
#load('~/ergis/data/final_report/c_RT_dispatch_stack_data.RData') # Not in repo
#c_RT_dispatch_stack_data$time = as.POSIXlt(format(c_RT_dispatch_stack_data$time, "%Y-%m-%d %H:%M"), tz="EST")
#
# Load/Define static ERGIS data 
#regions=readShapeSpatial('~/data/ergis/regions.shp', proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
#ergis_generators = generators=read.csv('~/data/ergis/gen.csv', header=TRUE, sep=',')
#centroids = read.csv('~/data/ergis/region-centroids.csv', header=T)
#ergis_layout = layout = as.matrix(centroids[,2:3])
#ergis_layout = as.matrix(centroids[,2:3])
#ergis_verts = sapply(centroids$ISO, toString)
#scenarios = data.frame('c_RT_loVG'='lowVG', 'c_RT_SRPS'='RTx10', 'c_RT_R30P'='RTx30', 'c_RT_N30P'='ITx30')
#ergis_colors = data.table(type=c("Hydro", "Nuclear", "Coal", "Gas CC", "Wind", "CT/Gas boiler", "Other", "Pumped Storage", "PV", "CHP-QF", "Geothermal", "Storage", "Biomass", "CSP", "Steam", "DR", "RPV"),
#                       color=c("#add8e6","#b22222","#333333","#6e8b3d","#4f94cd", "#ffb6c1","#8968cd", "#FFFFFF", "#ffc125", "gray20", "khaki1", "gray45", "mediumpurple2", "darkorange2", "orchid4", "gray60", "goldenrod2"))
#ergis_iso = c("Saskatchewan", "PJM", "SPP", "NBSO", "SERC", "IESO", "ISO-NE", "Manitoba", "NYISO", "MISO", "HQ", "FRCC")


#----------------------------------------------------------------------------
#' Plot ERGIS generation frame
#'
#' @description Draw a 1920x1080 frame of ERGIS with generator map and
#'              a dispatch stack for a single scenario
#'
#' @param t timestep of interest
draw_ergis <- function(t, scenario='c_RT_R30P', density='None',
                       types=c("Hydro", "Coal", "Gas CC", "Wind", "CT/Gas boiler", "Other", "Pumped Storage", "PV"),
                       scaling=0.002, weight=3, ...)
{
    par(bg='black', fg='white', mar=c(0.5,0.5,1.5,1), oma=c(2,0,0,2))
    par(fig=c(0, 1080/1920, 0, 1))
    draw_density(t, density, ergis_generators, ergis_generation, ergis_colors, ...)
    draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario=scenario, scaling=scaling)
    draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, scenario)
    draw_shadow(t)
    mtext("Generation & Flow", 1, -2, at=-92)

    par(fig=c(1080/1920, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
    draw_ergis_bars(t, scenario, weight)
    mtext("Regional dispatch", 1, 5)

    par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5))
    mtext(paste('Eastern Renewable Generation Integration Study', ' (', scenarios[1,scenario], ')', sep=''), font=2, cex=1.5)
    mtext(format(t, "%m-%d-2026 %H:%M EST"), 3, -1)
}

#----------------------------------------------------------------------------
#' Plot a series of ERGIS generation frames
#'
#' @description Plot a series of ergis 1920x1080 frames with a generator map
#'              a dispatch stack bar chart for a single scenario
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_ergis_series<- function(t0, tn, prefix, scenario='c_RT_R30P', density='None',
                             types=c("Hydro", "Coal", "Gas CC", "Wind", "CT/Gas boiler", "Other", "Pumped Storage", "PV"),
                             ...)
{
    ts = unique(c_RT_netinterchange$time)
    ts = ts[order(ts)]

    i0 = which.min(abs(ts-t0))
    i1 = which.min(abs(ts-tn))

    for (i in i0:i1)
    {
        t = ts[i]

        png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=1920, height=1080, pointsize=24)
        par(bg='black', fg='white')
        draw_ergis(t, scenario=scenario, density=density, types=types, weight=3,...)
        dev.off()
    }
}


#----------------------------------------------------------------------------
#' Stacked bar plot of ERGIS ISO generation
#'
#' @description Plot a stacked bar graph of dispatch of each ERGIS ISO for
#'              a given scenario
#'
#' @param t timestep
draw_ergis_bars <- function(t, scenario='c_RT_R30P', weight=3)
{
    index = c_RT_dispatch_stack_data$time==t & c_RT_dispatch_stack_data$scenario==scenario

    df = data.frame(zone=c_RT_dispatch_stack_data$zone[index],
                    type=c_RT_dispatch_stack_data$Type[index],
                    value=c_RT_dispatch_stack_data$value[index])

    types = data.table(type=c("Nuclear", "Coal", "Hydro", "Gas CC", "CT/Gas boiler", "Other", "Pumped Storage", "PV", "Wind", "Curtailment"),
                       color=c("#b22222","#333333","#add8e6","#6e8b3d","#ffb6c1","#8968cd","#888888","#ffc125","#4f94cd","#FF0000"))


    s = spread(df, zone, value, fill=0)
    m = as.matrix(s[,2:13])
    rownames(m) = s$type

    missing = types$type[!types$type %in% rownames(m)]
    for (i in missing) 
    {
        m = rbind(m, 0)
        rownames(m)[dim(m)[1]] = i
    }
    
    m=m[types$type,,drop=FALSE]
    m=m[,rev(c("NYISO", "ISO-NE", "FRCC", "SPP", "SERC", "PJM", "MISO", "HQ", "IESO", "NBSO", "Manitoba", "Saskatchewan"))]

    # aggregate Canadian ISOs
    m=cbind(rowSums(m[,1:5]),m)
    colnames(m)[1] = 'Canada'   
    m = m[,-2:-6]

    # plot
    types$type[10] = "VG Curtailment"
    par(lwd=0.5)
    b=barplot(m/1000, col=types$color, horiz=T, xlab='GW', xlim=c(0, 200), col.lab=par("fg"), col.axis=par("fg"))
    legend("topright", bty='o', legend=c(types$type,'Load'), col=par("fg"), pt.bg=c(types$color,par('fg')), pch=c(rep(22, length(types$type)),45), pt.cex=1.5, cex=0.7)

    x = as.matrix(s[s$type=='Load',2:13])
    x=x[,rev(c("NYISO", "ISO-NE", "FRCC", "SPP", "SERC", "PJM", "MISO", "HQ", "IESO", "NBSO", "Manitoba", "Saskatchewan"))]
    x = c('Canada'=sum(x[1:5]),x)[-2:-6]

    for (i in 1:length(x)) lines(rep(x[i]/1000,2), rep(b[i],2)+c(-0.5,0.5), type='l', lty=2, lwd=weight, col=par('fg'))
}


#----------------------------------------------------------------------------
#' Stacked bar plot of ERGIS ISO generation
#'
#' @description Plot a set stacked bar graphs of dispatch of each ERGIS ISO for
#'              the ERGIS scenarios
#'
#' @param t timestep 
draw_comparative_ergis_bars <- function(t, x0=1080/1920, x1=1, weight=3)
{
    types = data.table(type=c("Nuclear", "Coal", "Hydro", "Gas CC", "CT/Gas boiler", "Other", "Pumped Storage", "PV", "Wind", "Curtailment"),
                       color=c("#b22222","#333333","#add8e6","#6e8b3d","#ffb6c1","#8968cd","#888888","#ffc125","#4f94cd","#FF0000"))

    zone = rev(c("NYISO", "ISO-NE", "FRCC", "SPP", "SERC", "PJM", "MISO", "HQ", "IESO", "NBSO", "Manitoba", "Saskatchewan"))
    
    index = c_RT_dispatch_stack_data$time==t
    
    df = data.frame(zone=c_RT_dispatch_stack_data$zone[index],
                    type=c_RT_dispatch_stack_data$Type[index],
                    value=c_RT_dispatch_stack_data$value[index],
                    scenario=c_RT_dispatch_stack_data$scenario[index])

    s = spread(df, scenario, value, fill=0)

    colnames(s) = c("zone", "type", "lowVG", "ITx30", "RTx30", "RTx10")

   s = s %>%
    mutate(new_zone = ifelse(as.character(zone) %in% c("Manitoba", "Saskatchewan", "IESO", "HQ", "NBSO"), "Canada", as.character(zone))) %>%
        group_by(new_zone, type) %>% 
        summarise(ITx30 = sum(ITx30),
                  RTx30 = sum(RTx30),
                  RTx10 = sum(RTx10),
                  lowVG = sum(lowVG)) %>%
        ungroup() #turns back into a regular data frame (not really needed most the time)

    colnames(s) = c("zone", "type", "ITx30", "RTx30", "RTx10", "lowVG")
    zone = rev(c("NYISO", "ISO-NE", "FRCC", "SPP", "SERC", "PJM", "MISO", "Canada"))

    # convert to GW
    s$RTx10 = s$RTx10 / 1000
    s$RTx30 = s$RTx30 / 1000
    s$ITx30 = s$ITx30 / 1000
    s$lowVG = s$lowVG / 1000
    
    for(i in length(zone):1)
    {
        m=as.matrix(s[s$zone==zone[i],3:6])
        rownames(m) = s$type[s$zone==zone[i]]
        
        missing = types$type[!types$type %in% rownames(m)]
        for (j in missing) 
        {
            m = rbind(m, 0)
            rownames(m)[dim(m)[1]] = j
        }
        
        m=m[types$type,,drop=FALSE]

        par(mar=c(0.1,10.1,0.1,0.1), oma=c(6,8,2,2), fig=c(x0, x1, (i-1)/8, i/8), lwd=0.5, new=TRUE)
        
        if (i==1)
            b=barplot(m, col=types$color, horiz=T, xlab='MW', space=0, xlim=c(0,200), col.lab=par("fg"), col.axis=par("fg"), las=1, cex.names=0.5)
        else
            b=barplot(m, col=types$color, horiz=T, space=0, xlim=c(0,200), xaxt='n', col.lab=par("fg"), col.axis=par("fg"), las=1, cex.names=0.5)
        
        y = rep(b,each=2)+c(-0.5,0.5)
        x = rep(as.matrix(s[s$type=='Load' & s$zone==zone[i],3:6]), each=2)

        lines(x, y, type='l', lty=2, lwd=1, col=par('fg'))
        
        mtext(zone[i], 2, line=4, las=1)
    }
    mtext("GW", 1, 2.5)
    
    par(fig=c(x0,x1,0,1), new=TRUE)
    legend("topright", bty='o', legend=c(types$type,'Load'), col=par("fg"), pt.bg=c(types$color,par('fg')), pch=c(rep(22, length(types$type)),45), pt.cex=0.75, cex=0.35)
}

#----------------------------------------------------------------------------
#' Plot chord view of ERGIS data
#'
#' @description Draw a 1920x1080 frame of ERGIS with chord diagram and
#'              a dispatch stack for a single scenario
#'
#' @param t starting timestep
#' @param max_interchange maximum interchange to scale the chord diagram
draw_ergis_chord = function(t, max_interchange=0, scenario='c_RT_R30P', weight=3)
{
    index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
    interchange = sum(c_RT_netinterchange$value[index])

    # Sanity check
    # if (max_interchange < interchange) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
    max_interchange = max(interchange, max_interchange)

    print(format(t, "%m-%d-%Y %H:%M EST"))
    par(bg='black', fg='white')
    #par(bg='white', fg='black')
    par(fig=c(0, 1080/1920, 0, 1), mar=c(0.5,0.5,1.5,1), oma=c(2,0,0,2))
    circos.clear()
    circos.par(gap.degree=1 + (348/12) * (max_interchange-interchange)/max_interchange)
    draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, scenario, weight)
    mtext("Net interchange", 1)
    
    par(fig=c(1080/1920, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
    draw_ergis_bars(t, scenario, weight)
    mtext("Regional dispatch", 1, 5)

    par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5))
    mtext(paste('Eastern Renewable Generation Integration Study', ' (', scenarios[1,scenario], ')', sep=''), font=2, cex=1.5)
    mtext(format(t, "%m-%d-2026 %H:%M EST"), 3, -1)

}

#----------------------------------------------------------------------------
#' Plot a series of ERGIS net interchange frames
#'
#' @description Plot a series of ergis 1920x1080 frames with a chord diagram
#'              and a dispatch stack bar chart for a single scenario
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_ergis_chord_series = function(t0, tn, max_interchange=0, scenario='c_RT_R30P', prefix)
{
    ts = unique(c_RT_netinterchange$time)
    ts = ts[order(ts)]

    i0 = which.min(abs(ts-t0))
    i1 = which.min(abs(ts-tn))

    for (i in i0:i1)
    {
        t = ts[i]

        png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=1920, height=1080, pointsize=24)
        draw_ergis_chord(t, max_interchange, scenario, weight=3)
        dev.off()
    }
}               

#----------------------------------------------------------------------------
#' Plot comparative view of ERGIS generation data
#'
#' @description Plot a 1920x1080 frame of ERGIS generation for all four
#'              scenarios in a 2x2 pattern with a dispatch bar chart.
#' 
#' @param t starting timestep 
draw_ergis_comparative_map = function(t,
                                      density='None',
                                      types=c("Hydro", "Coal", "Gas CC", "Wind", "CT/Gas boiler", "Other", "Pumped Storage", "PV"),
                                      scaling=0.002, weight=1,
                                      ...)
{
    print(format(t, "%m-%d-%Y %H:%M EST"))
    par(cex=0.65, bg='black', fg='white')
    par(fig=c(0, 540/1920, 0, 0.5), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0))

    draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_R30P', ...)
    draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_R30P', scaling=scaling)
    draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, 'c_RT_R30P', arrow.scaling=4.0)
    draw_shadow(t)
    text(-105,25, 'R30P', cex=1)

    par(fig=c(540/1920, 1080/1920, 0, 0.5), mar=c(0.5,0.5,0.5,0.5), new=TRUE)
    draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_N30P', ...)
    draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_N30P', scaling=scaling)
    draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,  'c_RT_N30P', arrow.scaling=4.0)
    draw_shadow(t)
    text(-105,25, 'N30P', cex=1)

    par(fig=c(0, 540/1920, 0.5, 1), mar=c(0.5,0.5,0.5,0.5), new=TRUE)
    draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_loVG', ...)
    draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_loVG', scaling=scaling)
    draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,  'c_RT_loVG', arrow.scaling=4.0)
    draw_shadow(t)
    text(-105,25, 'loVG', cex=1)
    
    par(fig=c(540/1920, 1080/1920, 0.5, 1), mar=c(0.5,0.5,0.5,0.5), new=TRUE)
    draw_density(t, 'None',  ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_SRPS', ...)
    draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_SRPS', scaling=scaling)
    draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, 'c_RT_SRPS', arrow.scaling=4.0)
    draw_shadow(t)
    text(-105,25, 'SRPS', cex=1)
    
    par(cex=1)
    par(fig=c(0, 1080/1920, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
    mtext("Generation & Flow", 1)

    par(fig=c(1080/1920, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
    draw_comparative_ergis_bars(t, weight=3)
    mtext("Regional dispatch", 1, 4)
    
    par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2))
    mtext('ERGIS', 3, -1.25, font=2, cex=1.5, outer=TRUE)
    mtext(format(t, "%m-%d-2026 %H:%M EST"), 3, -2.25, outer=TRUE)
}

#----------------------------------------------------------------------------
#' Plot comparative view of ERGIS net interchange data
#'
#' @description Plot a 1920x1080 frame of ERGIS net interchange for all four
#'              scenarios in a 2x2 pattern of chord diagrams with a dispatch
#'              bar chart.
#'
#' @param t starting timestep 
draw_ergis_comparative = function(t, max_interchange=0)
{
    w = 540/1920
    h = 0.5

    index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_R30P' & c_RT_netinterchange$value > 0
    r30p_interchange = sum(c_RT_netinterchange$value[index])
    index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_N30P' & c_RT_netinterchange$value > 0
    n30p_interchange = sum(c_RT_netinterchange$value[index])
    index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_loVG' & c_RT_netinterchange$value > 0
    lovg_interchange = sum(c_RT_netinterchange$value[index])
    index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_SRPS' & c_RT_netinterchange$value > 0
    srps_interchange = sum(c_RT_netinterchange$value[index])

    # Sanity check
    # if (max_interchange < max(r30p_interchange, n30p_interchange, lovg_interchange, srps_interchange)) print(paste(scenario, interchange, format(t, "%m-%d-%Y %H:%M EST")))
    max_interchange = max(max_interchange, r30p_interchange, n30p_interchange, lovg_interchange, srps_interchange)

    print(format(t, "%m-%d-%Y %H:%M EST"))
    par(fig=c(0, w, 0, h), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0))    
    circos.clear()
    circos.par(gap.degree=1 + (348/12) * (max_interchange-r30p_interchange)/max_interchange)
    draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_R30P')    
    text(-0.90,-0.90, 'R30P', cex=1)
    
    par(fig=c(w, 2*w, 0, h), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
    circos.clear()
    circos.par(gap.degree=1 + (348/12) * (max_interchange-n30p_interchange)/max_interchange)
    draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_N30P')   
    text(-0.90,-0.90, 'N30P', cex=1)
    
    par(fig=c(0, w, h, 2*h), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
    circos.clear()
    circos.par(gap.degree=1 + (348/12) * (max_interchange-lovg_interchange)/max_interchange)
    draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_loVG')    
    text(-0.90,-0.90, 'loVG', cex=1)
    
    par(fig=c(w, 2*w, h, 2*h), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
    circos.clear()
    circos.par(gap.degree=1 + (348/12) * (max_interchange-srps_interchange)/max_interchange)
    draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_SRPS')    
    text(-0.90,-0.90, 'SRPS', cex=1)
    
    par(cex=1)
    par(fig=c(0, 2*w, 0, 2*h), oma=c(2,0,0,2), las=1, new=TRUE)
    mtext("Net interchange", 1)
    
    par(fig=c(1080/1920, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
    par(fig=c(2*w, 1, 0, 1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0))
    draw_comparative_ergis_bars(t, weight=3)
    mtext("Regional dispatch", 1, 4)
    
    par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2))
    mtext('ERGIS', 3, -1.25, font=2, cex=1.5, outer=TRUE)
    mtext(format(t, "%m-%d-2026 %H:%M EST"), 3, -2.25, outer=TRUE)
}


#----------------------------------------------------------------------------
#' Plot ERGIS layout for Insight Center 
#'
#' @description Plot a 5760x2400 frame of ERGIS generation, net interchange
#'              and dispatch for a single scenario
#'
#' @param t timestep of interest
draw_ergis_insight <- function(t, scenario='c_RT_R30P', density='None',
                               types=c("Hydro", "Coal", "Gas CC", "Wind", "CT/Gas boiler", "Other", "Pumped Storage", "PV"),
                               max_interchange=0,
                               scaling=0.002, weight=6, ...)
{
    par(bg='black', fg='white')
    par(fig=c(0, 2400/5760, 0, 1))
    draw_density(t, density, ergis_generators, ergis_generation, ergis_colors, scenario=scenario, ...)
    draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario=scenario, scaling=scaling)
    draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, scenario)
    draw_shadow(t)
    mtext("Generation & Flow", 1)

    index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario==scenario & c_RT_netinterchange$value > 0
    interchange = sum(c_RT_netinterchange$value[index])

    max_interchange = max(max_interchange, interchange)
    
    par(fig=c(2200/5760, 4600/5760, 0, 1), mar=c(0.5,0.5,1.5,1), oma=c(2,0,0,2), new=TRUE)
    circos.clear()
    circos.par(gap.degree=1 + (348/12) * (max_interchange-interchange)/max_interchange)
    draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, scenario, weight/3)
    mtext("Net interchange", 1)

    par(fig=c(4550/5760, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
    draw_ergis_bars(t, scenario, weight)
    mtext("Regional dispatch", 1, 5)

    par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5))
    mtext(paste('ERGIS', scenarios[1,scenario]), font=2, cex=1.5)
    mtext(format(t, "%m-%d-2026 %H:%M EST"), 3, -1)
}

#----------------------------------------------------------------------------
#' Plot insight-center layout series of ERGIS data
#'
#' @description Plot a series of 5760x2400 frames of ERGIS generation,
#'              net interchange and dispatch for a single scenario
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_ergis_insight1_series = function(t0, tn, max_interchange=0, scenario='c_RT_R30P', prefix)
{
    i0 = which.min(abs(ts-t0))
    i1 = which.min(abs(ts-tn))

    for (i in i0:i1)
    {
        t = ts[i]

        png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=5760, height=2400, pointsize=52)
        draw_ergis_insight(t, max_interchange, scenario)
        dev.off()
    }
}               

#----------------------------------------------------------------------------
#' Plot ERGIS layout for scenario comparison in the Insight Center 
#'
#' @description Plot a 5760x2400 frame comparing ERGIS generation, net interchange
#'              and dispatch for all four scenarios
#'
#' @param t timestep of interest
draw_ergis_comparative_insight <- function(t,
                                           density='None',
                                           max_interchange=0,
                                           types=c("Hydro", "Coal", "Gas CC", "Wind", "CT/Gas boiler", "Other", "Pumped Storage", "PV"),
                                           scaling=0.002, weight=3, ...)
{
    par(bg='black', fg='white')
    #par(bg='white', fg='black')
    par(fig=c(0, 2400/5760, 0, 1))
    print(format(t, "%m-%d-%Y %H:%M EST"))
    print(date())

    par(fig=c(0, 1200/5760, 0.1, 0.55), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0))
    draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_R30P', ...)
    draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_R30P', scaling=scaling)
    draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data,  'c_RT_R30P', arrow.scaling=4.0)
    draw_shadow(t)
    text(-105,25, 'RTx30', cex=0.75)

    par(fig=c(1200/5760, 2400/5760, 0.1, 0.55), mar=c(0.5,0.5,0.5,0.5), new=TRUE)
    draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_N30P', ...)
    draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_N30P', scaling=scaling)
    draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, 'c_RT_N30P', arrow.scaling=4.0)
    draw_shadow(t)
    text(-105,25, 'ITx30', cex=0.75)

    par(fig=c(0, 1200/5760, 0.55, 1), mar=c(0.5,0.5,0.5,0.5), new=TRUE)
    draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_loVG', ...)
    draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_loVG', scaling=scaling)
    draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, 'c_RT_loVG', arrow.scaling=4.0)
    draw_shadow(t)
    text(-105,25, 'lowVG', cex=0.75)
    
    par(fig=c(1200/5760, 2400/5760, 0.55, 1), mar=c(0.5,0.5,0.5,0.5), new=TRUE)
    draw_density(t, 'None', ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_SRPS', ...)
    draw_generators(t, types, ergis_generators, ergis_generation, ergis_colors, scenario='c_RT_SRPS', scaling=scaling)
    draw_interchange(t, ergis_verts, ergis_layout, c_RT_netinterchange, c_RT_dispatch_stack_data, 'c_RT_SRPS', arrow.scaling=4.0)
    draw_shadow(t)
    text(-105,25, 'RTx10', cex=0.75)
    
    par(cex=1)
    par(fig=c(0, 2400/5760, 0, 1), oma=c(2,0,0,2), las=1, new=TRUE)
    mtext("Generation & Flow", 1)

    print("interchange")
    print(date())

    w = 1200/5760
    h = 0.5

    index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_R30P' & c_RT_netinterchange$value > 0
    r30p_interchange = sum(c_RT_netinterchange$value[index])
    index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_N30P' & c_RT_netinterchange$value > 0
    n30p_interchange = sum(c_RT_netinterchange$value[index])
    index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_loVG' & c_RT_netinterchange$value > 0
    lovg_interchange = sum(c_RT_netinterchange$value[index])
    index = c_RT_netinterchange$time==t & c_RT_netinterchange$scenario=='c_RT_SRPS' & c_RT_netinterchange$value > 0
    srps_interchange = sum(c_RT_netinterchange$value[index])

    max_interchange = max(max_interchange, max(r30p_interchange, n30p_interchange, lovg_interchange, srps_interchange)) # per timestep normalization
    
    x0 = 2300/5760
    x1 = 3500/5760
    y0 = 0
    y1 = 0.49

    par(cex=0.65)
    par(fig=c(x0, x1, y0, y1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
    circos.clear()
    circos.par(gap.degree=1 + (348/12) * (max_interchange-r30p_interchange)/max_interchange)
    draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_R30P',link.size=2)    
    text(-0.90,-0.90, 'RTx30', cex=1)
    
     x0 = 3500/5760
    x1 = 4700/5760
    y0 = 0
    y1 = 0.49
    par(fig=c(x0, x1, y0, y1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
    circos.clear()
    circos.par(gap.degree=1 + (348/12) * (max_interchange-n30p_interchange)/max_interchange)
    draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_N30P',link.size=2)   
    text(-0.90,-0.90, 'ITx30', cex=1)
    
    x0 = 2300/5760
    x1 = 3500/5760
    y0 = 0.49
    y1 = 0.98
    par(fig=c(x0, x1, y0, y1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
    circos.clear()
    circos.par(gap.degree=1 + (348/12) * (max_interchange-lovg_interchange)/max_interchange)
    draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_loVG',link.size=2)    
    text(-0.90,-0.90, 'lowVG', cex=1)
    
    x0 = 3500/5760 
    x1 = 4700/5760 
    y0 = 0.49
    y1 = 0.98
    par(fig=c(x0, x1, y0, y1), mar=c(0.5,0.5,0.5,0.5),oma=c(2,2,2,0), new=TRUE)
    circos.clear()
    circos.par(gap.degree=1 + (348/12) * (max_interchange-srps_interchange)/max_interchange)
    draw_chord_interchange(t, ergis_iso, c_RT_netinterchange, 'c_RT_SRPS',link.size=2)    
    text(-0.90,-0.90, 'RTx10', cex=1)
    
    par(cex=1)
    par(fig=c(2400/5760, 2400/5760 + 2*w, 0, 2*h), oma=c(2,0,0,2), las=1, new=TRUE)
    mtext("Net interchange", 1)

    par(fig=c(4500/5760, 1, 0, 1), mar=c(5.1,4.1,2.1,2.1), oma=c(2,0,0,2), las=1, new=TRUE)
    draw_comparative_ergis_bars(t, x0=4500/5760, weight=weight)
    mtext("Regional dispatch", 1, 4)
    
    par(fig=c(0,1,0,1), mar=c(1.5,1.5,1.5,1.5), oma=c(2,0,0,2))
    mtext('Eastern Renewable Generation Integration Study', 3, -1.25, font=2, cex=1.25, outer=TRUE)
    mtext(format(t, "%m-%d-2026 %H:%M EST"), 3, -2.25, cex=1.25, outer=TRUE)
}

#----------------------------------------------------------------------------
#' Plot a series ERGIS layout for scenario comparison in the Insight Center 
#'
#' @description Plot a series of 5760x2400 frames comparing ERGIS generation,
#'              net interchange and dispatch for all four scenarios
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_comparative_insight_series = function(t0, tn, prefix, max_interchange=0)
{
    ts = unique(c_RT_netinterchange$time)
    ts = ts[order(ts)]
    
    i0 = which.min(abs(ts-t0))
    i1 = which.min(abs(ts-tn))

    for (i in i0:i1)
    {
        t = ts[i]

        png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=5760, height=2400, pointsize=52)
        draw_comparative_insight(t, 'None', max_interchange)
        dev.off()
    }
}      
