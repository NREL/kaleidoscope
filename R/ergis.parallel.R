#-------------------------------------------------------------------------------
# Copyright © 2016 the Alliance for Sustainable Energy, LLC, All Rights Reserved
#------------------------------------------------------------------------------- 

#----------------------------------------------------------------------------
#' Parallel Plot ERGIS data
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
draw_ergis_parallel_series<- function(t0, tn, prefix, scenario='c_RT_R30P', density='Wind',
                             types=c("Hydro", "Coal", "Gas CC", "Wind", "CT/Gas boiler", "PV"),
                             ...)
{
    ts = unique(ergis_generation$time)
    ts = ts[order(ts)]

    i0 = which.min(abs(ts-t0))
    i1 = which.min(abs(ts-tn))
    
    i = i0:i1

    cluster = makeCluster(detectCores()-1, type="FORK")
    parLapply(cluster, i0:i1, function(i) {
        t = ts[i]

        png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=1920, height=1080, pointsize=24)
        par(bg='black', fg='white')
        draw_ergis(t, scenario=scenario, density=density, types=types, ...)
        dev.off()
    })
    stopCluster(cluster)
}

#----------------------------------------------------------------------------
#' Parallel Plot ERGIS data
#'
#' @param t0 starting timestep 
#' @param tn ending timestep 
#' @export draw_insight_parallel_series
draw_insight_parallel_series<- function(t0, tn, prefix, max_interchange=0)
{
    ts = unique(ergis_generation$time)
    ts = ts[order(ts)]

    i0 = which.min(abs(ts-t0))
    i1 = which.min(abs(ts-tn))
    
    i = i0:i1

    cluster = makeCluster(detectCores()-4, type="FORK")
    parLapply(cluster, i0:i1, function(i) {
        t = ts[i]

        png(sprintf('%s_%s.png', prefix, format(t, "%m-%d-%H-%M")), width=5760, height=2400, pointsize=52)
        par(bg='black', fg='white')
        draw_comparative_insight1(t, max_interchange)
        dev.off()
    })
    stopCluster(cluster)
}
