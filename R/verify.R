# Code to examime PV generation across multiple ISOs with minimal data
# manipulation
#

#----------------------------------------------------------------------------
#' Plot a single row from the generation table
#'
#' @description Plot a single row from the generation table
#'
#' @param name the generator's name
plot_line <- function(name, generation, ...)
{
    lines(as.double(generation[generation$name == name,]), ...) # plot all data for give generator name
}

#----------------------------------------------------------------------------
#' Plot the PV generations for all generators in an ISO
#'
#' @description Plot the PV generations for all generators in an ISO
#'
#' @param iso the ISO's name
plot_iso <- function(iso, generators, ...)
{
    gens = as.character(generators$Generator_Name[generators$Node_Region==iso & generators$Type=='PV']) # get PV generators for given ISO
    gens = gens[gens %in% generation$name] # limit to available generators

    plot(NA, ylim=c(0,1860), xlim=c(0,dim(generation)[2]), ylab='', xlab='', xaxt='n') # set plot limits
    lapply(gens, plot_line, ...) # draw each generator

    abline(v=seq(from=92, to=dim(generation)[2], by=288), lty=3, lwd=2)  # draw some vertical lines for reference

    # Add labels and axis
    text(dim(generation)[2]/2, 1700, iso, font=2)
    mtext('PV Generation', side=2, line=3, cex=0.75)

    if (par()$mfg[1] == par()$mfg[3]) axis(1) # draw axis for last plot in layout
}

#
# Plot the ISOs on one page
#
#ergis_generators = read.csv('~/ergis/data/genNodes_full.csv', sep=',')
#ergis_generation = read.csv('~/ergis/data/generation_R30P.csv', header=T)
#par(mar=c(0.2,1,0.2,0.2), oma=c(5,5,5,5))
#layout(matrix(c(1,2,3,4,5), 5, 1))
#plot_iso('FRCC', col='#0000001F')
#plot_iso('PJM', col='#0000001F')
#plot_iso('SERC', col='#0000001F')
#plot_iso('SPP', col='#0000001F')
#plot_iso('MISO', col='#0000001F')
