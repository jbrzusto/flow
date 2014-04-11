##
##
## animate a chunk of the currently saved raw image data
##
animate_chunk = function(ij) {
  ## ij is a pair (i, j) giving the column and row of the chunk)

  for(i in 1:TimeSteps) {
    par(new = (i > 1)) ## re-use existing plot after first slice

    ## silly image() function needs its data transposed, hence t(...):
    
    image( t(getChunk(G, ij, ChunkDim)[,,i]),  ## show ith timestep of chunk ij
          col=gray.colors(256),
          useRaster=TRUE,
          xaxt="n",  ## no plotting of x axis
          yaxt="n"   ## no plotting of y axis
          )
  }
}
