genind2gtypes after splitting and setting the strata


gen2gtype <- function (x) {
  
  gen.mat <- genind2df(x, usepop = TRUE, oneColPerAll = TRUE)
  strata <- x@strata
  rownames(strata) <- rownames(gen.mat)
  x@strata <- strata
  gen.mat[gen.mat == "NA"] <- NA
  has.pop <- !is.null(x@pop)
  df2gtypes(x = gen.mat, ploidy = x@ploidy[1], id.col = NULL, 
            strata.col = if (has.pop) 
              1
            else NULL, loc.col = if (has.pop) 
              2
            else 1, 
            schemes = x@strata,  other = other(x))

  
}



