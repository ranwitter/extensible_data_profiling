##
# R function to discover keys in a given relation w.r.t an given approximate threshold
##
# @param (p) (intermediate data profiling result)
# @param (t) (approximation threshold (between 0 and 1). default is 0.)
##
# @return (intermediate data profiling result with unary key discovery results)
##
profiling.discovery.keys.unary <- function(p, t=0) {
  # initialization of candidates
  
  # compound predicate and set isUK feature value
  p$isUK[((p$isNullable==FALSE) & (as.integer(p$cardinality) >= round((1-as.double(t))*as.integer(p$tupleCount))))] <- TRUE
  # negate compound predicate and set isUK feature value
  p$isUK[!((p$isNullable==FALSE) & (as.integer(p$cardinality) >= round((1-as.double(t))*as.integer(p$tupleCount))))] <- FALSE
  

  # general confidence for nullable attributes
  p$conUK[(p$isNullable==TRUE) & (p$isUK==FALSE)] <- format(round((1-t), 2), nsmall = 2)
  # more specific confidence on true unary keys
  p$conUK[(p$isUK==TRUE)] <- format(round((1-t), 2), nsmall = 2) 
  
  #confidence calculation ends here
  
  return(as.data.frame(p))
}