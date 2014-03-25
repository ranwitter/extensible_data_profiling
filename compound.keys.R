##
# R function to discover compound keys of a given relation w.r.t. a compound key size and a approximation threshold
##
# @param (p) (intermediate data profiling result)
# @param (r) (name of the relation)
# @param (n) (size of the compound key. minimum and default is 2)
# @param (t) (approximation threshold (between 0 and 1). default is 0.)
##
# @return (intermediate data profiling result with compound key discovery results)
##
profiling.discovery.keys.compound <- function(p,r,n=2,t=0) {
  
  # make UKs minimal compound keys
  p$isminCKC[(is.na(p$isminCKC)) & (p$relation == r) & (p$isUK == TRUE)] <- TRUE
  
  
  comcandidates <- p$attribute[(p$isNullable == FALSE) & (p$relation == r) & (p$isUK == FALSE) & (is.na(p$isminCKC))]
  
  if(length(comcandidates) == 2) {
      # calculate compound candidate cardinality
      cardinality <- profiling.discovery.relation.attributes.compound(r, comcandidates)$cardinality  
      # set isminCKC feature
      p$isminCKC[((is.na(p$isminCKC)) & (p$isNullable==FALSE) & (p$attribute %in% comcandidates) & (as.integer(cardinality) >= (1-as.double(t))*as.integer(p$tupleCount)))] <- TRUE
  }
  
  if(length(comcandidates) > 2) {
    # calculate compound candidate cardinality
    combinations <- combn(comcandidates,n)
    noofcomb <- length(combinations[1,])
  
    for (i in 1:noofcomb) {
      print(i)
      cardinality <- profiling.discovery.relation.attributes.compound(r,combinations[,i])$cardinality
      if(cardinality %in% p$tupleCount[(p$relation == r)]) {
        p$isminCKC[((is.na(p$isminCKC)) & (p$isNullable==FALSE) & (p$attribute %in% combinations[,i]) & (as.integer(cardinality) >= (1-as.double(t))*as.integer(p$tupleCount)))] <- TRUE
        break
      }
    }
  }

  as.data.frame(p)
}


##
# R function to discover distinct values of a set of compound attributes
##
# @param (r) (name of the relation)
# @param (as) (a set of attributes within relation)
##
# @return (distincts) (distinctive values of compound attributes)
# @return (cardinality) (cardinality of compound attributes)
##
profiling.discovery.relation.attributes.compound <- function(r, as) {
  
  # comma separeted string of attributes
  attributes_quoted <- paste(as, collapse=", ") 
  distincts <- dbGetQuery(con, paste("SELECT DISTINCT",attributes_quoted,"FROM", as.character(r)))
  # return results as a list
  list(distincts=distincts,cardinality=dim(distincts)[1])
}



