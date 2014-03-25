##
# R function for discovery of key based UINDs
# 
##
# @param (p) (intermediate data profiling result)
##
# @return (intermediate data profiling result with UIND discovery results)
##
profiling.discovery.uinds <- function(p) {
  
   # set isUIND feature to FALSE
   p$isUIND[is.na(p$isUIND)]  <- FALSE
  
   # extract right hand side attributes
   R <- p[p$isUK==TRUE,]
   # extract left hand side attributes
   L <- p[p$isUK==FALSE,]
     
   # iterate through all right hand candidates 
   for (ri in 1:nrow(R))  {
     for (li in 1:nrow(L))  {
      #print(paste(paste(R[ri,"relation"], 
      #                  R[ri,"attribute"], 
      #                  R[ri,"dataType"], sep="."),
      #            paste(L[li,"relation"],
      #                  L[li,"attribute"],
      #                  L[li,"dataType"], sep="."),
      #            sep=">")
      #      )
    #  
      #print(paste(paste(R[ri,"relation"],
      #                  R[ri,"attribute"],
      #                  as.integer(R[ri,"cardinality"]),
      #                  sep="."),
      #            paste(L[li,"relation"],
      #                  L[li,"attribute"],
      #                  as.integer(L[li,"cardinality"]),
      #                  sep="."), 
      #            sep=">=")
      #      )
      # 
      if((R[ri,"dataType"]==L[li,"dataType"]) & (as.integer(R[ri,"cardinality"])>=as.integer(L[li,"cardinality"]))) {
        #print(paste(paste(L[li,"relation"], L[li,"attribute"], sep="."),paste(R[ri,"relation"], R[ri,"attribute"], sep="."), sep="->"))
        inclusion <- profiling.discovery.attributes.inclusion(L[li,"relation"],L[li,"attribute"],R[ri,"relation"],R[ri,"attribute"])
        if(inclusion$confidence != 0) {
          # set isUIND feature to TRUE
          p$isUIND[p$identifier==L[li,"identifier"]]  <- TRUE
          # set UINDof feature to be the right hand key attribute
          p$UINDof[p$identifier==L[li,"identifier"]]  <- inclusion$uindof
          # set inclusion confidence. i.e. fraction of inclusions
          p$conUIND[p$identifier==L[li,"identifier"]]  <- format(round(inclusion$confidence, 2), nsmall = 2)
        }
        else {
          # set isUIND feature to FALSE
          p$isUIND[p$identifier==L[li,"identifier"]]  <- FALSE
        }
      }
      else {
        print("FALSE")
      }
     }
   }
  # return results 
  return(as.data.frame(p))
}

##
# R function for calculating fraction of inclusions between two attributes.
# @assumption (right hand side attribute is a unary key)
##
# @param (rl) (left hand side relation)
# @param (al) (left hand side attribute)
# @param (rr) (right hand side relation)
# @param (ar) (right hand side attribute - an unary key)
##
# @return (confidence) (confidence of inclusion result representing fraction of inclusions)
# @return (uindof) (if UIND exists then fully qualified name of its key attribute otherwise <NA>)
##
profiling.discovery.attributes.inclusion <- function(rl,al,rr,ar) {
  
  al.values <- NA
  
  ar.values <- NA
  # left hand side attribute values
  al.values <- profiling.discovery.relation.attribute(rl,al)$distincts
  # right hand side attribute values
  ar.values <- profiling.discovery.relation.attribute(rr,ar)$distincts

  # left hand side attribute values count
  al.values.count <- nrow(al.values)
  # right hand side attribute value count
  ar.values.count <- nrow(ar.values)

  # left hand side attribute value inclusions within right hand side
  al.inclusions <- 0 
  
  for(i in 1:al.values.count) {
    # check for inclusions 
    if(al.values[i,al] %in% ar.values[,ar]) {
      # increase inclusions
      al.inclusions <- al.inclusions + 1
    }
  }  
  
  # fraction of inclusions
  confidence <- as.double(al.inclusions / ar.values.count)
  
  # fully qualified right hand side attribute name 
  uindof <- paste(rr,ar,sep=".")
  # return results
  list(confidence=confidence, uindof=uindof)
}

#profiling.discovery.attributes.uind("tourists","quarter","visits","quarter")
#profiling.discovery.attributes.uind("visits","park","parks","park")
