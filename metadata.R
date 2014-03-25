###
# R function to discover meta data  
##
# @param (d) (connection to the database)
# @param (N) (estimated number of total attributes in all relations in the database)
##
# @return (intermediate data profiling result initialized with metadata)
##
profiling.discovery.metadata <- function(d, N=500, verbose=TRUE) {
    ## initialization

    # metadata structure with features
    meta.data <- data.frame(identifier=rep(0, N),
                            relation=rep(NA, N), 
                            attribute=rep(NA,N), 
                            dataType=rep(NA, N), 
                            isNullable=rep(FALSE,N),
                            tupleCount=rep(0, N), 
                            cardinality=rep(0, N), 
                            isUK=rep(NA, N), 
                            conUK=rep(NA, N),
                            isminCKC=rep(NA, N), 
                            isUIND=rep(NA, N),
                            UINDof=rep(NA, N),
                            conUIND=rep(NA, N)
                            )
    # extracting relations
    relations <- dbListTables(d)
    
    i <- 1;
    
    for(r in relations) {
      for(a in profiling.discovery.relation.attributes(r)$collection) {
          tc <- profiling.discovery.relation.tuples(r)$count
          am <- profiling.discovery.relation.attribute(r,a)
          # initializing meta data strucutre data records
          meta.data[i,] <- c(i, r, a, am$datatype, am$isnullable, tc, am$cardinality, NA, NA, NA, NA, NA, NA)
          i <- i + 1
      }
    }
    # returning metadata as result
    return(as.data.frame(meta.data[1:i-1,]))
}


###
# R function to discover attribute dimensions of a given relation
##
# @param (r) (relation name)
##
# @return (count) (number of attributes within relation)
# @return (collection) (collection of attributes within relation)
##
profiling.discovery.relation.attributes <- function(r) {
  # attributes in the relation
  attributes <- dbListFields(con , as.character(r)) 
  #print(class(attributes))
  #attr <- array(fields) # vector to array
  # return of results as a list
  list(count=length(attributes),collection=attributes)
}


###
# R function to discover attribute domain of a given attribute in a given relation 
##
# @param (r) (relation name)
# @param (a) (attribute name)
##
# @return (distincts) (distinctive values of attribute)
# @return (cardinality) (cardinality of attribute)
##
profiling.discovery.relation.attribute <- function(r, a) {
  
  #print(paste("SELECT DISTINCT(", as.character(a), ") FROM ", as.character(r)))
  
  distincts <- dbGetQuery(con, paste("SELECT DISTINCT(", as.character(a), ") FROM ", as.character(r)))
  # extracting SQL datatype 
  sqltype <- dbDataType(con, distincts[1 ,1])
  
  nullability <- dbGetQuery(con, paste("SELECT 1 FROM ", as.character(r), " WHERE ", as.character(a), "IS NULL"))

  ifelse((length(nullability)!=0),nullable<-TRUE,nullable<-FALSE)

  list(distincts=distincts,cardinality=dim(distincts)[1],datatype=sqltype,isnullable=nullable)
}



###
# R function to discover number of tuples in a relation
##
# @param (r) (relation name)
##
# @return (count) (tuple count of relation)
##
profiling.discovery.relation.tuples <- function(r) {
  # counting no of tuples
  count <- dbGetQuery(con, paste("SELECT COUNT(*) FROM ", as.character(r)))
  list(count=count[1,1])
}
