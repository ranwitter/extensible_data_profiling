source('~/R/dbconnect.r', echo=TRUE)
source('~/R/metadata.R', echo=TRUE)
source('~/R/unary.keys.R', echo=TRUE)
source('~/R/compound.keys.R', echo=TRUE)
source('~/R/attribute.uind.r', echo=TRUE)

profile1 <- profiling.discovery.metadata(con, N=100)
profile1
profile2 <- profiling.discovery.keys.unary(profile1, t=0)
profile2

profile3 <- profiling.discovery.keys.compound(profile2,"visits")
profile3 <- profiling.discovery.keys.compound(profile3,"toursits", n=5)
profile3 <- profiling.discovery.keys.compound(profile3,"parks")
profile3
profile4 <- profiling.discovery.uinds(profile3)
profile4

#pmml.treemodel(p4)
