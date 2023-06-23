#mrp_get function won't work without loading the mregions2 library
mregions2::mrp_get("eez", cql_filter = "territory1 = 'Azores'")

library(mregions2)
azores <- mrp_get("eez", cql_filter = "territory1 = 'Fiji'")


plot(azores[1])

#plot seems to be rotated 90 degrees clockwise??

azores

sessionInfo()
