library(mregions2)

#this fails
mrp_col_unique("goas", "name")

#but there is a name column since we can query with it:
mrp_get("goas", cql_filter = "name = 'South Pacific Ocean'")

#other column values for goas can be found:
mrp_col_unique("goas", "latitude")
