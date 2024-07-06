florida_bbox_vector_lnglat <- c(
  c(-88.088379, 23.301901),
  c(-78.859863, 31.128199)
)

points_in_florida <- st_sf(geometry = st_sfc(
  st_point(c(-82.737061, 27.615710)), # Off the coast of Fort De Soto
  st_point(c(-86.489750, 30.411087)), # In Joe's Bayou - Choctawhatchee Bay
  st_point(c(-81.406299, 30.417531)), # Coast of Huguenot Park, Jacksonville
  st_point(c(-80.156357, 25.666343)), # Key of Biscayne, Miami
  st_point(c(-84.179639, 30.073820)) # Lighthouse, Area X
))

points_not_in_florida <- st_sf(geometry = st_sfc(
  st_point(c(-90.084114, 30.026300)), # New Orleans, LA, USA
  st_point(c(-117.124386, 32.536249)), # San Diego, CA, USA
  st_point(c(4.903421, 52.379529)) # Amsterdam, NH, NL
))

point_geospatical_obj <- rbind(points_in_florida, points_not_in_florida)

notsf_msg <- "Object passed to `scrg__get_geometry_in_bbox` is not an sf object"

# Used in "corner_point", "side_point", and "inside point tests"
point_on_corner<- st_sf(geometry = st_sfc(
  st_point(c(-88.088379, 23.301901))
))

point_on_side<- st_sf(geometry = st_sfc(
  st_point(c(-83.088379, 23.301901))
))

point_inside <- st_sf(geometry = st_sfc(
  st_point(c(-85.088379, 25.301901))
))

set_s2 <- function(env = parent.frame(), s2=FALSE) {
  withr::with_options(list(sf_use_s2=s2), getOption("sf_use_s2"))
}
