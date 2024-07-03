test_that("empty dataset", {
  expect_error(
    ecoinfoscrg::scrg__get_geometry_in_bbox(
      c(), st_sf()
    )
  )
})

test_that("long lat", {
  sf_use_s2(FALSE)
  fl_set <- scrg__get_geometry_in_bbox(
    florida_bbox_vector_lnglat, point_geospatical_obj
  )
  # Yeah, we know...and also don't care. Names are not part of the functionality
  # tested.
  expect_warning(
    expect_setequal(fl_set, points_in_florida),
    "ignores names"
  )
})

test_that("test NA", {
  expect_equal(
    scrg__get_geometry_in_bbox(florida_bbox_vector_lnglat, NA), NA
  )
})

test_that("is character", {
  expect_success(is.character(scrg__get_geometry_in_bbox("test"))
  )
})

test_that("vectors", {
  expect_success(
    scrg__get_geometry_in_bbox(
    )
  )
})

test_that("vector test", {
  points <- list(c(81.3, 81.4), c(81.5, 81.6))
  sf_object <- st_sf(geometry = st_sfc(
    st_point(c(81.35, 81.45)),
    st_point(c(81.55, 81.65))
    ))

  result <- scrg__get_geometry_in_bbox(points, sf_object)

  expected_result <- st_sf(geometry = st_sfc(st_point(c(81.35, 81.45))))

  expect_equal(result, expected_result)
})

