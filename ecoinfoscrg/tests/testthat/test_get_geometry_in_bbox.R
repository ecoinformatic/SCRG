test_that("test empty dataset", {
  expect_error(
    ecoinfoscrg::scrg__get_geometry_in_bbox(
      c(), st_sf()
    )
  )
})

test_that("test geospatial object of points, none on bbox border", {
  set_s2(FALSE)
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

test_that("test all forms of nothing returns nothing, complains", {
  for(form_of_nope in c(NaN, NA, NULL)){
    expect_warning(
      nothing <- scrg__get_geometry_in_bbox(
        florida_bbox_vector_lnglat,
        form_of_nope
      ),
      notsf_msg
    )
    expect_equal(nothing, form_of_nope)
  }
})

test_that("test string returns string, complains", {
  characters <- "test"
  expect_warning(
    get_geometry_result <- scrg__get_geometry_in_bbox(
      florida_bbox_vector_lnglat,
      characters
    ),
    notsf_msg
  )
  expect_equal(characters, get_geometry_result)
})

test_that("test corner point should be returned in sf object", {
  set_s2(FALSE)
  fl_set <- scrg__get_geometry_in_bbox(
    florida_bbox_vector_lnglat, point_on_corner
  )
  expect_warning(
    expect_setequal(fl_set, point_on_corner),
    "ignores names"
  )
})

test_that("test side point should be returned in sf object", {
  set_s2(FALSE)
  fl_set <- scrg__get_geometry_in_bbox(
    florida_bbox_vector_lnglat, point_on_side
  )
  expect_warning(
    expect_setequal(fl_set, point_on_side),
    "ignores names"
  )
})

test_that("test inside point should be returned in sf object", {
  set_s2(FALSE)
  fl_set <- scrg__get_geometry_in_bbox(
    florida_bbox_vector_lnglat, point_inside
  )
  expect_warning(
    expect_setequal(fl_set, point_inside),
    "ignores names"
  )
})
