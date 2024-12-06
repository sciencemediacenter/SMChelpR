#######################
## helpers_SMC tests ##
#######################

test_that("get_param behaves as expected", {
  
  params <- list(margin_SMC = 10, font_size = 12)
  
  # Test 1: Parameter exists in the list
  result <- get_param(params, "margin_SMC", 8)
  expect_equal(result, 10)
  
  # Test 2: Parameter does not exist, returns default value
  result <- get_param(params, "non_existing_param", 8)
  expect_equal(result, 8)
  
  # Test 3: Empty list, should return default value
  result <- get_param(list(), "non_existing_param", 8)
  expect_equal(result, 8)
})


test_that("size_in_pt behaves as expected", {
  
  # Test 1: Convert px to pt (default DPI = 96)
  result <- size_in_pt(12, dpi = 96)
  expected_pt <- 12 * 72 / 96
  expect_equal(result, expected_pt)
  
  # Test 2: Convert px to pt with custom DPI (e.g., DPI = 120)
  result <- size_in_pt(12, dpi = 120)
  expected_pt <- 12 * 72 / 120
  expect_equal(result, expected_pt)
  
  # Test 3: Edge case with 0 px (should return 0 pt)
  result <- size_in_pt(0, dpi = 96)
  expect_equal(result, 0)
  
  # Test 4: Handle negative px input (should return a negative pt)
  result <- size_in_pt(-12, dpi = 96)
  expected_pt <- -12 * 72 / 96
  expect_equal(result, expected_pt)
  
  # Test 5: Handle invalid DPI (e.g., DPI = 0) gracefully
  expect_error(size_in_pt(12, dpi = 0), "DPI cannot be zero. Please provide a valid DPI value.")
})