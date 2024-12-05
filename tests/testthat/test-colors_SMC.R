test_that("colors_SMC_named returns a named list", {
  
  # Test 1: Ensure colors_SMC_named is a list
  expect_type(colors_SMC_named, "list")
  
  # Test 2: Ensure the correct names are present in the list
  expected_names <- c("blue", "green", "purple", "orange", "brown", 
                      "pink", "grey", "red", "yellow")
  expect_setequal(names(colors_SMC_named), expected_names)
  
  # Test 3: Check that all values are valid hex color codes
  is_valid_hex <- function(color) grepl("^#[0-9A-Fa-f]{6}$", color)
  expect_true(all(sapply(colors_SMC_named, is_valid_hex)))
  
  # Test 4: Verify the number of colors matches the expected count
  expect_length(colors_SMC_named, length(expected_names))
  
})

test_that("colors_SMC_unnamed returns a character vector with hexcodes", {
  
  # Test 1: Ensure colors_SMC_unnamed is a character vector
  expect_type(colors_SMC_unnamed, "character")
  
  # Test 2: Check that all values are valid hex color codes
  is_valid_hex <- function(color) grepl("^#[0-9A-Fa-f]{6}$", color)
  expect_true(all(sapply(colors_SMC_unnamed, is_valid_hex)))
  
  # Test 3: Verify the number of colors matches the expected count
  expect_length(colors_SMC_unnamed, 9)
  
})

test_that("colors_SMC behaves as expected", {
  
  # Test 1: Default behavior returns all colors in unnamed order
  expect_equal(colors_SMC(), colors_SMC_unnamed)
  
  # Test 2: Subset selection by ColorNames
  expect_equal(colors_SMC(c("blue", "green")), c("#377eb8", "#4daf4a"))
  
  # Test 3: Single color selection returns a single string
  expect_equal(colors_SMC("blue"), "#377eb8")
  
  # Test 4: Reverse order
  expect_equal(colors_SMC(rev = TRUE), rev(colors_SMC_unnamed))
  
  # Test 5: Reverse a subset
  expect_equal(colors_SMC(c("blue", "green"), rev = TRUE), c("#4daf4a", "#377eb8"))
  
  # Test 6: Invalid ColorNames return NA
  result <- colors_SMC("invalid_color")
  expect_true(is.na(result))

  result <- colors_SMC(c("blue", "invalid_color"))
  expect_true(!is.na(result[[1]]))
  expect_true(is.na(result[[2]]))
  
})