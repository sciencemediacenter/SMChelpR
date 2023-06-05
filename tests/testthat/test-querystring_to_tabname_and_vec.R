test_that("querystring_to_tabname_and_vec", {
  expect_equal(
    querystring_to_tabname_and_vec(
      "query MyQuery {
  test_R_Packages_test_story {
    publication_date
    url
    type
    title
    story_no
  }
}
"),
list(
  tabellenname = "test_R_Packages_test_story",
  variablen = c("publication_date", "url", "type", "title", "story_no")
)
  )
})
