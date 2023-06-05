###########################
## GraphQL_get_table_vec ##
###########################

n <- 85
p <- 6

story_testdaten_vec <-
  GraphQL_get_table_vec(
    tabellenname = "test_R_Packages_test_story",
    variablen = c(
      "story_no",
      "ressort",
      "title",
      "publication_date",
      "type",
      "url"
    ),
    datenserver = "https://data.smclab.io/v1/graphql"
  )




test_that("GraphQL_get_table_vec Data", {
  expect_equal(dim(story_testdaten_vec), c(n, p))
  expect_equal(length(unique(story_testdaten_vec$title)), n)
  expect_equal(length(unique(story_testdaten_vec$title)), n)
  expect_equal(class(story_testdaten_vec),
               c("tbl_df", "tbl", "data.frame"))
})

test_that("GraphQL_get_table_vec Encoding", {
  expect_equal(story_testdaten_vec$ressort[1], "Energie & Mobilität")
  expect_equal(as.Date(story_testdaten_vec$publication_date[1]),
               as.Date("2022-03-17"))
})

test_that("GraphQL_get_table_vec Missing values", {
  expect_true(story_testdaten_vec$url[6] == "")
  expect_true(is.na(story_testdaten_vec$publication_date[85]))
  expect_true(is.na(story_testdaten_vec$url[85]))
})

test_that("GraphQL_get_table_vec Error handling", {
  expect_error(
    GraphQL_get_table_vec(
      tabellenname = "test_R_Packages_test_story",
      variablen = c("story_no"),
      datenserver = "https://data.smclab.io/v1/graphpl"
    )
  )
  
  expect_true(is.data.frame(
    GraphQL_get_table_vec(
      tabellenname = "test_R_Packages_test_story",
      variablen = c("story_no", "story_no2"),
      datenserver = "https://data.smclab.io/v1/graphql"
    )
  ))
  
  expect_equal(dim(
    GraphQL_get_table_vec(
      tabellenname = "test_R_Packages_test_story",
      variablen = c("story_no", "story_no2"),
      datenserver = "https://data.smclab.io/v1/graphql"
    )
  ), c(0, 0))
  
  expect_equal(dim(
    GraphQL_get_table_vec(
      tabellenname = "test_R_Packages_test_story2",
      variablen = c("story_no"),
      datenserver = "https://data.smclab.io/v1/graphql"
    )
  ), c(0, 0))
  
})

test_that("GraphQL_get_table_vec default datenserver", {
  expect_equal(
    GraphQL_get_table_vec(
      tabellenname = "test_R_Packages_test_story",
      variablen = c("story_no")
    )$story_no[1],
22042
  )
  
})


##############################
## GraphQL_get_table_string ##
##############################

story_testdaten_string <- 
  GraphQL_get_table_string('query MyQuery {
  test_R_Packages_test_story {
    story_no
    ressort
    title
    publication_date
    type
    url
  }
}',
datenserver = "https://data.smclab.io/v1/graphql")

test_that("GraphQL_get_table_string Data", {
  expect_equal(dim(story_testdaten_string), c(n, p))
  expect_equal(length(unique(story_testdaten_string$title)), n)
  expect_equal(length(unique(story_testdaten_string$title)), n)
  expect_equal(class(story_testdaten_string),
               c("tbl_df", "tbl", "data.frame"))
})

test_that("GraphQL_get_table_string Encoding", {
  expect_equal(story_testdaten_string$ressort[1], "Energie & Mobilität")
  expect_equal(as.Date(story_testdaten_string$publication_date[1]),
               as.Date("2022-03-17"))
})

test_that("GraphQL_get_table_string Missing values", {
  expect_true(story_testdaten_string$url[6] == "")
  expect_true(is.na(story_testdaten_string$publication_date[85]))
  expect_true(is.na(story_testdaten_string$url[85]))
})

test_that("GraphQL_get_table_string Error handling", {
  expect_error(
    GraphQL_get_table_string(
      'query MyQuery {
  test_R_Packages_test_story {
    story_no
    ressort
    title
    publication_date
    type
    url
  }
}',
datenserver = "https://data.smclab.io/v1/graphpl"
    )
  )
  
  expect_true(is.data.frame(
    GraphQL_get_table_string(
      'query MyQuery {
  test_R_Packages_test_story {
    story_no
    story_no2
  }
}',
datenserver = "https://data.smclab.io/v1/graphql"
    )
  ))
  
  expect_equal(dim(
    GraphQL_get_table_string(
      'query MyQuery {
  test_R_Packages_test_story {
    story_no
    story_no2
  }
}',
datenserver = "https://data.smclab.io/v1/graphql"
    )
  ), c(0, 0))
  
  expect_equal(dim(
    GraphQL_get_table_string(
      'query MyQuery {
  test_R_Packages_test_story2 {
    story_no
  }
}',
datenserver = "https://data.smclab.io/v1/graphql"
    )
  ), c(0, 0))
})

test_that("GraphQL_get_table_string default datenserver", {
  expect_equal(
    GraphQL_get_table_string(
      'query MyQuery {
  test_R_Packages_test_story {
    story_no
  }
}'
    )$story_no[1],
22042
  )
  
})


test_that("GraphQL_get_table_string complex querys", {
  expect_equal(
    GraphQL_get_table_string(
      'query MyQuery {
  test_R_Packages_test_story(limit: 1, where: {story_no: {_gt: 22021}, ressort: {_eq: "Medizin & Lebenswissenschaften"}}) {
    publication_date
    url
    type
    title
    story_no
  }
}'
    )$title[1],
"Testdaten Mögliches Risiko für Missbildungen durch Diabetestherapie bei Vater"
  )
})
