#' Get Data from GraphQL API
#'
#' functions to query data via a GraphQL API. Here `GraphQL_get_table_vec` allows a query specifying the table name and the desired variable names.
#' `GraphQL_get_table_string` allows more complex queries by passing a complete GraphQL query as character-string. 
#' 
#' @param tabellenname character string name of queried table
#' @param variablen character vector containing the desired variable names
#' @param querystring character string contains a valid GraphQL query
#' @param datenserver character string mit URL der Datenbank, default: https://data.smclab.io/v1/graphql
#' @return tibble with the requested data
#' @details
#' The functions are primarily intended to make the [Data Collection](https://github.com/sciencemediacenter/DataCollection) 
#' of the [SMC](https://www.sciencemediacenter.de/) accessible. By adjusting the parameter 'data server',
#' other instances can also be accessed.
#' 
#' 
#' @examples 
#' GraphQL_get_table_vec(
#'   tabellenname = "test_R_Packages_test_story",
#'   variablen = c(
#'     "story_no",
#'     "ressort",
#'     "title",
#'     "publication_date",
#'     "type",
#'     "url"
#'   )
#' )
#' 
#' GraphQL_get_table_string(
#' 'query MyQuery {
#'    test_R_Packages_test_story(
#'      limit: 10, 
#'      where: {story_no: {_gt: 22021}, 
#'      ressort: {_eq: "Medizin & Lebenswissenschaften"}}) {
#'    publication_date
#'    url
#'    type
#'    title
#'    story_no
#'    }
#' }'
#' )
#' @export GraphQL_get_table_vec
GraphQL_get_table_vec <-
    function(tabellenname, variablen, datenserver = "https://data.smclab.io/v1/graphql") {
        conn <- ghql::GraphqlClient$new(url = datenserver)
        query <- ghql::Query$new()
        variablenliste <- paste(variablen, collapse = " \n ")
        queryText <-
            paste0("{", tabellenname, " {\n ", variablenliste, "\n }}")
        query$query("query", queryText)
        tmp <- conn$exec(query$queries$query)
        jsonlite::fromJSON(tmp[[1]][[1]])$data[[1]] %>% dplyr::as_tibble()
    }

#' @examples 
#' \dontrun{
#' ToDo
#' }
#' @export GraphQL_get_table_string
#' @rdname GraphQL_get_table_vec
GraphQL_get_table_string <-
    function(querystring, datenserver = "https://data.smclab.io/v1/graphql") {
        conn <- ghql::GraphqlClient$new(url = datenserver)
        query <- ghql::Query$new()
        query$query("query", querystring)
        tmp <- conn$exec(query$queries$query)
        jsonlite::fromJSON(tmp[[1]][[1]])$data[[1]] %>% dplyr::as_tibble()
    }
