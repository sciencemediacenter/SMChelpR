#' Create Vectors with table- and variable-names from a GraphQL query 
#' 
#' Helper function to extraxt tablename and vector of variable names from an simple GraphQL query
#' 
#' @param querystring character string containing a valid simple GraphQL-query. Must not contain additional parameter or nested structures.
#' @return invisible returns a list with tabname and variables. Cats string into console for esy copy and pasting.
#' @examples
#' querystring_to_tabname_and_vec(
#' "query MyQuery {
#'   smc_story_meta {
#'   story_no
#'   title
#'   type
#'   url
#'   ressort
#'   publication_date
#'   }
#' }")
#' @export querystring_to_tabname_and_vec
#' @rdname GraphQL_get_table_vec
querystring_to_tabname_and_vec <-
    function(querystring) {
        querystring <- gsub("query MyQuery", "", querystring)
        tabname <- stringr::str_extract(querystring, "[a-zA-Z-_]+")
        querystring <- gsub(tabname, "", querystring)
        variables <-
          stringr::str_extract_all(querystring, "[a-zA-Z-_]+")[[1]]
        cat(paste0('variablen = c("', paste(variables, collapse = '", "'), '"), \n'))
        cat(paste0('tabellenname = "', tabname, '"\n\n'))
        invisible(list(tabellenname = tabname, variablen = variables))
    }

