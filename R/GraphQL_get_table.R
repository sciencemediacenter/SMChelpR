#' GraphQL_get_table
#'
#' Funktionen, um Datensätze über die GraphQL-API abzufragen. Dabei ermöglicht `GraphQL_get_table_vec` eine Datensatzabfrage unter Angabe des Tabellennamens und der benötigten Variablennamen.
#' `GraphQL_get_table_string` erlaubt komplexere Abfragen, indem eine vollständige GraphQL-Abfrage als character-string übergeben werden kann. 
#' 
#' @param tabellenname character string
#' @param variablen Variablenvektor, der die Namen der abzufragenden Tabellenvariablen enthält
#' @param querystring character string enthält eine valide GraphQL-Abfrage
#' @param datenserver character string mit URL der Datenbank, Defaultwert: https://data.smclab.io/v1/graphql
#' @return tibble mit den angefragten Daten
#' @examples 
#' \dontrun{
#' variablen <- c()
#' GraphQL_get_table_vec("",  variablen, "https://data.smclab.io/v1/graphql")
#' }
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
