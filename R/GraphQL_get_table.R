#' Get Data from GraphQL API (vector‑style)
#'
#' @param tabellenname character string name of queried table
#' @param variablen character vector containing the desired variable names
#' @param datenserver character string URL of the GraphQL endpoint
#' @return tibble with the requested data
#' @export
GraphQL_get_table_vec <- function(
    tabellenname,
    variablen,
    datenserver = "https://data.smclab.io/v1/graphql"
) {
    fields <- paste(variablen, collapse = "\n  ")
    query_text <- sprintf("{ %s {\n  %s\n} }", tabellenname, fields)

    resp <- request(datenserver) |>
        req_method("POST") |>
        req_headers(`Content-Type` = "application/json") |>
        req_body_json(list(query = query_text)) |>
        req_perform()

    tbl <- resp |>
        resp_body_json(simplifyVector = TRUE) |>
        pluck("data", tabellenname) |>
        as_tibble()
}

#' Get Data from GraphQL API (raw query string)
#'
#' @param querystring a complete GraphQL query (including operation name or anonymous)
#' @param datenserver character string URL of the GraphQL endpoint
#' @return tibble with the requested data
#' @export
GraphQL_get_table_string <- function(
    querystring,
    datenserver = "https://data.smclab.io/v1/graphql"
) {
    resp <- request(datenserver) |>
        req_method("POST") |>
        req_headers(`Content-Type` = "application/json") |>
        req_body_json(list(query = querystring)) |>
        req_perform()

    json_resp <- resp |>
        resp_body_json(simplifyVector = TRUE)

    # Check if data exists and is not NULL or empty
    if (is.null(json_resp$data) || length(json_resp$data) == 0) {
        # Return an empty tibble or handle the error as appropriate
        return(tibble())
    }

    tabellenname <- names(json_resp$data)[1]

    # Check if tabellenname exists
    if (length(tabellenname) == 0) {
        return(tibble())
    }

    # Check if the data for the table exists
    table_data <- json_resp$data[[tabellenname]]
    if (is.null(table_data) || length(table_data) == 0) {
        return(tibble())
    }

    tbl <- as_tibble(table_data)

    return(tbl)
}
