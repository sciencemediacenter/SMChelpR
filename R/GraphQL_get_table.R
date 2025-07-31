#' Build a GraphQL query string
#'
#' Creates a properly formatted GraphQL query string to retrieve data from a GraphQL endpoint.
#' The function supports filtering, pagination, and field selection.
#'
#' @param tabellenname character string name of queried table
#' @param variablen character vector containing the desired variable names (fields to retrieve)
#' @param limit integer; optional maximum number of records to return
#' @param offset integer; optional number of records to skip (for pagination)
#' @param where character string; optional GraphQL-formatted condition for filtering results
#'
#' @return character string containing a formatted GraphQL query
#' @export
#'
#' @examples
#' build_graphql_query(
#'   tabellenname = "test_R_Packages_test_story",
#'   variablen = c("publication_date", "story_no"),
#'   limit = 10,
#'   offset = 20
#' )
#'
#' # With filtering
#' build_graphql_query(
#'    tabellenname = "test_R_Packages_test_story",
#'    variablen = c(
#'        "publication_date",
#'        "ressort",
#'        "story_no",
#'        "title",
#'        "type",
#'        "url"
#'    ),
#'    where = 'publication_date: {_lt: "2022-01-12"}'
#')
build_graphql_query <- function(
    tabellenname,
    variablen,
    limit = NULL,
    offset = NULL,
    where = NULL
) {
    # Format the fields list
    fields <- paste(variablen, collapse = "\n    ")

    # Assemble argument pieces
    args <- character()
    if (!is.null(where)) {
        args <- c(args, paste0("where: {", where, "}"))
    }
    if (!is.null(limit)) {
        args <- c(args, sprintf("limit: %d", limit))
    }
    if (!is.null(offset)) {
        args <- c(args, sprintf("offset: %d", offset))
    }

    # Glue into ( ... ) or omit if empty
    arg_str <- if (length(args)) {
        paste0("(", paste(args, collapse = ", "), ")")
    } else {
        ""
    }

    # Construct final query
    sprintf("{ %s%s {\n    %s\n  } }", tabellenname, arg_str, fields)
}

#' Handle GraphQL error responses
#'
#' Processes GraphQL error responses and generates a formatted error message before stopping execution.
#' This function formats each error object as pretty-printed JSON and combines them into a
#' comprehensive error message.
#'
#' @param json_errors list; error objects returned from a GraphQL query response
#'
#' @return Never returns; calls `stop()` with formatted error message
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Error handling in a GraphQL request:
#' if (!is.null(json$errors) && length(json$errors) > 0) {
#'   stop_graphql_errors(json$errors)
#' }
#' }
stop_graphql_errors <- function(json_errors) {
    # json_errors is typically a list of 1+ named lists / data.frames
    msgs <- lapply(json_errors, function(err) {
        # pretty-print each error object as JSON
        toJSON(err, auto_unbox = TRUE, pretty = TRUE)
    })
    stop(
        "GraphQL returned error(s):\n\n",
        paste(msgs, collapse = "\n\n"),
        call. = FALSE
    )
}


#' Retrieve data from a GraphQL API with pagination (vector-style)
#'
#' Fetches data from a GraphQL endpoint using automatic pagination to handle large result sets.
#' The function constructs GraphQL queries, handles pagination, and combines results into a single tibble.
#' Includes automatic retry logic to handle temporary network issues such as HTTP 502 errors.
#'
#' @param tabellenname character string; name of the table/entity to query in the GraphQL schema
#' @param variablen character vector; field names to retrieve from the GraphQL endpoint
#' @param where character string; optional GraphQL-formatted condition string for filtering results
#' @param datenserver character string; URL of the GraphQL endpoint (defaults to "https://data.smclab.io/v1/graphql")
#' @param page_size integer; number of records to fetch per request (defaults to 1000)
#' @param max_pages integer; maximum number of pages to retrieve (defaults to Inf for all available pages)
#' @param max_tries integer; maximum number of retry attempts for failed requests (defaults to 3)
#' @param backoff function; determines wait time between retries in milliseconds, default is exponential backoff
#'        starting at 100ms (function(i) 2^i * 100)
#' @param ssl_options list; optional SSL configuration parameters passed to req_options().
#'        Examples: list(ssl_verifypeer = 0, ssl_verifyhost = 0) or list(cainfo = "stern.int.crt")
#'
#' @return tibble containing all retrieved data with columns matching the requested fields
#' @export
#'
#' @examples
#' # Basic query for story data with default pagination
#' GraphQL_get_table_vec(
#'   tabellenname = "test_R_Packages_test_story",
#'   variablen = c("publication_date", "story_no", "title")
#' )
#'
#' # Query with filtering, custom page size and increased retry attempts
#' GraphQL_get_table_vec(
#'   tabellenname = "test_R_Packages_test_story",
#'   variablen = c("publication_date", "ressort", "story_no", "title"),
#'   where = 'publication_date: {_gt: "2022-01-01"}',
#'   page_size = 500,
#'   max_pages = 10,
#'   max_tries = 5
#' )
#'
#' # With SSL certificate verification disabled
#' GraphQL_get_table_vec(
#'   tabellenname = "test_R_Packages_test_story",
#'   variablen = c("publication_date", "story_no", "title"),
#'   ssl_options = list(ssl_verifypeer = 0, ssl_verifyhost = 0)
#' )
GraphQL_get_table_vec <- function(
    tabellenname,
    variablen,
    where = NULL,
    datenserver = "https://data.smclab.io/v1/graphql",
    page_size = 1000,
    max_pages = Inf,
    max_tries = 3,
    backoff = function(i) 2^i * 100,
    ssl_options = NULL
) {
    # Initialize result tibble and pagination variables
    all_results <- tibble()
    current_offset <- 0
    page_count <- 0
    has_more_data <- TRUE

    # Loop until no more data or max pages reached
    while (has_more_data && page_count < max_pages) {
        # Create query with pagination using the helper function
        query_text <- build_graphql_query(
            tabellenname = tabellenname,
            variablen = variablen,
            limit = page_size,
            offset = current_offset,
            where = where
        )

        req <- request(datenserver) |>
            req_progress() |>
            req_method("POST") |>
            req_headers(`Content-Type` = "application/json") |>
            req_body_json(list(query = query_text))

        # Apply SSL options if provided
        if (!is.null(ssl_options)) {
            req <- do.call(req_options, c(list(req), ssl_options))
        }

        resp <- req |>
            req_retry(max_tries = max_tries, backoff = backoff) |>
            req_perform()

        # Catch errors in the JSON
        json <- tryCatch(
            resp |> resp_body_json(simplifyVector = TRUE),
            error = function(e) stop("Failed to parse JSON: ", e$message)
        )

        # Check for GraphQL errors
        if (!is.null(json$errors) && length(json$errors) > 0) {
            stop_graphql_errors(json$errors)
        }

        # Extract data for this page
        page_data <- json |>
            pluck("data", tabellenname) |>
            as_tibble()

        # Check if we got any results
        if (nrow(page_data) > 0) {
            all_results <- bind_rows(all_results, page_data)
            current_offset <- current_offset + page_size
            page_count <- page_count + 1

            # Print progress
            message(glue(
                "Page {page_count}: Retrieved {nrow(page_data)} rows. Total so far: {nrow(all_results)}"
            ))
        } else {
            has_more_data <- FALSE
        }
    }

    return(all_results)
}

#' Get Data from GraphQL API (raw query string)
#'
#' Executes a raw GraphQL query string against a GraphQL endpoint and returns the results.
#' Includes automatic retry logic to handle temporary network issues such as HTTP 502 errors.
#'
#' @param querystring a complete GraphQL query (including operation name or anonymous)
#' @param datenserver character string URL of the GraphQL endpoint
#' @param max_tries integer; maximum number of retry attempts for failed requests (defaults to 3)
#' @param backoff function; determines wait time between retries in milliseconds, default is exponential backoff
#'        starting at 100ms (function(i) 2^i * 100)
#' @param ssl_options list; optional SSL configuration parameters passed to req_options().
#'        Examples: list(ssl_verifypeer = 0, ssl_verifyhost = 0) or list(cainfo = "stern.int.crt")
#' @return tibble with the requested data
#' @export
GraphQL_get_table_string <- function(
    querystring,
    datenserver = "https://data.smclab.io/v1/graphql",
    max_tries = 3,
    backoff = function(i) 2^i * 100,
    ssl_options = NULL
) {
    req <- request(datenserver) |>
        req_method("POST") |>
        req_headers(`Content-Type` = "application/json") |>
        req_body_json(list(query = querystring))

    # Apply SSL options if provided
    if (!is.null(ssl_options)) {
        req <- do.call(req_options, c(list(req), ssl_options))
    }

    resp <- req |>
        req_retry(max_tries = max_tries, backoff = backoff) |>
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


#' Convert a GraphQL geometry column to sf
#'
#' Takes a tibble with a geometry column from GraphQL and converts it to a simple features (sf) object.
#' The function handles the JSON structure that GraphQL returns for geometries.
#'
#' @param tbl A tibble containing a geometry column in GraphQL format
#' @param geom_col The name of the geometry column (defaults to "geometry")
#' @param crs_default The default CRS to use if none is found in the data (defaults to 4326)
#'
#' @return A tibble converted to an sf object with proper geometry
#' @export
#'
#' @examples
#' \dontrun{
#' # Pipe-friendly usage
#' polygons_sf <- GraphQL_get_table_vec(
#'   tabellenname = "spatial_data",
#'   variablen = c("id", "name", "geometry")
#' ) |>
#'   convert_geometry_column()
#' }
convert_geometry_column <- function(
    tbl,
    geom_col = "geometry",
    crs_default = 4326
) {
    # Function to convert a single geometry row to sf geometry
    convert_geom <- function(geom_row) {
        # Create a proper GeoJSON structure
        geojson_str <- jsonlite::toJSON(
            list(
                type = geom_row$type,
                crs = geom_row$crs,
                coordinates = geom_row$coordinates[[1]]
            ),
            auto_unbox = TRUE
        )

        # Convert to sf geometry
        geojsonsf::geojson_sfc(geojson_str)
    }

    # Apply conversion to each row
    sf_data <- tbl |>
        rowwise() |>
        mutate(
            geom_obj = list(convert_geom(.data[[geom_col]])),
            crs_value = .data[[geom_col]]$crs$properties$name %||% crs_default # Extract CRS or use default
        ) |>
        ungroup()

    # Extract the CRS value before calling st_as_sf
    crs_val <- sf_data$crs_value[1]

    # Complete the transformation
    sf_data <- sf_data |>
        mutate(!!sym(geom_col) := do.call(c, get("geom_obj"))) |>
        select(-"geom_obj", -"crs_value") |>
        sf::st_as_sf(crs = crs_val)

    return(sf_data)
}
