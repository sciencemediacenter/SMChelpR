#' image_helper.R
#'
#' Function to create generic image captions in dynamic documents.
#' For this purpose, HTML code is created, which contains a link to the data needed to create this figure. 
#' Also includes links to download the figure in different formats.
#' 
#' The functions image_helper_light(), image_helper() return a list containing the file path, the title including links, 
#' an "alt" text (consisting of the image name) and possibly other HTML tags.
#' 
#' @param plot ggplot-object: The ggplot-object to be used for static images.
#' @param filename string: The name of the image file, without file extension.
#' @param filepath file.path: The path to the folder in which the image file should be saved. (For image_helper(), CSV must be placed manually, PNG and SVG are stored automatically).
#' @param extra_html_tags string: Between \'\' further HTML-Params can be added, e.g. \'width = \"100%\" data-zoom=\"1\"\'
#' @param csv_opt boolean, FALSE: no link to a CSV-Download will be provided; default: TRUE.
#' @param plotly boolean: FALSE, a PNG of the provided ggplot-object will be displayed. TRUE: no figure is shown, plotly-figures must be displayed manually bforehand. Default: FALSE
#' @param caption string: If not empty: text for the caption, e.g. sources.
#' @param save_svg boolean: Generation of the corresponding svg. Default: TRUE. For very detailed graphics, you should refrain from creating a sv-graphic, since the file size can become too large.
#' @param captions vector: A vector with all the captions for the plot.
#' @param data tibble: A dataframe with two columns 1) fileformat, and 2) filename_suffix. This dataframe is used to generate a link to each data set needed to generate the plot, usually there is one data set per plot, but sometimes there are more.
#' @param images tibble: A dataframe with two columns 1) fileformat, and 2) filename_suffix. This dataframe is used to generate links to files storing the plot in different image formats.
#' @param static_image boolean, FALSE: display static image above captions
#' @param static_image_parameters string: params to pass to img tag displaying static image
#' @return Invisible: HTML code for image caption
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' tmp_ggplot <- 
#'   ggplot(data = mpg, aes(x = displ, y = hwy)) +
#'   geom_point()
#' tmp_ggplot %>% image_helper("MPG", file.path(tempdir())) 
#' filename <- "a"
#' filepath <- tempdir()
#' data <- tibble(fileformat = c("csv", "json"), filename_suffix = c("", "extra_json_suffix"))
#' images <- tibble(fileformat = c("html"), filename_suffix = c(""))
#' captions = c("my_caption_1", "my_caption_2")
#' write(0, file = file.path(tempdir(), "a.csv"))
#' write(0, file = file.path(tempdir(), "a_extra_json_suffix.json"))
#' write(0, file = file.path(tempdir(), "a.html"))
#' image_helper_light(captions, filename, filepath, data, images)

#' @export image_helper
image_helper <-
  function(plot,
           filename,
           filepath,
           extra_html_tags = 'width = "100%" data-zoom="1"',
           csv_opt = TRUE,
           plotly = FALSE,
           caption = "",
           save_svg = TRUE
          ) {
    pngpfad <- file.path(filepath, paste0(filename, ".png"))
    svgpfad <- file.path(filepath, paste0(filename, ".svg"))
    datenpfad <- file.path(filepath, paste0(filename, ".csv"))
    
    if(caption != ""){plot <- plot + labs(caption = caption)}
    ggsave(plot = plot,
           filename = pngpfad,
           device = "png")
    if (save_svg == TRUE) {
      ggsave(plot = plot,
             filename = svgpfad,
             device = "svg")
    } 
    
    # Create HTML code
    HTML_text <- "<center>"

    if (plotly == FALSE) {
      HTML_text <- HTML_text |>
        paste0(
          '<img src="',
          pngpfad,
          '" alt="',
          toString(filename),
          '" ',
          extra_html_tags,
          " />",
          sep = ""
        )
    }

    if(caption != "" & plotly){
      HTML_text <- HTML_text |>
        paste0(
          "<small>",
          caption,
          "</small> <br>"
        )
    }

    if (csv_opt == TRUE) {
      HTML_text <- HTML_text |>
        paste0(
          "[Die Daten zur Erstellung dieser Abbildung herunterladen.](",
          datenpfad,
          ") <br>",
          sep = ""
        )
    }
    HTML_text <- HTML_text |>
      paste0(
        'Diese Abbildung herunterladen: <a href ="',
        pngpfad,
        '" download>Als PNG.</a>',
        sep = ""
      ) 

    if (save_svg == TRUE){
      HTML_text <- HTML_text |>
        paste0(
          ' <a href ="',
          svgpfad,
          '" download>Als SVG.</a>',
          sep = ""
        )
    }
    HTML_text <- HTML_text |>
      paste0("</center> <br>")
      
    cat(HTML_text)
    invisible(HTML_text)
  }


globalVariables(c("filename_suffix", "fileformat", "download_link"))

process_data_tibble <- function(data, filename, filepath, type) {
  columns <- names(data)
  column_length <- length(columns)
  correct_column_names <- ("fileformat" %in% columns & "filename_suffix" %in% columns)
  if (column_length != 2 | !correct_column_names) {
    message <- paste("Parameter", type, "should have two columns only. Their names should be: 'fileformat', and 'filename_suffix'")
    stop(message)
  }
  
  data <- data %>%
    mutate(
      path = if_else(
        filename_suffix != "", 
        file.path(filepath, paste0(filename, "_", filename_suffix, ".", fileformat)), # paste0 because sep must be ""
        file.path(filepath, paste0(filename, ".", fileformat)) # paste0 because sep must be ""
      ),
      file_exists = file.exists(path),
      download_link = if_else(
        file_exists,
        paste0( # paste0 because sep must be ""
          '<a href ="',
          path,
          '" download>',
          filename_suffix,
          ' Als ',
          toupper(fileformat),
          '.</a>'
        ),
        NA
      )
    )
  
  if (!all(data$file_exists)) {
    message <- paste("Data files missing: check if data files exist or correct parameter", type, "in image_helper_light()")
    stop(message)
  }
  
  # data_download_links <- apply(data, 1, create_links)
  data_download_links <- data |>
    filter(!is.na(download_link)) |>
    pull(download_link)
  
  data_download_links_str <- paste(data_download_links, collapse=" ") # mind collapse parameter!! else as many html_texts as links
  
  data_download_links_str
}

#' @export image_helper_light
#' @rdname image_helper
image_helper_light <- function(
    captions,
    filename=NULL,
    filepath=NULL,
    data=NULL,
    images=NULL,
    static_image=FALSE,
    static_image_parameters=""
) {
  
  # Create HTML code
  HTML_text <- "<center>"
  
  if (static_image == TRUE & !is.null(images)) {
    
    static_image_paths <- images |>
      mutate(
        path = if_else(
          filename_suffix != "", 
          file.path(filepath, paste0(filename, "_", filename_suffix, ".", fileformat)), # paste0 because sep must be ""
          file.path(filepath, paste0(filename, ".", fileformat)) # paste0 because sep must be ""
        ),
        file_exists = file.exists(path)
      ) |>
      filter(file_exists == TRUE) |>
      pull(path)
    
    HTML_text <- HTML_text |>
      paste0(
        '<img src="',
        static_image_paths[1],
        '" alt="',
        toString(filename),
        '" ',
        static_image_parameters,
        " /><br/>",
        sep = ""
      )
  }
  
  caption_str <- paste(captions, collapse=" | ")
  HTML_text <- HTML_text |>
    paste0( # paste0 because sep must be ""
      "<small>",
      caption_str,
      "</small> <br>"
    )
  
  if (!is.null(data)) {
    if (is.null(filename) | is.null(filepath)) {
      stop("Parameters filename and filepath are required if data parameter is passed to image_helper_light()")
    }
    
    HTML_text <- HTML_text |>
      paste0("Die Daten zur Erstellung dieser Abbildung herunterladen: ") # paste0 because sep must be ""
    
    # make paths for all data formats
    data_download_links_str <- process_data_tibble(data, filename, filepath, "data")
    
    HTML_text <- HTML_text |>
      paste(data_download_links_str, "<br>") # paste because sep must be " "
    
  }
  
  if (!is.null(images)) {
    if (is.null(filename) | is.null(filepath)) {
      stop("Parameters filename and filepath are required if images parameter is passed to image_helper_light()")
    }
    
    HTML_text <- HTML_text |>
      paste0("Diese Abbildung herunterladen: ") # paste0 because sep must be ""
    
    # make paths for all image formats
    images_download_links_str <- process_data_tibble(images, filename, filepath, "images")
    
    HTML_text <- HTML_text |>
      paste(images_download_links_str) # paste0 because sep must be " "
    
  }
  
  HTML_text <- HTML_text |>
    paste("</center> <br>")
  
  cat(HTML_text)
  invisible(HTML_text)
}