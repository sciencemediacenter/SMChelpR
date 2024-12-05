#' Helper functions to create map visualizations with leaflet



#' create_base_map
#' 
#' Function for initializing a leaflet map with customized parameters.
#' 
#' Wrapper function for leaflet() that initializes an empty leaflet map with customized parameters:
#' leafletOptions, setView, setMaxBounds, and prepends a html-extra_style.
#' 
#' @param ... Additional parameters to be passed to leafletOptions().
#' @param theme_params list, list of the customized parameters that can be obtained via [get_base_map_parameters()].
#' @return Leaflet map widget
#' @examples
#' create_base_map(base_map_parameters = get_base_map_parameters())
#' @export create_base_map
create_base_map <- function(..., base_map_parameters = list()) {
  map <- leaflet(
    options = leafletOptions(
      ...,
      
      height = base_map_parameters$height,
      minZoom = base_map_parameters$minZoom,
      maxZoom = base_map_parameters$maxZoom,
      opacity = base_map_parameters$opacity,
      doubleClickZoom = base_map_parameters$doubleClickZoom,
      scrollWheelZoom = base_map_parameters$scrollWheelZoom,
      maxBoundsViscosity = base_map_parameters$maxBoundsViscosity
    )
  ) |>
    setView(
      base_map_parameters$center_lon,
      base_map_parameters$center_lat,
      base_map_parameters$zoom
    ) |>
    setMaxBounds(
      base_map_parameters$min_lon,
      base_map_parameters$min_lat,
      base_map_parameters$max_lon,
      base_map_parameters$max_lat
    ) |>
    htmlwidgets::prependContent(base_map_parameters$extra_style)
  
  return(map)
}

#' get_base_map_parameters
#' 
#' Function that returns a list of default parameters for creating a leaflet map.
#' Default parameters are tweaked such that they are suitable for a map of Germany.
#' 
#' @return List of default parameters for creating a leaflet map
#' @examples
#' get_base_map_parameters()
#' @export get_base_map_parameters
get_base_map_parameters <-  function() {
  list(
    height = 5000,
    minZoom = 4,
    maxZoom = 14,
    opacity = 0,
    center_lon = 9.5,
    center_lat = 51.2,
    zoom = 5.5,
    min_lon = 3,
    min_lat = 47,
    max_lon = 17,
    max_lat = 56,
    doubleClickZoom = FALSE,
    scrollWheelZoom = FALSE,
    maxBoundsViscosity = 0.5,
    # If maxBounds is set, this option controls how solid the bounds are when dragging
    extra_style = tags$style(".leaflet-container { background: white !important; }")
  )
}

#' map_add_choropleth_layer
#' 
#' Wrapper function for addPolygons() that adds a choropleth layer to a leaflet map.
#'
#' @param map Leaflet map widget
#' @param polygon_data SpatialPolygonsDataFrame, data for the choropleth layer
#' @param polygon_color_mapping character vector, one color per value in polygon_data
#' @param polygon_label character vector, labels for the polygons
#' @param choropleth_layer_parameters list, list of the customized parameters that can be obtained via [get_choropleth_layer_parameters()].
#' @param leaflet_group character, name of the group to which the layer belongs
#' @export map_add_choropleth_layer
map_add_choropleth_layer <- function(map,
                                     polygon_data,
                                     polygon_color_mapping,
                                     polygon_label,
                                     choropleth_layer_parameters,
                                     leaflet_group) {
  map <- map |>
    addPolygons(
      data = polygon_data,
      fillColor = ~ polygon_color_mapping,
      color = ~ polygon_color_mapping,
      label = polygon_label,
      
      # aesthetic parameters
      smoothFactor = choropleth_layer_parameters$smoothFactor,
      fillOpacity = choropleth_layer_parameters$fillOpacity,
      weight = choropleth_layer_parameters$weight,
      labelOptions = choropleth_layer_parameters$labelOptions,
      highlight = choropleth_layer_parameters$highlight,
      
      # organizational parameters
      options = choropleth_layer_parameters$options,
      group = leaflet_group
    )
  
  return(map)
}

#' get_choropleth_layer_parameters
#' 
#' Function that returns a list of default parameters for a choropleth layer in a leaflet map.
#' 
#' @return List of default parameters for adding a leaflet choropleth layer
#' @examples
#' get_choropleth_layer_parameters()
#' @export get_choropleth_layer_parameters
get_choropleth_layer_parameters <- function() { 
  list(
    smoothFactor = 0.2,
    fillOpacity = 1,
    weight = 0,
    labelOptions = labelOptions(style = list(
      "background" = "#f4efe1",
      "font-size" = "12px"
    )),
    highlight = highlightOptions(
      # on mouseover
      weight = 3,
      color = "black",
      bringToFront = TRUE,
      # sendToBack = TRUE
    ),
    options = pathOptions(pane = "choropleth")
  )
}

#' addLegendCustom
#' 
#' Wrapper function for addLegend() that allows to put circles as legend items.
#' 
#' @param map Leaflet map widget
#' @param position character, position of the legend
#' @param colors character vector, colors of the circles
#' @param labels character vector, labels of the circles
#' @param sizes numeric vector, sizes of the circles
#' @param title character, title of the legend
#' @param opacity numeric, opacity of the circles
#' @export addLegendCustom
addLegendCustom <- function(map, position, colors, labels, sizes, title = NULL, opacity = 0.5){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, position, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity, title = title
  ))
}

#' map_add_circle_marker_layer
#' 
#' Function that generates default parameters for addCircleMarkers()
#' 
#' @return List of default parameters for addCircleMarkers()
#' @examples
#' get_circle_marker_parameters()
#' @export get_circle_marker_parameters
get_circle_marker_parameters <- function() {
  list(
    radius = 5,
    stroke = TRUE,
    weight = 0.5,
    fillOpacity = 1,
    labelOptions = labelOptions(style = list("font-size" = "12px")),
    options = markerOptions(riseOnHover = TRUE, pane = "markerPane")
  )
}

#' create_custom_palette
#' 
#' Function, that allows for a diverging color palette with a shifted midpoint.
#' In addition to the linear scaling, the function allows for an exponential scaling of the color ramp.
#' 
#' @param low_color character, color for low values
#' @param mid_color character, color for mid values
#' @param high_color character, color for high values
#' @param start_point numeric, start point of the color scale
#' @param mid_point numeric, midpoint of the color scale
#' @param end_point numeric, end point of the color scale
#' @param exp_factor numeric, factor for the exponential scaling (linear scaling if 1)
#' @return Function that maps numeric values to colors
#' @examples
#' create_custom_palette("blue", "white", "red", 0, 50, 100, exp_factor = 1)
#' @export create_custom_palette
create_custom_palette <- function(low_color,
                                              mid_color,
                                              high_color,
                                              start_point,
                                              mid_point,
                                              end_point,
                                              exp_factor = 1) {
  f <- function(x) {
    # Rescale values below midpoint with exponentially increasing speed
    below_mid <- x[x <= mid_point]
    if (length(below_mid) > 0) {
      normalized <- (below_mid - start_point) / (mid_point - start_point)
      below_mid <- normalized ^ exp_factor  # exponential scaling
      below_mid <- below_mid * 0.5  # scale to 0-0.5 range
    }
    
    # Rescale values above midpoint with exponentially decreasing speed
    above_mid <- x[x > mid_point]
    if (length(above_mid) > 0) {
      normalized <- (above_mid - mid_point) / (end_point - mid_point)
      above_mid <- 1 - (1 - normalized) ^ exp_factor  # inverse exponential scaling
      above_mid <- 0.5 + (above_mid * 0.5)  # scale to 0.5-1 range
    }
    
    # Combine the rescaled values
    rescaled <- numeric(length(x))
    rescaled[x <= mid_point] <- below_mid
    rescaled[x > mid_point] <- above_mid
    
    # Create color ramp and apply
    colors <- colorRampPalette(c(low_color, mid_color, high_color))(100)
    color_indices <- round(rescaled * 99) + 1
    return(colors[color_indices])
  }
  
  # Add necessary attributes for leaflet
  attr(f, "colorType") <- "numeric"
  attr(f, "domain") <- c(start_point, end_point)
  attr(f, "range") <- c(low_color, mid_color, high_color)
  attr(f, "interpolate") <- TRUE
  
  return(f)
}

