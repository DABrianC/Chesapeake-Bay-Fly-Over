library(raster)
library(magrittr)
library(stringr)
library(rgl)
library(rayshader)
library(magick)
library(ggplot2)
library(httr)
library(leaflet) 
library(sf)

source("https://gist.github.com/fkeck/4820db83b9ff2fbf1f7fe901563ddc82/raw/")

# Some useful functions by Will Bishop from his post on rayshader
# https://wcmbishop.github.io/rayshader-demo/
source("https://raw.github.com/wcmbishop/rayshader-demo/master/R/rayshader-gif.R")

mnt <- raster("combined_leman.tif")
mnt <- aggregate(mnt, 2) # Simplification factor

ccv <- rgdal::readOGR("ccv.shp")

plot(mnt)
plot(ccv, add = T)


bbox <- list(
  p1 = list(long = -77.5, lat = 37.8864 )
  , p2 = list(long = -75.2433 , lat = 39.723 )
)



leaflet() %>%
  addTiles() %>%
  addRectangles(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat
    , lng2 = bbox$p2$long, lat2 = bbox$p2$lat
    , fillColor = "transparent"
  )  %>%
  fitBounds(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat
    , lng2 = bbox$p2$long, lat2 = bbox$p2$lat
  )

###Custom function for image size. This is important for rayshader
define_image_size <- function(bbox, major_dim = 400) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs(({bbox$p1$long} - {bbox$p2$long}) / ({bbox$p1$lat} - {bbox$p2$lat}))
  # define dimensions
  img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}



image_size <- define_image_size(bbox, major_dim = 600)


get_usgs_elevation_data <- function(bbox, size = "400,400", file = NULL, 
                                    sr_bbox = 4326, sr_image = 4326) {
  require(httr)
  
  # TODO - validate inputs
  
  url <- parse_url("https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage")
  res <- GET(
    url, 
    query = list(
      bbox = paste(bbox$p1$long, bbox$p1$lat, bbox$p2$long, bbox$p2$lat,
                   sep = ","),
      bboxSR = sr_bbox,
      imageSR = sr_image,
      size = size,
      format = "tiff",
      pixelType = "F32",
      noDataInterpretation = "esriNoDataMatchAny",
      interpolation = "+RSP_BilinearInterpolation",
      f = "json"
    )
  )
  
  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    # TODO - check that bbox values are correct
    # message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    
    img_res <- GET(body$href)
    img_bin <- content(img_res, "raw")
    if (is.null(file)) 
      file <- tempfile("elev_matrix", fileext = ".tif")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    warning(res)
  }
  invisible(file)
}

#Download my elevation file
elev_file <- file.path("cb_elevevation.tif")
get_usgs_elevation_data({bbox}
                        , size = image_size$size
                        , file = elev_file
                        , sr_bbox = 4326
                        , sr_image = 4326)

#Load my elevation file
elev_img <- raster::raster(elev_file)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

# calculate rayshader layers
ambmat <- ambient_shade(elev_matrix, zscale = 30)
raymat <- ray_shade(elev_matrix, zscale = 30, lambert = TRUE)
watermap <- detect_water(elev_matrix)

# plot 2D
elev_matrix %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()

#Function for getting the overlay image
get_arcgis_map_image <- function(bbox, map_type = "World_Street_Map", file = NULL, 
                                 width = 400, height = 400, sr_bbox = 4326) {
  require(httr)
  require(glue) 
  require(jsonlite)
  
  url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")
  
  # define JSON query parameter
  web_map_param <- list(
    baseMap = list(
      baseMapLayers = list(
        list(url = jsonlite::unbox(glue("https://services.arcgisonline.com/ArcGIS/rest/services/{map_type}/MapServer",
                                        map_type = map_type)))
      )
    ),
    exportOptions = list(
      outputSize = c(width, height)
    ),
    mapOptions = list(
      extent = list(
        spatialReference = list(wkid = jsonlite::unbox(sr_bbox)),
        xmax = jsonlite::unbox(max(bbox$p1$long, bbox$p2$long)),
        xmin = jsonlite::unbox(min(bbox$p1$long, bbox$p2$long)),
        ymax = jsonlite::unbox(max(bbox$p1$lat, bbox$p2$lat)),
        ymin = jsonlite::unbox(min(bbox$p1$lat, bbox$p2$lat))
      )
    )
  )
  
  res <- GET(
    url, 
    query = list(
      f = "json",
      Format = "PNG32",
      Layout_Template = "MAP_ONLY",
      Web_Map_as_JSON = jsonlite::toJSON(web_map_param))
  )
  
  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    if (is.null(file)) 
      file <- tempfile("overlay_img", fileext = ".png")
    
    img_res <- GET(body$results[[1]]$value$url)
    img_bin <- content(img_res, "raw")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    message(res)
  }
  invisible(file)
}

# fetch overlay image
overlay_file <- "chesapeake-map.png"
get_arcgis_map_image(bbox, map_type = "World_Topo_Map", file = overlay_file,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)
overlay_img <- png::readPNG(overlay_file)

# 2D plot with map overlay
elev_matrix %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  plot_map()

#Make it 3d
zscale <- 10
rgl::clear3d()
elev_matrix %>% 
  sphere_shade(texture = "imhof4") %>% 
  add_water(watermap, color = "imhof4") %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = TRUE, soliddepth = -max(elev_matrix)/zscale, wateralpha = 0,
          theta = 25, phi = 30, zoom = 0.65, fov = 60)
render_snapshot()


# frame transition variables
n_frames <- 180

waterdepthvalues <- min(elev_matrix)/2 - min(elev_matrix)/2 * cos(seq(0,2*pi,length.out = n_frames))
thetavalues <- -90 + 45 * cos(seq(0, 2*pi, length.out = n_frames))
# shadow layers
ambmat <- ambient_shade(elev_matrix, zscale = zscale)
raymat <- ray_shade(elev_matrix, zscale = zscale, lambert = TRUE)

# generate .png frame images
img_frames <- paste0("bay", seq_len(n_frames), ".png")
for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  elev_matrix %>%
    sphere_shade(texture = "imhof1") %>%
    add_shadow(ambmat, 0.5) %>%
    add_shadow(raymat, 0.5) %>%
    plot_3d(elev_matrix, solid = TRUE, shadow = TRUE, zscale = zscale, 
            water = TRUE, watercolor = "imhof3", wateralpha = 0.8, 
            waterlinecolor = "#ffffff", waterlinealpha = 0.5,
            waterdepth = waterdepthvalues[i]/zscale, 
            theta = thetavalues[i], phi = 45)
  render_snapshot(img_frames[i])
  rgl::clear3d()
}
  save_3d_gif <- function(hillshade
                          , heightmap
                          , file
                          , duration = 5, ...) {
    require(rayshader)
    require(magick)
    require(rgl)
    require(gifski)
    require(rlang)
    
    # capture dot arguments and extract variables with length > 1 for gif frames
    dots <- rlang::list2(...)
    var_exception_list <- c("windowsize")
    dot_var_lengths <- purrr::map_int(dots, length)
    gif_var_names <- names(dots)[dot_var_lengths > 1 & 
                                   !(names(dots) %in% var_exception_list)]
    # split off dot variables to use on gif frames
    gif_dots <- dots[gif_var_names]
    static_dots <- dots[!(names(dots) %in% gif_var_names)]
    gif_var_lengths <- purrr::map_int(gif_dots, length)
    # build expressions for gif variables that include index 'i' (to use in the for loop)
    gif_expr_list <- purrr::map(names(gif_dots), ~rlang::expr(gif_dots[[!!.x]][i]))
    gif_exprs <- exprs(!!!gif_expr_list)
    names(gif_exprs) <- names(gif_dots)
    message(paste("gif variables found:", paste(names(gif_dots), collapse = ", ")))
    
    # TODO - can we recycle short vectors?
    if (length(unique(gif_var_lengths)) > 1) 
      stop("all gif input vectors must be the same length")
    n_frames <- unique(gif_var_lengths)
    
    # generate temp .png images
    temp_dir <- tempdir()
    img_frames <- file.path(temp_dir, paste0("frame-", seq_len(n_frames), ".png"))
    on.exit(unlink(img_frames))
    message(paste("Generating", n_frames, "temporary .png images..."))
    for (i in seq_len(n_frames)) {
      message(paste(" - image", i, "of", n_frames))
      rgl::clear3d()
      hillshade %>%
        plot_3d_tidy_eval(heightmap, !!!append(gif_exprs, static_dots))
      rgl::snapshot3d(img_frames[i])
    }
    
    # build gif
    message("Generating .gif...")
    magick::image_write_gif(magick::image_read(img_frames) 
                            , path = "chesapeake.gif"
                            , delay = duration/n_frames)
    message("Done!")
    invisible(file)
  }
  
  
  plot_3d_tidy_eval <- function(hillshade, ...) {
    dots <- rlang::enquos(...)
    plot_3d_call <- rlang::expr(plot_3d(hillshade, !!!dots))
    rlang::eval_tidy(plot_3d_call)
  }
  
  
  #' Create a numeric vector of transition values.
  #' @description This function helps generate a sequence 
  #' of numeric values to transition "from" a start point
  #' "to" some end point. The transition can be "one_way" 
  #' (meaning it ends at the "to" point) or "two_way" (meaning
  #' we return back to end at the "from" point).
  #'
  #' @param from starting point for transition values
  #' @param to ending point (for one-way transitions) or turn-around point 
  #'           (for two-way transitions)
  #' @param steps the number of steps to take in the transation (i.e. the length
  #'              of the returned vector)
  #' @param one_way logical value to determine if we should stop at the "to" value
  #'                (TRUE) or turn around and return to the "from" value (FALSE)
  #' @param type string defining the transition type - currently suppoerts "cos"
  #'             (for a cosine curve) and "lin" (for linear steps)
  #'
  #' @return a numeric vector of transition values
  #' 
  transition_values <- function(from, to, steps = 10, 
                                one_way = FALSE, type = "cos") {
    if (!(type %in% c("cos", "lin")))
      stop("type must be one of: 'cos', 'lin'")
    
    range <- c(from, to)
    middle <- mean(range)
    half_width <- diff(range)/2
    
    # define scaling vector starting at 1 (between 1 to -1)
    if (type == "cos") {
      scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))
    } else if (type == "lin") {
      if (one_way) {
        xout <- seq(1, -1, length.out = steps)
      } else {
        xout <- c(seq(1, -1, length.out = floor(steps/2)), 
                  seq(-1, 1, length.out = ceiling(steps/2)))
      }
      scaling <- approx(x = c(-1, 1), y = c(-1, 1), xout = xout)$y 
    }
    
    middle - half_width * scaling
  }
  
  
  # gif transition variables
  theta <- transition_values(from = 0, to = 360, steps = n_frames, 
                             one_way = TRUE, type = "lin")
  phi <- transition_values(from = 10, to = 70, steps = n_frames, 
                           one_way = FALSE, type = "cos")
  zoom <- transition_values(from = 0.4, to = 0.8, steps = n_frames, 
                            one_way = FALSE, type = "cos")
  
  # GIF it!
  zscale <- 10
  elev_matrix %>% 
    sphere_shade(texture = "imhof4") %>% 
    add_water(watermap, color = "imhof4") %>%
    add_overlay(overlay_img, alphalayer = 0.5) %>%
    add_shadow(raymat, 0.4) %>%
    add_shadow(ambmat, 0.4) %>%
    save_3d_gif(elev_matrix, file = "chesapeake_fly_by2.gif", duration = 10,
                zscale = zscale, windowsize = c(1200, 1000), wateralpha = 0,
                water = TRUE, soliddepth = -max(elev_matrix)/zscale, 
                theta = theta, phi = phi, zoom = zoom, fov = 60)
  