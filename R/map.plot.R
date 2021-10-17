#' Map Plot
#'
#' This function creates map plots for given coordinates.
#'
#' @param coordinates matrix of longitude and latitude coordinates (see details)
#' @param names character; row names of \code{coordinates} (depending on \code{coordinates}; see details)
#' @param maptype character; type of map; default: "\code{terrain}" (see details)
#' @param size numeric; integer specifying the size for each object in \code{names}; default: \code{2}
#' @param shape numeric; integer specifying the shape for each object in \code{names}; default: \code{16} (depending on \code{coordinates}; see details)
#' @param colors character; color for each object in \code{names}; default: "\code{black}" (depending on \code{coordinates}; see details)
#' @param x.lab character; label of x-axis; default: "\code{Longitude}"
#' @param y.lab character; label of y-axis; default: "\code{Latitude}"
#' @param title character; title of the plot; default: ""
#' @param legend character; position of the legend; default: "\code{right}" (see details)
#'
#' @details
#' In \code{coordinates} the first columns stands for the longitude and the
#' second column for the latitude coordinate. Furthermore the i-th row entry of \code{coordinates} corresponds to the i-th entry
#' in \code{names}, \code{size}, \code{shape} and \code{colors}. If only one value is provided for \code{names}, \code{size}, \code{shape} and \code{colors} this value is
#' used for all row entries in \code{coordinates}.
#' Only finite values of \code{coordinates} are used.
#'
#' As map source "\code{stamen}" is chosen. Consequently the available maptypes are: "\code{terrain}",
#' "\code{terrain-background}", "\code{terrain-labels}", "\code{terrain-lines}", "\code{toner}",
#' "\code{toner-2010}", "\code{toner-2011}", "\code{toner-background}", "\code{toner-hybrid}",
#' "\code{toner-labels}", "\code{toner-lines}", "\code{toner-lite}", "\code{watercolor}".
#'
#' For the parameters \code{size}, \code{shape} and \code{colors} the specifications of ggplot2 are available.
#'
#' For the parameter \code{legend} the following options are available: "\code{right}", "\code{left}", "\code{bottom}", "\code{top}", "\code{hide}".
#'
#' @return
#' ggplot object with a map plot for given coordinates.
#'
#' @examples
#' #simulated data
#' coordinates <- c(13.38, 52.53, 2.35, 48.86, -0.13, 51.51, -3.70, 40.42)
#' coordinates <- matrix(coordinates, ncol = 2, byrow = TRUE)
#' names1 <- c("Berlin", "Paris", "London", "Madrid")
#' names2 <- c("stations")
#' maptype1 <- "toner-lite"
#' maptype2 <- "terrain-background"
#' size1 <- c(1, 3, 5, 7)
#' size2 <- 5
#' shape1 <- 16:19
#' shape2 <- 17
#' colors1 <- c("darkred", "steelblue", "orange", "darkgreen")
#' colors2 <- "darkred"
#' x.lab <- "Lon"
#' y.lab <- "Lat"
#' title <- "Map"
#' legend <- "bottom"
#'
#' #map.plot plot
#' map.plot(coordinates = coordinates, names = names1)
#' map.plot(coordinates = coordinates, names = names1, maptype = maptype1,
#' size = size1, shape = shape1, colors = colors1, x.lab = x.lab, y.lab = y.lab,
#' title = title, legend = legend)
#' map.plot(coordinates = coordinates, names = names2, maptype = maptype2,
#' size = size2, shape = shape2, colors = colors2)
#'
#' @author David Jobst
#'
#' @rdname map.plot
#'
#' @importFrom stats setNames
#' @importFrom ggmap get_stamenmap ggmap make_bbox calc_zoom
#' @importFrom ggplot2 ggplot ggtitle aes labs xlab ylab theme element_text scale_color_manual scale_shape_manual scale_size_manual
#' @export
map.plot <- function(coordinates, names, maptype = "terrain", size = 2, shape = 16, colors = "black", x.lab = "Longitude", y.lab = "Latitude", title = "", legend = "right") {

  if (!is.array(coordinates)) {
    stop("'coordinates' should be a matrix!")
  }
  if (ncol(coordinates) != 2) {
    stop("'coordinates' should have 2 columns!")
  }
  if(!(length(names) %in% c(1, nrow(coordinates)))) {
    stop("Length of 'names' is not equal to 1 or the number of rows of 'coordinates'!")
  }
  if(length(names) == 1) {
    names <- rep(names, nrow(coordinates))
  }
  if (length(size) != length(shape) || length(size) != length(colors)) {
    stop("Lengths of 'size', 'shape' and 'colors' are not equal!")
  }

  #prepare data
  Longitude <- coordinates[, 1]
  Latitude <- coordinates[, 2]
  location <- make_bbox(Longitude, Latitude)
  zoom <- calc_zoom(location)
  suppressMessages(map <- get_stamenmap(location, zoom = zoom, maptype = maptype))
  Legend <- names
  data <- data.frame(Longitude, Latitude, Legend)

  if (!(length(size) %in% c(1, length(names)))) {
    stop("Length of 'size' should be 1 or the length of 'names'!")
  } else if (length(size) == 1) {
      size <- rep(size, length(names))
  }
  if (!(length(shape) %in% c(1, length(names)))) {
    stop("Length of 'shape' should be 1 or the length of 'names'!")
  } else if (length(shape) == 1) {
      shape <- rep(shape, length(names))
  }

  if (!(length(colors) %in% c(1, length(names)))) {
    stop("Length of 'colors' should be 1 or the length of 'names'!")
  } else if (length(colors) == 1) {
      colors <- rep(colors, length(names))
  }
  shape <- setNames(shape, Legend)
  size <- setNames(size, Legend)
  colors <- setNames(colors, Legend)

  mymap <- ggmap(map)
  p <- mymap +
    geom_point(aes(x = Longitude, y = Latitude, color = Legend, shape = Legend, size = Legend), data) +
    scale_shape_manual(values = shape) +
    scale_size_manual(values = size) +
    scale_color_manual(values = colors) +
    xlab(x.lab) +
    ylab(y.lab) +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = legend)

  return(p)
}

