#' @aliases to_kmz
#'
#' @title Generate KMZ Files from Tracking Locations
#'
#' @description
#' Generates KMZ files containing tracking locations and tracks. By default,
#' the data are split by individual ID and one KMZ file is created for each
#' individual. All locations can instead be combined in one KMZ file by setting
#' \code{split_by = NULL}.
#'
#' Locations within each subset are ordered by \code{DateTime} and displayed as
#' points. When a track is included, the locations are connected sequentially.
#' Point names show the value of the splitting variable, the chronological
#' position and the total number of locations, for example
#' \code{K25713 (72 of 125)}.
#'
#' @param sdata A data frame containing tracking locations. It must contain:
#'   \describe{
#'     \item{\code{DateTime}}{Date and time of each location in GMT, either as
#'       \code{\link[base:DateTimeClasses]{POSIXct}} or as a character value in
#'       \code{YYYY-MM-DD HH:MM:SS} format, for example
#'       \code{2012-06-03 01:33:46}.}
#'     \item{\code{lon}}{Longitude of each location, expressed in the
#'       coordinate reference system specified by \code{crs}.}
#'     \item{\code{lat}}{Latitude of each location, expressed in the
#'       coordinate reference system specified by \code{crs}.}
#'   }
#'   When \code{split_by} is not \code{NULL}, the data frame must also contain
#'   the column specified by \code{split_by}.
#'
#' @param split_by Character or \code{NULL}. Name of the column used to split
#'   the data into separate KMZ files. Defaults to \code{"id"}, producing one
#'   KMZ file for each individual. If \code{NULL}, all locations are exported
#'   to one KMZ file and treated as one chronological track.
#'
#' @param crs Coordinate reference system of the input \code{lon} and
#'   \code{lat} columns, specified in a form accepted by
#'   \code{\link[sf:st_crs]{st_crs}}. Defaults to EPSG:4326 (WGS 84). Input
#'   coordinates are transformed to WGS 84 before the KMZ files are created.
#'
#' @param file_name Character vector or \code{NULL}. Controls the output file
#'   names.
#'
#'   When \code{split_by} is not \code{NULL} and \code{file_name = NULL}, each
#'   value of the splitting variable is used as the file name.
#'
#'   When \code{append_file_name = TRUE}, \code{file_name} must contain one
#'   value, which is appended to each splitting value. For example,
#'   \code{file_name = "filtered"} produces
#'   \code{K25713_filtered.kmz}.
#'
#'   When \code{append_file_name = FALSE}, \code{file_name} must contain one
#'   complete file name for each output KMZ file. Names can be supplied in the
#'   order of the splitting values or as a named character vector in which each
#'   name matches a splitting value.
#'
#'   When \code{split_by = NULL}, \code{file_name} must contain at most one
#'   value. If \code{file_name = NULL}, the output file is named
#'   \code{locations.kmz}.
#'
#' @param append_file_name Logical. Controls how \code{file_name} is used when
#'   \code{split_by} is not \code{NULL}. If \code{TRUE}, the default,
#'   \code{file_name} is appended to each splitting value. If \code{FALSE},
#'   each value of \code{file_name} is used as the complete name of one output
#'   KMZ file. Ignored when \code{file_name = NULL} or
#'   \code{split_by = NULL}.
#'
#' @param output_dir Character. Directory in which to save the KMZ files.
#'   Defaults to the current working directory. The directory is created if it
#'   does not exist.
#'
#' @param point_symbol Character. URL of the KML symbol used for tracking
#'   locations. Defaults to a circular symbol from the Google KML icon library.
#'
#' @param point_label Character vector or \code{NULL}. Names of columns used to
#'   provide descriptive information for each point. This information is
#'   separate from the point name and is displayed by Google Earth beneath the
#'   point in the Places pane and in the popup when the point is selected. Each
#'   column name and value pair is shown on a separate line, for example:
#'   \preformatted{
#' DateTime: 2012-06-03 01:33:46
#' qi: 4
#'   }
#'   If \code{NULL}, the default, only \code{DateTime} is used. Supplying one or
#'   more column names replaces this default. For example,
#'   \code{point_label = "qi"} displays only \code{qi}, whereas
#'   \code{point_label = c("DateTime", "qi")} displays both.
#'   
#' @param point_colour Point colours. Supply a single colour, a palette function
#'   that accepts the required number of colours, or a vector of colours.
#'
#'   When \code{point_colour_by = NULL}, a single colour is applied to all
#'   points, a palette function generates colours in chronological order within
#'   each subset, and a colour vector must contain one colour for each original
#'   row of \code{sdata}.
#'
#'   When \code{point_colour_by} specifies a column, supply either a palette
#'   function or one colour for each distinct retained value in that column.
#'   Colours must be recognised by \code{\link[grDevices]{col2rgb}}. Defaults
#'   to \code{"red"}.
#'
#' @param point_colour_by Character or \code{NULL}. Name of the column used to
#'   assign point colours. If \code{NULL}, the default, a palette function
#'   applies colours chronologically within each subset. If a column is
#'   specified, equal values receive the same colour across all output files.
#'   Numeric values are mapped in ascending order; other values are mapped in
#'   their order of first appearance.
#'
#' @param point_size Numeric. Scale of the point symbols. Defaults to
#'   \code{0.6}.
#'
#' @param line_colour Character or \code{NULL}. Track colour recognised by
#'   \code{\link[grDevices]{col2rgb}}. Set to \code{NULL} to omit tracks and
#'   display points only. Defaults to \code{"grey67"}.
#'
#' @param line_size Numeric. Track width in KML units. Defaults to \code{3}.
#'   Ignored when \code{line_colour = NULL}.
#'
#' @param overwrite Logical. If \code{TRUE}, existing KMZ files with the same
#'   names are overwritten. Defaults to \code{FALSE}.
#'
#' @details
#' Records with missing \code{lon}, \code{lat} or \code{DateTime} values are
#' excluded. If the data are split using a column such as \code{id}, any row
#' where the value in that column is missing cannot be assigned to an output
#' file and is therefore removed.
#'
#' If \code{point_colour_by} is supplied, its retained values must not be
#' missing. The colour mapping is determined across all retained data before
#' the data are split. The same value therefore receives the same colour in
#' every output file.
#'
#' Row-specific point colours remain associated with the correct records after
#' incomplete records are removed, the data are split and the locations are
#' sorted.
#'
#' Point numbering is assigned separately within each subset after
#' chronological sorting. A point named \code{K25713 (72 of 125)} is therefore
#' the 72nd of 125 retained locations for \code{K25713}.
#'
#' Point names are hidden beside the map symbols. When a point is selected, the
#' columns specified by \code{point_label} are displayed in the standard Google
#' Earth popup. Each column name and value pair is shown on a separate line.
#' \code{\link[base:DateTimeClasses]{POSIXct}} values are displayed in
#' \code{YYYY-MM-DD HH:MM:SS} format.
#'
#' Changing an output file name does not change the point names or track name
#' within the KMZ file. These continue to use the corresponding value of the
#' splitting variable.
#'
#' A track is included only when a subset contains at least two complete
#' locations and \code{line_colour} is not \code{NULL}.
#'
#' When \code{split_by = NULL}, all retained locations are treated as one track,
#' sorted by \code{DateTime} and connected sequentially regardless of
#' individual identity. If multiple individuals were tracked at the same time,
#' locations from different individuals may be repeatedly connected according
#' only to their chronological order. Use an individual or track identifier for
#' \code{split_by} when separate tracks are required.
#'
#' @return Invisibly returns a character vector containing the full paths to
#'   the KMZ files created.
#'
#' @author Takahiro Shimada
#'
#' @seealso \code{\link{to_map}}
#'
#' @examples
#' ## Prepare tracking data
#' data(turtle)
#' data(turtle2)
#' sdata <- rbind(turtle, turtle2)
#' sdata <- ddfilter(dupfilter(sdata))
#'
#' \dontrun{
#' ## Generate one KMZ file per ID
#' to_kmz(sdata)
#'
#' ## Apply a chronological HCL colour gradient within each ID
#' to_kmz(
#'   sdata,
#'   point_colour = function(n) {
#'     grDevices::hcl.colors(
#'       n,
#'       palette = "YlOrRd",
#'       rev = TRUE
#'     )
#'   }
#' )
#'
#' ## Generate palette colours according to qi
#' to_kmz(
#'   sdata,
#'   point_colour_by = "qi",
#'   point_colour = function(n) {
#'     grDevices::hcl.colors(
#'       n,
#'       palette = "YlOrRd",
#'       rev = TRUE
#'     )
#'   },
#'   point_label = c("DateTime", "qi")
#' )
#'
#' ## Assign specified colours to qi values 4 to 8
#' to_kmz(
#'   sdata,
#'   point_colour_by = "qi",
#'   point_colour = c(
#'     "blue",
#'     "cyan",
#'     "yellow",
#'     "orange",
#'     "red"
#'   ),
#'   point_label = c("DateTime", "qi")
#' )
#'
#' ## Append text to each split file name
#' ## For example, K25713_filtered.kmz
#' to_kmz(
#'   sdata,
#'   file_name = "filtered"
#' )
#'
#' ## Supply complete file names in split-value order
#' to_kmz(
#'   sdata,
#'   file_name = c(
#'     "Moreton_Bay_track",
#'     "Hervey_Bay_track"
#'   ),
#'   append_file_name = FALSE
#' )
#'
#' ## Create a species column, split by species and display points only
#' sdata$species <- "Chelonia mydas"
#'
#' to_kmz(
#'   sdata,
#'   split_by = "species",
#'   line_colour = NULL
#' )
#'
#' ## Export all locations as one combined_locations.kmz file
#' to_kmz(
#'   sdata,
#'   split_by = NULL,
#'   line_colour = NULL,
#'   file_name = "combined_locations"
#' )
#' }
#'
#' @export


to_kmz <- function(
    sdata,
    split_by = "id",
    crs = 4326,
    file_name = NULL,
    append_file_name = TRUE,
    output_dir = getwd(),
    point_symbol =
      "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
    point_label = NULL,
    point_colour = "red",
    point_colour_by = NULL,
    point_size = 0.6,
    line_colour = "grey67",
    line_size = 3,
    overwrite = FALSE
) {
  
  # Validate inputs
  if (!is.data.frame(sdata) || !nrow(sdata)) {
    stop("`sdata` must be a non-empty data frame.", call. = FALSE)
  }
  
  if (
    !is.null(split_by) &&
    (!is.character(split_by) || length(split_by) != 1L)
  ) {
    stop(
      "`split_by` must be `NULL` or one column name.",
      call. = FALSE
    )
  }
  
  if (
    !is.null(point_label) &&
    (!is.character(point_label) || !length(point_label))
  ) {
    stop(
      "`point_label` must be `NULL` or one or more column names.",
      call. = FALSE
    )
  }
  
  if (
    !is.null(point_colour_by) &&
    (!is.character(point_colour_by) ||
     length(point_colour_by) != 1L)
  ) {
    stop(
      "`point_colour_by` must be `NULL` or one column name.",
      call. = FALSE
    )
  }
  
  required <- c(
    "lon",
    "lat",
    "DateTime",
    split_by,
    point_label,
    point_colour_by
  )
  
  missing <- setdiff(required, names(sdata))
  
  if (length(missing)) {
    stop(
      "Missing required column",
      if (length(missing) == 1L) ": " else "s: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  input_rows <- nrow(sdata)
  
  # Retain row-specific colours before filtering
  if (
    is.null(point_colour_by) &&
    !is.function(point_colour) &&
    length(point_colour) > 1L
  ) {
    if (length(point_colour) != input_rows) {
      stop(
        "When `point_colour_by = NULL`, a point-colour vector must ",
        "contain one colour for each original row of `sdata`.",
        call. = FALSE
      )
    }
    
    sdata$.point_colour <- point_colour
  }
  
  # Remove incomplete locations
  complete_columns <- c(
    "lon",
    "lat",
    "DateTime",
    split_by
  )
  
  complete <- stats::complete.cases(
    sdata[complete_columns]
  )
  
  removed <- sum(!complete)
  sdata <- sdata[complete, , drop = FALSE]
  
  if (!nrow(sdata)) {
    stop("No complete locations found.", call. = FALSE)
  }
  
  if (removed) {
    message(
      removed,
      " incomplete location",
      if (removed == 1L) "" else "s",
      " removed."
    )
  }
  
  if (
    !is.null(point_colour_by) &&
    anyNA(sdata[[point_colour_by]])
  ) {
    stop(
      "`", point_colour_by,
      "` contains missing values and cannot be used for point colours.",
      call. = FALSE
    )
  }
  
  # Convert input coordinates to WGS 84
  coordinates <- sdata |>
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = crs
    ) |>
    sf::st_transform(4326) |>
    sf::st_coordinates()
  
  sdata$lon <- coordinates[, "X"]
  sdata$lat <- coordinates[, "Y"]
  
  # Helpers
  kml_colour <- function(x) {
    rgb <- grDevices::col2rgb(x)
    
    sprintf(
      "ff%02x%02x%02x",
      rgb[3, ],
      rgb[2, ],
      rgb[1, ]
    )
  }
  
  escape_xml <- function(x) {
    x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    gsub("'", "&apos;", x, fixed = TRUE)
  }
  
  escape_cdata <- function(x) {
    gsub(
      "]]>",
      "]]]]><![CDATA[>",
      as.character(x),
      fixed = TRUE
    )
  }
  
  safe_name <- function(x) {
    x <- tools::file_path_sans_ext(
      basename(as.character(x))
    )
    
    gsub('[<>:"/\\\\|?*]', "_", x)
  }
  
  format_value <- function(x) {
    if (inherits(x, "POSIXt")) {
      format(x, "%Y-%m-%d %H:%M:%S")
    } else {
      as.character(x)
    }
  }
  
  # Assign colours from a variable across all retained data
  if (!is.null(point_colour_by)) {
    values <- sdata[[point_colour_by]]
    
    levels <- if (is.numeric(values)) {
      sort(unique(values))
    } else {
      unique(as.character(values))
    }
    
    palette <- if (is.function(point_colour)) {
      point_colour(length(levels))
    } else {
      point_colour
    }
    
    if (length(palette) != length(levels)) {
      stop(
        "`point_colour` must provide one colour for each distinct value ",
        "of `", point_colour_by, "` (",
        length(levels), " colours required).",
        call. = FALSE
      )
    }
    
    sdata$.point_colour <- palette[
      match(
        as.character(values),
        as.character(levels)
      )
    ]
  }
  
  point_symbol <- escape_xml(point_symbol)
  
  if (!is.null(line_colour)) {
    line_colour <- kml_colour(line_colour)
  }
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Write one KMZ
  write_kmz <- function(data, track_name, output_name) {
    
    data <- data[order(data$DateTime), , drop = FALSE]
    
    total <- nrow(data)
    number <- seq_len(total)
    track_name <- escape_xml(track_name)
    
    point_names <- paste0(
      track_name,
      " (",
      number,
      " of ",
      total,
      ")"
    )
    
    # Point colours
    colours <- if (is.function(point_colour) && is.null(point_colour_by)) {
        point_colour(total)
      
      } else if (length(point_colour) == 1L && is.null(point_colour_by)) {
        rep(point_colour, total)
      
      } else {
        data$.point_colour
      }
    
    if (length(colours) != total || anyNA(colours)) {
      stop(
        "`point_colour` must provide one non-missing colour per point.",
        call. = FALSE
      )
    }
    
    colours <- kml_colour(colours)
    
    # Popup labels
    label_columns <- if (is.null(point_label)) {
      "DateTime"
    } else {
      point_label
    }
    
    labels <- vapply(
      number,
      function(i) {
        fields <- vapply(
          label_columns,
          function(column) {
            paste0(
              "<b>",
              escape_cdata(column),
              ":</b> ",
              escape_cdata(
                format_value(data[[column]][i])
              )
            )
          },
          character(1)
        )
        
        paste(fields, collapse = "<br/>")
      },
      character(1)
    )
    
    # Output paths
    kmz_file <- file.path(
      output_dir,
      paste0(safe_name(output_name), ".kmz")
    )
    
    if (file.exists(kmz_file) && !overwrite) {
      stop(
        "Output file already exists: ",
        kmz_file,
        "\nSet `overwrite = TRUE` to overwrite it.",
        call. = FALSE
      )
    }
    
    if (file.exists(kmz_file)) {
      unlink(kmz_file)
    }
    
    temp_dir <- tempfile("to_kmz_")
    dir.create(temp_dir)
    
    on.exit(
      unlink(temp_dir, recursive = TRUE),
      add = TRUE
    )
    
    kml_file <- file.path(temp_dir, "doc.kml")
    
    # Point styles
    point_styles <- vapply(
      number,
      function(i) {
        paste0(
          '    <Style id="point_', i, '">\n',
          "      <IconStyle>\n",
          "        <color>", colours[i], "</color>\n",
          "        <scale>", point_size, "</scale>\n",
          "        <Icon><href>", point_symbol, "</href></Icon>\n",
          "      </IconStyle>\n",
          "      <LabelStyle><scale>0</scale></LabelStyle>\n",
          "    </Style>"
        )
      },
      character(1)
    )
    
    # Point placemarks
    point_placemarks <- vapply(
      number,
      function(i) {
        paste0(
          "    <Placemark>\n",
          "      <name>", point_names[i], "</name>\n",
          "      <description><![CDATA[",
          labels[i],
          "]]></description>\n",
          "      <styleUrl>#point_", i, "</styleUrl>\n",
          "      <Point><coordinates>",
          format(data$lon[i], scientific = FALSE, trim = TRUE),
          ",",
          format(data$lat[i], scientific = FALSE, trim = TRUE),
          ",0</coordinates></Point>\n",
          "    </Placemark>"
        )
      },
      character(1)
    )
    
    # Track
    if (!is.null(line_colour) && total > 1L) {
      route_style <- paste0(
        '    <Style id="route">\n',
        "      <LineStyle>\n",
        "        <color>", line_colour, "</color>\n",
        "        <width>", line_size, "</width>\n",
        "      </LineStyle>\n",
        "    </Style>"
      )
      
      route_coordinates <- paste(
        paste0(
          format(data$lon, scientific = FALSE, trim = TRUE),
          ",",
          format(data$lat, scientific = FALSE, trim = TRUE),
          ",0"
        ),
        collapse = " "
      )
      
      route_placemark <- paste0(
        "    <Placemark>\n",
        "      <name>", track_name, " Track</name>\n",
        "      <styleUrl>#route</styleUrl>\n",
        "      <LineString>\n",
        "        <tessellate>1</tessellate>\n",
        "        <coordinates>",
        route_coordinates,
        "</coordinates>\n",
        "      </LineString>\n",
        "    </Placemark>"
      )
      
    } else {
      route_style <- route_placemark <- character()
    }
    
    # Assemble and compress KML
    kml <- paste(
      '<?xml version="1.0" encoding="UTF-8"?>',
      '<kml xmlns="http://www.opengis.net/kml/2.2">',
      "  <Document>",
      paste(point_styles, collapse = "\n"),
      route_style,
      paste(point_placemarks, collapse = "\n"),
      route_placemark,
      "  </Document>",
      "</kml>",
      sep = "\n"
    )
    
    writeLines(
      kml,
      kml_file,
      useBytes = TRUE
    )
    
    zip::zipr(
      zipfile = kmz_file,
      files = "doc.kml",
      root = temp_dir
    )
    
    kmz_file
  }
  
  # Generate one combined KMZ
  if (is.null(split_by)) {
    if (!is.null(file_name) && length(file_name) != 1L) {
      stop(
        "When `split_by = NULL`, `file_name` must contain one name.",
        call. = FALSE
      )
    }
    
    output_name <- if (is.null(file_name)) {
      "locations"
    } else {
      file_name
    }
    
    output_files <- write_kmz(
      data = sdata,
      track_name = safe_name(output_name),
      output_name = output_name
    )
    
    # Generate one KMZ per split value
  } else {
    split_values <- unique(
      as.character(sdata[[split_by]])
    )
    
    if (
      !is.null(file_name) &&
      append_file_name &&
      length(file_name) != 1L
    ) {
      stop(
        "When `append_file_name = TRUE`, `file_name` must contain one value.",
        call. = FALSE
      )
    }
    
    if (
      !is.null(file_name) &&
      !append_file_name &&
      length(file_name) != length(split_values)
    ) {
      stop(
        "When `append_file_name = FALSE`, `file_name` must contain ",
        length(split_values),
        " names.",
        call. = FALSE
      )
    }
    
    if (
      !is.null(file_name) &&
      !append_file_name &&
      !is.null(names(file_name)) &&
      !all(split_values %in% names(file_name))
    ) {
      stop(
        "Named `file_name` values must match all values in `split_by`.",
        call. = FALSE
      )
    }
    
    output_files <- vapply(
      seq_along(split_values),
      function(i) {
        value <- split_values[i]
        
        data <- sdata[
          as.character(sdata[[split_by]]) == value,
          ,
          drop = FALSE
        ]
        
        output_name <- if (is.null(file_name)) {
          value
        } else if (append_file_name) {
          paste0(value, "_", safe_name(file_name))
        } else if (is.null(names(file_name))) {
          file_name[i]
        } else {
          file_name[[value]]
        }
        
        write_kmz(
          data = data,
          track_name = value,
          output_name = output_name
        )
      },
      character(1)
    )
  }
  
  message(
    length(output_files),
    " KMZ file",
    if (length(output_files) == 1L) "" else "s",
    " written to ",
    normalizePath(output_dir, winslash = "/")
  )
  
  invisible(unname(output_files))
}
