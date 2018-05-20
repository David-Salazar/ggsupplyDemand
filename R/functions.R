
# ggsupplyDemand ----------------------------------------------------------


#' @title Create System of Supply and Demand
#' @description Creates a tibble to represent a system of linear equations for supply and demand.
#' @param qd exogenous demand.
#' @param qs exogenous supply. Default: 250
#' @param a Demand's sensitivity to the price. Default: 10
#' @param b Supply's sensitivity to the price. Default: 5
#' @return A tibble that represents a system of linear equations for supply and demand.
#' @details Use it to create a tibble and then use it with plot_system_and_demand.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  create_supply_and_demand() %>%
#'  plot_supply_and_demand()
#'  }
#' }
#' @export
#' @rdname create_supply_and_demand
#' @seealso
#'  \code{\link[Hmisc]{bezier}}
#'  \code{\link[tibble]{as_data_frame}}
#'  \code{\link[dplyr]{rename}},\code{\link[dplyr]{left_join}}
#' @importFrom Hmisc bezier
#' @importFrom tibble as_data_frame
#' @importFrom magrittr "%>%"
#' @importFrom dplyr rename left_join
create_supply_and_demand <- function(qd = 1000,
                                     qs = 250,
                                     a = 10,
                                     b = 5) {

  x <- seq(0, qs + b*2 * (qd - qs)/(a + b), length.out = 4)

  demand <- Hmisc::bezier(x,
                          (qd - x)/a) %>%
    tibble::as_data_frame() %>%
    dplyr::rename(demand = y)

  supply <- Hmisc::bezier(x,
                          (x - qs)/b) %>%
    tibble::as_data_frame() %>%
    dplyr::rename(supply = y)

  dplyr::left_join(demand, supply, by = "x")
}



#' @title Shift Demand
#' @description Takes a tibble that represents a system of supply and demand and
#'   creates another curve by shifting one of the already existing demands.
#' @param supply_and_demand_system A tibble created from \code{create_supply_and_demand()}
#' @param outwards Whether you want to shift the demand outwards or not. Default: TRUE
#' @param curve Which of the existing demands you want to shift: i.e., shift a demand already shifted \code{demand1}, Default: demand
#' @return A tibble with an extra demand curve resulting from the shifting.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  create_supply_and_demand() %>%
#'  shift_demand() %>%
#'  shift_demand(demand = demand1) %>%
#'  plot_supply_and_demand()
#'  }
#' }
#' @export
#' @rdname shift_demand
#' @seealso
#'  \code{\link[rlang]{enquo}}
#' @import rlang
shift_demand <- function(supply_and_demand_system, outwards = TRUE, curve = demand,
                         shifter = 1000/4,
                         qd = 1000,
                         qs = 250,
                         a = 10,
                         b = 5) {
  curve <- rlang::enquo(curve)

  x <- seq(0, qs + b*2 * (qd - qs)/(a + b), length.out = 4)

  if (outwards) {
    plus_minus = 1
  } else {
    plus_minus = -1
  }

  demands <- Hmisc::bezier(x,
                          ((qd + plus_minus * shifter) - x)/a) %>%
    tibble::as_data_frame() %>%
    dplyr::rename(demand = y)

  col_names <- colnames(supply_and_demand_system)
  number_of_demands <- length(col_names[stringr::str_detect(col_names, "demand")])
  name <- paste0("demand", number_of_demands)
  supply_and_demand_system %>%
    dplyr::mutate(!! name := c(demands$demand))

}

#' @title Shift Supply
#' @description Takes a tibble that represents a system of supply and demand and
#'   creates another curve by shifting one of the already existing supply curves.
#' @param supply_and_demand_system A tibble created from \code{create_supply_and_demand()}
#' @param outwards Whether you want to shift the demand outwards or not. Default: TRUE
#' @param curve Which of the existing demands you want to shift: i.e., shift a demand already shifted \code{demand1}, Default: demand
#' @return A tibble with an extra demand curve resulting from the shifting.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  create_supply_and_demand() %>%
#'  shift_demand() %>%
#'  shift_supply() %>%
#'  plot_supply_and_demand()
#'  }
#' }
#' @export
#' @rdname shift_supply
#' @seealso
#'  \code{\link[rlang]{enquo}}
#' @import rlang
shift_supply <- function(supply_and_demand_system, outwards = TRUE, curve = supply,
                         shifter = 250/2,
                         qd = 1000,
                         qs = 250,
                         a = 10,
                         b = 5) {
  curve <- rlang::enquo(curve)
  x <- seq(0, qs + b*2 * (qd - qs)/(a + b), length.out = 4)
  if (outwards) {
    plus_minus = 1
  } else {
    plus_minus = -1
  }

  supplys <- Hmisc::bezier(x,
                         (x - (qs + plus_minus * shifter))/b) %>%
    tibble::as_data_frame() %>%
    dplyr::rename(supply = y)

  col_names <- colnames(supply_and_demand_system)
  number_of_supplies <- length(col_names[stringr::str_detect(col_names, "supply")])
  name <- paste0("supply", number_of_supplies)
  supply_and_demand_system %>%
    dplyr::mutate(!! name := c(supplys$supply))

}


# Find intersections ------------------------------------------------------

#' @title Find Intersection
#' @description Given a dataframe with two curves (first the x) and then the output of functions, it approximates the intersection between the functions.
#' @param df A matrix with the curves approximated numerically.
#' @return A tibble with the point of intersection between the curves.
#' @details This function was taken directly from https://www.andrewheiss.com/blog/2017/09/15/create-supply-and-demand-economics-curves-with-ggplot2/
#' @rdname curve_intersect_two

curve_intersect_two <- function(df) {
  df <- df[apply(df, 1, min) >= 0,,drop=FALSE]
  # Approximate the functional form of both curves
  curve1_f <- approxfun(df[,1], df[,2], rule = 2)
  curve2_f <- approxfun(df[,1], df[,3], rule = 2)

  # Calculate the intersection of curve 1 and curve 2 along the x-axis
  point_x <- uniroot(function(x) curve1_f(x) - curve2_f(x),
                     c(min(df[,1]), max(df[,1])))$root

  # Find where point_x is in curve 2
  point_y <- curve2_f(point_x)

  # All done!
  return(list(x = point_x, y = point_y))
}

#' @title Find Curve Intersections
#' @description Given a tibble representing a system of supply and demand, find
#'   all the intersections between the supply curves and demand curves.
#' @param supply_and_demand_system A tibble that represents a system of linear equations for supply and demand.
#' @return A tibble with the respective curves and their intersection.
#' @rdname find_all_intersections
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{bind_rows}}
#' @importFrom dplyr select bind_rows
#' @importFrom magrittr "%>%"
#' @importFrom dplyr starts_with
find_all_intersections <- function(supply_and_demand_system) {

  x <- supply_and_demand_system$x

  supply_curves <- supply_and_demand_system %>%
    dplyr::select(dplyr::starts_with("supply"))


  demand_curves <- supply_and_demand_system %>%
    dplyr::select(dplyr::starts_with("demand"))

  map(supply_curves, function(y) for_each(y, demand_curves, x)) %>%
    dplyr::bind_rows(.id = "id")



}

#' @title Ancilliary Function
#' @description FUNCTION_DESCRIPTION
#' @param curve
#' @param other_set_curves
#' @param x
#' @return
#'
#' @importFrom purrr map
#' @import dplyr
#' @importFrom magrittr "%>%"
for_each <- function(curve, other_set_curves, x) {

  purrr::map(other_set_curves, function(y) cbind(x, curve, y)) %>%
    purrr::map(curve_intersect_two) %>%
    dplyr::bind_rows(.id = "id2")
}


# Plot lines --------------------------------------------------------------

#' @title Plot Supply and Demand
#' @description Given a tibble representing a system of supply and demand, it plots all of the curves.
#' @param supply_and_demand_system A tibble that represents a system of linear equations for supply and demand created with \code{create_supply_and_demand()}.
#' @param consumer_surplus Whether you want to plot the consumer surplus or not. Default: TRUE
#' @return A ggplot2 plot.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  create_supply_and_demand() %>%
#'  shift_demand() %>%
#'  shift_supply() %>%
#'  plot_supply_and_demand()
#'  }
#' }
#' @export
#' @rdname plot_supply_and_demand
#' @seealso
#'  \code{\link[tidyr]{gather}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}}
#'  \code{\link[purrr]{map}}
#' @importFrom tidyr gather
#' @importFrom dplyr mutate filter
#' @importFrom purrr map
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_detect
plot_supply_and_demand <- function(supply_and_demand_system,
                        consumer_surplus = TRUE) {

  intersection <- find_all_intersections(supply_and_demand_system = supply_and_demand_system)

  data <- supply_and_demand_system %>%
    tidyr::gather(key = "curve", value = y, -x) %>%
    dplyr::mutate(var = ifelse(stringr::str_detect(curve, "supply"), "supply", "demand"))

  supply <- data %>% dplyr::filter(curve == "supply")
  demand <- data %>% dplyr::filter(curve == "demand")

  supplies <- data %>% dplyr::filter(var == "supply", curve != "supply" )
  demands <- data %>% dplyr::filter(var == "demand", curve != "demand")
  g <- ggplot(mapping = aes(x = x, y = y)) +
    geom_path(data = supply, mapping = aes(x = x, y = y), color = "#FF4036") +
    geom_path(data = supplies, mapping = aes(x = x, y = y, group = curve), color = "#FF4036",
              linetype = 2) +
    geom_path(data = demands, mapping = aes(x = x, y = y, group = curve), color = "#0073D9",
              linetype = 2) +
    geom_path(data = demand, mapping = aes(x = x, y = y), color = "#0073D9") +
    geom_segment(data = intersection,
                 aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
    geom_segment(data = intersection,
                 aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") +
    geom_point(data = intersection, aes(y = y), size = 3) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA),
                       breaks = intersection$x, labels = round(intersection$x,0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                       breaks = intersection$y, labels = round(intersection$y,0)) +
    theme_classic() +
    labs(x = "Quantity",
         y = "Price")

  if (consumer_surplus) {
    demandas <- data %>% filter(var == "demand") %>% pull(curve) %>% unique()
    ofertas <- data %>% filter(var == "supply") %>% pull(curve) %>% unique()

    #
    # map(demandas, function(x, y) add_geom_consumer(data, supply = ofertas, demand = x,
    #                                                intersection = intersection,
    #                                                supply_and_demand_system))

    # cs <- map(ofertas, function(x, y) add_geom_consumer(data, supply = x, demand = "demand",
    #                                                  intersection = intersection,
    #                                                  supply_and_demand_system))

    cs <- purrr::map(ofertas, function(x) purrr::map(demandas, function(y) add_geom_consumer(data, supply = x, demand = y,
                                                                               intersection = intersection,
                                                                               supply_and_demand_system)))

    # g$layers <- list(g$layers, cd) %>% unlist()
    g$layers <- list(g$layers, cs) %>% unlist()
  }

  g

}
#' @title Ancilliary Function
#' @description
#'
#' @importFrom purrr map
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr "%>%"
add_geom_consumer <- function(data, supply, demand, intersection, supply_and_demand_system) {

  relevant_intersection <- find_all_intersections(supply_and_demand_system)%>%
    filter(id == supply,
           id2 == demand)
  datos <- data %>% filter(x <= relevant_intersection$x,
                           curve %in% c(supply, demand)) %>%
    select(-var) %>%
    spread(key = "curve", value = y)


  datos$min <- apply(datos[, c(supply, demand)], 1, min)
  datos <- datos %>% mutate(min = ifelse(min < relevant_intersection$y, relevant_intersection$y, min))
  datos$max <- apply(datos[, c(supply, demand)], 1, max)

  datos <- datos %>% gather(key = "curve", value = "y", -c(x, min, max))

  geom_ribbon(data = datos, mapping = aes(x = x, ymax = max, ymin = min),
              fill = "grey70", alpha = 3/10, inherit.aes = FALSE)

}

