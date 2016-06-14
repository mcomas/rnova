safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes))

#' Create a population dataset from the information contained in population file and cmbd file (optional)
#' 
#' @param min_intro minimum entrance allowed
#' @param max_intro maximum entrance allowed
#' @param population_definition definition of population file
#' @param cmbd_definition definition of cmbd file
#' @return population object
#' 
#' @export
population.build = function(minimum_intro, maximum_intro, population_definition, cmbd_definition = NULL){
  population = sisap.read_file(population_definition, vars = c('ocip', 'dbirth', 'dsit', 'sit'))
  if(!is.null(cmbd_definition)){
    cmbd.death = sisap.read_file(cmbd_def, vars = c('ocip', 'd_alta', 'c_alta')) %>% 
      subset(c_alta == 6)%>%
      dplyr::group_by(ocip) %>%
      dplyr::summarise(ddeath = first(d_alta))
    population = population %>%
      dplyr::left_join(cmbd.death, by='ocip') %>%
      dplyr::mutate(
        ddeath = safe.ifelse(sit == 'D' & is.na(ddeath), dsit, ddeath),
        dtrans = safe.ifelse(sit == 'T' & is.na(ddeath), dsit, NA)) %>%
      dplyr::select(ocip, dbirth, ddeath, dtrans)
  }else{
    population = population %>%
      dplyr::mutate(
        ddeath = safe.ifelse(sit == 'D', dsit, NA),
        dtrans = safe.ifelse(sit == 'T', dsit, NA)) %>%
      dplyr::select(ocip, dbirth, ddeath, dtrans)
  }
  population = population %>%
    mutate(
      min.intro = minimum_intro,
      max.intro = maximum_intro,
      max.intro = pmin(ddeath-1, max.intro, na.rm=TRUE),
      max.intro = pmin(dtrans-1, max.intro, na.rm=TRUE))
  population = population %>%
    subset(min.intro <= max.intro)
  return(population)
}

#' Population filtered according to age filter
#' 
#' @param population population object
#' @param min_age minimum age allowed
#' @param max_age maximum age allowed
#' @return population object
#' 
#' @export
population.filter_age = function(population, min_age = -Inf, max_age = Inf){
  population %>%
    dplyr::mutate(
      min.intro = pmax(dbirth + floor(min_age * 365.25), min.intro),
      max.intro = pmin(dbirth + floor(max_age * 365.25), max.intro)) %>%
    subset(min.intro <= max.intro)
}
