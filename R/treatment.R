#' Create a treatment data.frame from registry
#' 
#' @param treatment_definition definition of treatment file
#' @param ocip_filter filtering to be applied (optional)
#' @param day_of_month day of month to be considered as a facturation date (default=28)
#' @return a data.frame containing the facturations
#' 
#' @export
treatment.build = function(treatment_definition, ocip_filter = NULL, day_of_month = 28){
  treatment = sisap.read_file(treatment_definition, vars = c('ocip', 'date', 'pfc', 'env'))
  if(!is.null(ocip_filter)){
    treatment = treatment %>% subset(ocip %in% ocip_filter)
  }
  treatment %>% 
    mutate(date = as.Date(sprintf("%s%02d", date, day_of_month), format = '%Y%m%d')) %>%
    left_join(catalog %>% select(pfc, pf.units), by = 'pfc')
}

#' Create a treatment data.frame with a treatment from registry
#' 
#' @param df.treatment data.frame with treatment 
#' @param months_effect months effect
#' @param include_env consider the number of containers in the effect
#' @return a data.frame containing the facturations
#' 
#' @export
treatment.periods = function(df.treatment, months_effect = 6, include_env = TRUE){
  if(include_env){
    df.treatment = df.treatment %>%
      mutate(date.effect = date + 30 * months_effect + env * ifelse(is.na(pf.units), 28, pf.units))
  }else{
    df.treatment = df.treatment %>%
      mutate(date.effect = date + 30 *  months_effect)
  }
  df.treatment = df.treatment %>% 
    group_by(ocip) %>%
    arrange(date, date.effect)
  dbeg = df.treatment %>%
    mutate(date.effect.prev = lag(date.effect)) %>%
    subset( is.na(date.effect.prev) | date > date.effect.prev) %>%
    arrange(ocip, date)
  dend = df.treatment %>%
    mutate(date.next = lead(date)) %>%
    subset( is.na(date.next) | date.next > date.effect) %>%
    arrange(ocip, date)
  bind_cols(dbeg %>% select(ocip, date), dend %>% ungroup %>% select(date.effect))
}

#' @export
treatment.beginnings = function(df.treatment, months_effect = 6, include_env = TRUE){
  if(include_env){
    df.treatment = df.treatment %>%
      mutate(date.effect = date + 30 * months_effect + env * ifelse(is.na(pf.units), 28, pf.units))
  }else{
    df.treatment = df.treatment %>%
      mutate(date.effect = date + 30 *  months_effect)
  }
  df.treatment %>% 
    group_by(ocip) %>%
    arrange(date, date.effect) %>%
    mutate(date.effect.prev = lag(date.effect)) %>%
    subset( is.na(date.effect.prev) | date > date.effect.prev) %>%
    arrange(ocip) %>%
    select(-date.effect.prev)
}

#' @export
treatment.endings = function(df.treatment, months_effect = 6, include_env = TRUE){
  if(include_env){
    df.treatment = df.treatment %>%
      mutate(date.effect = date + 30 * months_effect + env * ifelse(is.na(pf.units), 28, pf.units))
  }else{
    df.treatment = df.treatment %>%
      mutate(date.effect = date + 30 *  months_effect)
  }
  df.treatment %>%
    group_by(ocip) %>%
    arrange(date, date.effect) %>%
    mutate(date.next = lead(date)) %>%
    subset( is.na(date.next) | date.next > date.effect) %>%
    arrange(ocip) %>%
    select(-date.next)
}
