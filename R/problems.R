#' Read a delimited sisap file
#' 
#' @param ecap_definition Definition of sisap ecap file
#' @param cmbd_definition Definition of sisap cmbd file
#' @param ocip_filter filtering to be applied (optional)
#' @return returns a data.frame in \code{\link{tbl_df}} format
#' 
#' @export
problems.build = function(ecap_definition = NULL, cmbd_definition = NULL, ocip_filter = NULL){
  if(is.null(ecap_definition) & is.null(cmbd_definition)){
    stop("Either ecap or cmbd definition is required")
  }
  if(!is.null(ecap_definition)){
    ecap = sisap.read_file(ecap_definition, vars = c('ocip', 'dalta', 'icd'))
    if(!is.null(ocip_filter)){
      ecap = ecap %>%
        subset(ocip %in% ocip_filter)
    }
    ecap = ecap %>%
      mutate(source = 'ecap', field = 'ecap') %>%
      select(ocip, date = dalta, source, field,  icd)
  }
  if(!is.null(cmbd_definition)){
    cmbd = sisap.read_file(cmbd_definition, vars = c('ocip', 'd_ingres', 'dp', paste0('ds',1:9), 'pp', paste0('ps',1:7)))
    if(!is.null(ocip_filter)){
      cmbd = cmbd %>%
        subset(ocip %in% ocip_filter)
    }
    cmbd = cmbd %>% 
      gather(key='field', value='icd', -ocip, -d_ingres) %>% na.omit %>%
      mutate(source = 'cmbd') %>%
      select(ocip, date = d_ingres, source, field, icd)
  }
  if(is.null(ecap_definition)){
    return(cmbd)
  }
  if(is.null(cmbd_definition)){
    return(ecap)
  }
  return(bind_rows(ecap, cmbd))
}

#' Get the date of the first event
#' 
#' @param df.disease data.frame containing the registry
#' @param xml_disease_file xml file containing the disease definition
#' @param diseases name of diseases of interest (must be contained in the xml file definition)
#' @param any logical indicating whether to calculate the overall minimum (TRUE) or the separated by disease (FALSE)
#' @param where_icd9 character indicating where to find the information inside column field
#' @param where_icd10 character indicating where to find the information inside column field
#' @return returns a data.frame with the date of the first event
#' 
#' @export
problems.first_event = function(df.disease, xml_disease_file, diseases, any = TRUE, 
                                where_icd9 = c('dp', paste0('ds',1:9)), where_icd10 = 'ecap'){
  f_xml = xmlParse(xml_disease_file)
  
  df.disease.ecap = df.disease %>% subset(field %in% where_icd10)
  df.disease.cmbd = df.disease %>% subset(field %in% where_icd9)
  
  icd10.probs = df.disease.ecap %>% { unique(.[['icd']])}
  icd9.probs = df.disease.cmbd %>% { unique(.[['icd']])}
  
  probs.icd10 = get_icd(disease = diseases, f_xml = f_xml, icd_list = icd10.probs, icd = 10, any = any)
  probs.icd9 = get_icd(disease = diseases, f_xml = f_xml, icd_list = icd9.probs, icd = 9, any = any)
  
  if(any){
    bind_rows(df.disease.ecap %>% subset(icd %in% probs.icd10),
              df.disease.cmbd %>% subset(icd %in% probs.icd9)) %>%
      group_by(ocip) %>%
      summarise(date = min(date))
  }else{
    lapply(diseases, function(disease){
      bind_rows(df.disease.ecap %>% subset(icd %in% probs.icd10[[disease]]),
                df.disease.cmbd %>% subset(icd %in% probs.icd9[[disease]])) %>%
        group_by(ocip) %>%
        summarise(disease = disease, date = min(date))
    }) %>% bind_rows
  }
}

#' Get the date of the first procedure
#' 
#' @param df.disease data.frame containing the registry
#' @param xml_disease_file xml file containing the disease definition
#' @param diseases name of diseases of interest (must be contained in the xml file definition)
#' @param any logical indicating whether to calculate the overall minimum (TRUE) or the separated by disease (FALSE)
#' @param where_icd9 character indicating where to find the information inside column field
#' @return returns a data.frame with the date of the first procedure
#' 
#' @export
problems.first_procedure = function(df.disease, xml_disease_file, procedures, any = TRUE, 
                                where_icd9 = c('pp', paste0('ps',1:7))){
  f_xml = xmlParse(xml_disease_file)
  
  df.disease.cmbd = df.disease %>% subset(field %in% where_icd9)
  
  icd9.procs = df.disease.cmbd %>% { unique(.[['icd']])}
  
  probs.icd9 = get_icd_proc(procedure = procedures, f_xml = f_xml, icd_list = icd9.procs, icd = 9, any = any)
  
  if(any){
    df.disease.cmbd %>% subset(icd %in% probs.icd9) %>%
      group_by(ocip) %>%
      summarise(date = min(date))
  }else{
    lapply(procedures, function(procedure){
      df.disease.ecap %>% subset(icd %in% probs.icd10[[procedure]]) %>%
        group_by(ocip) %>%
        summarise(procedure = procedure, date = min(date))
    }) %>% bind_rows
  }
}

get_unique_icd = function(disease, f_xml, icd_list, icd = 10){
  l_icd = c()
  meta_diseases_in = getNodeSet(f_xml, sprintf("/problems/disease[@name='%s']/meta[not(@action='exclude')]/text()", disease))
  if(length(meta_diseases_in)>0){
    meta_diseases_in_str = sapply(meta_diseases_in, function(el) xmlValue(el))
    l_icd = c(l_icd, unlist(sapply(meta_diseases_in_str, function(name_disease) get_unique_icd(name_disease, f_xml, icd_list, icd))))
  }
  icds_in = getNodeSet(f_xml, sprintf("/problems/disease[@name='%s']/icd[@edition='icd%d' and not(@action='exclude')]", disease, icd))
  if(length(icds_in) > 0){
    l_icd = c(l_icd, unlist(sapply(icds_in, function(el) icd_filter(icd_list, xmlValue(el)))))
  }
  icds_out = getNodeSet(f_xml, sprintf("/problems/disease[@name='%s']/icd[@edition='icd%d' and @action='exclude']", disease, icd))
  if(length(icds_out) > 0){
    l_icd = setdiff(l_icd, unlist(sapply(icds_out, function(el) icd_filter(icd_list, xmlValue(el)))))
  }
  meta_diseases_out = getNodeSet(f_xml, sprintf("/problems/disease[@name='%s']/meta[@action='exclude']/text()", disease))
  if(length(meta_diseases_out)>0){
    meta_diseases_out_str = sapply(meta_diseases_out, function(el) xmlValue(el))
    l_icd = setdiff(l_icd, unlist(sapply(meta_diseases_out_str, function(name_disease) get_unique_icd(name_disease, f_xml, icd_list, icd))))
  }
  unname(l_icd)
}


get_icd = function(disease, f_xml, icd_list, icd = 10, any = TRUE){
  res = lapply(disease, get_unique_icd, f_xml, icd_list, icd = icd)
  if(any){
    return( sort(unlist(res)) )
  }else{
    names(res) = disease
    return(res)
  }
}

get_unique_icd_proc = function(procedure, f_xml, icd_list, icd=9){
  l_icd = c()
  meta_procedures_in = getNodeSet(f_xml, sprintf("/problems/procedure[@name='%s']/meta[not(@action='exclude')]/text()", procedure))
  if(length(meta_procedures_in)>0){
    meta_procedures_in_str = sapply(meta_procedures_in, function(el) xmlValue(el))
    l_icd = c(l_icd, unlist(sapply(meta_procedures_in_str, function(name_procedure) get_icd_proc(name_procedure, f_xml, icd_list, icd))))
  }
  icds_in = getNodeSet(f_xml, sprintf("/problems/procedure[@name='%s']/icd[@edition='icd%d' and not(@action='exclude')]", procedure, icd))
  if(length(icds_in) > 0){
    l_icd = c(l_icd, unlist(sapply(icds_in, function(el) icd_filter(icd_list, xmlValue(el)))))
  }
  icds_out = getNodeSet(f_xml, sprintf("/problems/procedure[@name='%s']/icd[@edition='icd%d' and @action='exclude']", procedure, icd))
  if(length(icds_out) > 0){
    l_icd = setdiff(l_icd, unlist(sapply(icds_out, function(el) icd_filter(icd_list, xmlValue(el)))))
  }
  meta_procedures_out = getNodeSet(f_xml, sprintf("/problems/procedure[@name='%s']/meta[@action='exclude']/text()", procedure))
  if(length(meta_procedures_out)>0){
    meta_procedures_out_str = sapply(meta_procedures_out, function(el) xmlValue(el))
    l_icd = setdiff(l_icd, unlist(sapply(meta_procedures_out_str, function(name_procedure) get_icd(name_procedure, f_xml, icd_list, icd))))
  }
  l_icd
}

get_icd_proc = function(procedure, f_xml, icd_list, icd = 10, any = TRUE){
  res = lapply(procedure, get_unique_icd_proc, f_xml, icd_list, icd = icd)
  if(any){
    return( sort(unlist(res)) )
  }else{
    names(res) = procedure
    return(res)
  }
}

icd_filter = function(icd_list, icd_selection){
  
  sel = icd_list %in% icd_selection |
    sprintf("%s*", str_sub(icd_list, 1, 1)) %in% icd_selection |
    sprintf("%s*", str_sub(icd_list, 1, 2)) %in% icd_selection |
    sprintf("%s*", str_sub(icd_list, 1, 3)) %in% icd_selection |
    sprintf("%s*", str_sub(icd_list, 1, 4)) %in% icd_selection |
    sprintf("%s*", str_sub(icd_list, 1, 5)) %in% icd_selection
  icd_list[sel]
}