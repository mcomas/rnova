#' Read a delimited sisap file
#' 
#' @param file_def file definition
#' @param vars variables of interest
#' @param ... other parameters passed to \code{\link{read_delim}}
#' @return returns a data.frame in \code{\link{tbl_df}} format
#' 
#' @export
sisap.read_file = function(file_def, vars = names(file_def$col_names), locale = readr::locale(date_format = "%Y%m%d"), ...){
  vnames = names(file_def$col_names)
  vtypes = file_def$col_names
  if( length(dv <- setdiff(vars , vnames)) > 0){
    stop(sprintf('%s not available', paste(dv, collapse = ', ')))
  }
  ind = sort(match(vars, vnames))
  col_types = rep('_', length(vnames))
  col_types[ind] = vtypes[ind]
  col_types = paste(col_types, collapse='')
  rf = readr::read_delim(file_def$file,file_def$delim, col_names = vnames[ind], 
                         col_types = col_types, locale = locale, ...)
  dplyr::select_(rf, .dots = vars)
}

#' Sisap file definition
#' 
#' @param filename path to the file
#' @param delim file delimiter
#' @param integer_vars variables to be considered integers
#' @param double_vars variables to be considered doubles
#' @param logical_vars variables to be considered logicals
#' @param date_vars variables to be considered dates
#' @return file definition
#' 
#' @export
sisap.file_definition = function(filename, delim = '@', col_names = NULL, 
                                 integer_vars = NULL, double_vars = NULL,
                                 logical_vars = NULL, date_vars = NULL,
                                 type = NULL, numcol = NULL){
  fr = readr::read_delim(filename, delim, n_max = 0, col_names = FALSE)
  if(!(is.null(type) & is.null(numcol))){
    if(is.null(numcol)){
      return(sisap.column_names(filename, delim, type, NCOL(fr)))
    }else{
      return(sisap.column_names(filename, delim, type, numcol))
    }
  }
  if(is.null(col_names)){
    cnames = rep('c', NCOL(fr))
    names(cnames) = names(fr)
  }else{
    if(length(col_names) != NCOL(fr)){
      stop(sprintf("Given length of the names (%d) needs to be equal to number of columns of the file (%d)", 
                   length(col_names), NCOL(fr)))
    }
    if(length(unique(col_names)) != length(col_names)){
      stop("Given names need to be different")
    }
    cnames = rep('c', NCOL(fr))
    names(cnames) = col_names
  }
  if( !is.null(integer_vars) ){
    if(!prod(integer_vars %in% names(cnames))){
      stop("Integer names should be contained in columns names")
    }
    cnames[integer_vars] = 'i'
  }
  if( !is.null(double_vars) ){
    if(!prod(double_vars %in% names(cnames))){
      stop("Double names should be contained in columns names")
    }
    cnames[double_vars] = 'd'
  }
  if( !is.null(logical_vars) ){
    if(!prod(logical_vars %in% names(cnames))){
      stop("Logical names should be contained in columns names")
    }
    cnames[logical_vars] = 'l'
  }
  if( !is.null(date_vars) ){
    if(!prod(date_vars %in% names(cnames))){
      stop("Date names should be contained in columns names")
    }
    cnames[date_vars] = 'D'
  }
  structure(list(
    'type' = NULL,
    'file' = filename,
    'delim' = delim,
    'col_names' = cnames),
    class='sisap.file')
}

sisap.column_names = function(filename, delim, type, numcol){
  if(type == 'population'){
    if(numcol == 12){
      return(sisap.file_definition(
        filename = filename, delim = delim, 
        col_names = c('ocip', 'sector', 'up', 'uba', 'dbirth', 'sex', 'nationality', 
                      'sit', 'dsit', 'fvis', 'lvis', 'code'),
        date_vars = c('dbirth', 'dsit', 'fvis', 'lvis')))
    }
    if(numcol == 8){
      return(sisap.file_definition(
        filename = filename, delim = delim, 
        col_names = c('ocip', 'sector', 'dbirth', 'sex', 'sit', 'dsit', 'fvis', 'lvis'),
        date_vars = c('dbirth', 'dsit', 'fvis', 'lvis')))
    }
  }
  if(type == 'cmbd'){
    if(numcol == 47){
      return(sisap.file_definition(
        filename = filename, delim = delim,
        col_names = c('ocip', 'projecte', 'motiu1', 'filtre', 'numero_id', 'up_assistencia', 'eap', 'sexe', 'd_naix', 'edat', 
                      'abs', 'residencia_a', 'd_ingres', 'c_ingres', 'd_alta', 'c_alta', 'up_desti', 'dies_est', 't_act', 'catsalut',
                      'dp', 'ds1', 'ds2', 'ds3', 'ds4', 'ds5', 'ds6', 'ds7', 'ds8', 'ds9', 
                      'ce1', 'ce2', 'ce3', 'ce4', 'ce5',
                      'pp', 'ps1', 'ps2', 'ps3', 'ps4', 'ps5', 'ps6', 'ps7', 'px1', 'px2',
                      'grd_ap_sensepx', 'any_'),
        date_vars = c('d_naix', 'd_ingres', 'd_alta'),
        integer_vars = c('c_ingres', 'c_alta', 'dies_est')))
    }
    if(numcol == 33){
      return(sisap.file_definition(
        filename = filename, delim = delim,
        col_names = c(
          'ocip', 'X', 'd_ingres', 'c_ingres', 'Pr_ingres', 'd_alta', 'c_alta', 'dp',
          'ds1', 'ds2', 'ds3', 'ds4', 'ds5', 'ds6', 'ds7', 'ds8', 'ds9', 'ce1', 'ce2', 'ce3', 'ce4', 'ce5',
          'pp', 'ps1', 'ps2', 'ps3', 'ps4', 'ps5', 'ps6', 'ps7', 'px1', 'px2', 'grd_ap_sensepx'),
        date_vars = c('d_ingres', 'd_alta'),
        integer_vars = c('c_ingres', 'c_alta')))
    }
    if(numcol == 35){
      return(sisap.file_definition(
        filename = filename, delim = delim,
        col_names = c(
          'ocip', 'up_assistencia', 'X', 'd_ingres', 'c_ingres', 'Pr_ingres', 'd_alta', 'c_alta', 'dp',
          'ds1', 'ds2', 'ds3', 'ds4', 'ds5', 'ds6', 'ds7', 'ds8', 'ds9', 'ce1', 'ce2', 'ce3', 'ce4', 'ce5',
          'pp', 'ps1', 'ps2', 'ps3', 'ps4', 'ps5', 'ps6', 'ps7', 'px1', 'px2', 'grd_ap_sensepx', 'any_'),
        date_vars = c('d_ingres', 'd_alta'),
        integer_vars = c('c_ingres', 'c_alta')))
    }
  }
  if(type == 'ecap'){
    if(numcol == 4){
      return(sisap.file_definition(
        filename = filename, delim = delim,
        col_names = c('ocip', 'icd', 'dalta', 'dbaixa'),
        date_vars = c('dalta', 'dbaixa')))
    }
  }
  if(type == 'treatment'){
    if(numcol == 11){
      return(sisap.file_definition(
        filename = filename, delim = delim,
        col_names = c('ocip', 'date', 'atc', 'pfc', 'env', 'ndd', 'pvp', 'liq', 'reg', 'tip', 'rce'),
        integer_vars = c('env'),
        double_vars = c('pvp', 'liq')))
    }
    if(numcol == 14){
      return(sisap.file_definition(
        filename = filename, delim = delim,
        col_names = c('ocip', 'upi', 'upa', 'col', 'atc', 'pfc', 'date', 'env', 'ndd', 'pvp', 'liq', 'reg', 'tip', 'rce'),
        integer_vars = c('env'),
        double_vars = c('pvp', 'liq')))
    }
    if(numcol == 6){
      return(sisap.file_definition(
        filename = filename, delim = delim,
        col_names = c('ocip', 'atc', 'pfc', 'date', 'env', 'pvp'),
        integer_vars = c('env'),
        double_vars = c('pvp')))
    }
  }
  if(type == 'medea'){
    if(numcol == 2){
      return(sisap.file_definition(
        filename = filename, delim = delim,
        col_names = c('ocip', 'medea')))
      
    }
  }
}
