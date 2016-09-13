#' an interactive add in to help visualise your data and chosen model
#'
#' an interactive add in to help visualise your data and chosen model
#'
#' @export

nlsView <- function(model, data, groups, param_bds){

  # create model and parameters
  formula <- stats::as.formula(model)
  params_ind <- all.vars(formula[[3]])[all.vars(formula[[3]]) %in% colnames(data)]
  params_est <- all.vars(formula[[3]])[! all.vars(formula[[3]]) %in% colnames(data)]
  param_response <- as.character(formula[[2]])

  # create  parameter boundaries
  params_bds <- data.frame(param = params_est, stringsAsFactors = FALSE)
  for(i in 1:nrow(params_bds)){
    params_bds$low.bds[i] <- param_bds[(2*i)-1]
    params_bds$high.bds[i] <- param_bds[2*i]
    params_bds$mean[i] <- (params_bds$low.bds[i] + params_bds$high.bds[i])/2
  }

  # create means dataset
  if(groups = NULL){
    means <- dplyr::group_by_(data, params_ind) %>%
    dplyr::summarise_(., mean = lazyeval::interp(~mean(var), var = as.name(param_response)))
    # create new smooth x value
    model.data <- data.frame(x = seq(min(means[,params_ind], na.rm = T), max(means[,params_ind], na.rm = T), length.out = 250))
    colnames(model.data) <- params_ind
    # evaluate model with predictions

    model.data[, param_response] = eval(formula[[3]], envir = c(tidyr::spread(params_bds[,c(1, 4)], key = param, value = mean), model.data[,params_ind, drop = F]))

  }
  else{
    grp_cols <- c(groups, params_ind)
    dots <- lapply(grp_cols, as.symbol)
    means <- dplyr::group_by_(data, .dots = dots) %>%
    dplyr::summarise_(., mean = lazyeval::interp(~mean(var), var = as.name(param_response)))}




}
