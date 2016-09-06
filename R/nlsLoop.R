#########################################################
################### nlsLoop



# function


#' Loops through a non-linear model on many different curves to find the best
#' possible fit.
#'
#' Fits the best possible model to each of a set of curves using non-linear
#' least squares regression using nlsLM. The best fit is determined using AIC
#' scores.
#'
#' Returns a list of objects which includes a parameter dataframe of the best estimated parameter
#' fits for each level of `id_col` with associated AIC score.
#'
#' %% ~~ If necessary, more details than the description above ~~
#'
#' @param data Data that includes the rates
#' @param model The formula that is usually fed into nlsLM. Make sure there is
#' a y and an x (the column in your dataframe that is the explanatory variable)
#' eg. ln.resp.cor ~ schoolfield.high(ln.c, Ea, Eh, Th, temp=K, Tc = 25)
#' @param tries Number of combinations of different starting parameters that
#' are tried on each curve.
#' @param id_col The column name that identifies each curve that is to be
#' fitted. Needs to be in speech marks, ' '.
#' @param param_bds Upper and lower boundaries for the start parameters. If
#' missing these default to +/- 1e+09. Need to specified as a vector as :
#' c(lower bound param 1, upper bound param 1, lower bound param 2, upper bound
#' param 2 etc)
#' @param r2 Whether or not you want the quasi rsquared value to be returned.
#' This defaults to no, (so not specifying the argument or r2 = 'N' results in
#' no r2 values being returned), to include the r2 values use r2 = 'Y'
#' @param supp.errors If supp.errors = 'Y' then no error messages will be shown
#' from the actual nlsLM function, reducing the number of error messages
#' received while the model works through starting parameters from which the
#' model cannot converge. Advised to only be used once it is expected that
#' error messages in the nlsLM function are not important.
#' @param AICc Whether or not the small sample AIC should be used. Defaults to
#' 'Y'. Override this using AICc == 'N'. AICc should be used instead of AIC
#' when sample size is small in comparison to the number of estimated
#' parameters (Burnham & Anderson 2002 recommend its use when n / K < 40).
#' @param control If specific control arguments are desired they can be specified using nls.lm.control here
#' @param \dots Extra arguments to pass to nlsLM if necessary.
#' @return Returns a list of class nlsLoop. Notable elements within the list are $params and $predictions that give the best fit parameters and predictions based on these parameters.
#' @note Useful additional arguments for nlsLM include: na.action = na.omit,
#'
#' lower/upper = c() where these represent upper and lower boundaries for
#' parameter estimates
#' @author Daniel Padfield
#' \code{\link[nlsTools]{quasi.rsq.nls}} for details on the calculation of r squared
#' values for non linear models.
#' \code{\link[minpack.lm]{nlsLM}} for details on additional arguments to pass to the nlsLM function.
#' See AICc in the AICcmodavg package for application of AICc.
#' @examples
#'
#' @export nlsLoop


nlsLoop <-
  # arguments needed for nlsLoop ####
  function(model, data, tries, id_col, param_bds, r2 = 'N', supp.errors = 'N', AICc = 'Y', control,...){

    # checking whether MuMIn is installed
    if (!requireNamespace("MuMIn", quietly = TRUE)){
      stop("The MuMIn package is needed for calculation of AICc. Please install. Can be bypassed by using classic AIC using AICc = 'N'",
           call. = FALSE)
    }

  # create model ####
  formula <- stats::as.formula(model)

  # define parameters to estimate and independent variable ####
  params_ind <- all.vars(formula[[3]])[all.vars(formula[[3]]) %in% colnames(data)]
  params_est <- all.vars(formula[[3]])[! all.vars(formula[[3]]) %in% colnames(data)]

  # set up parameter boundaries ####
  params_bds <- data.frame(param = params_est, stringsAsFactors = FALSE)
  params_bds$low.bds <- NA
  params_bds$high.bds <- NA

  if(missing(param_bds)){
    cat('No boundaries specified for the sought parameters. \n',
        'Default values of +/- 1e+10 will be used. This is likely to slow the process of finding the best model. \n')
    r <- readline("Continue with default values [y/n]? ")
    if(tolower(r) == "y") {
      params_bds$low.bds <- 10^-10
      params_bds$high.bds <- 10^10
    }
    if(tolower(r) == "n"){
      stop('Please enter upper and lower parameter boundaries as param_bds in function argument.')
    }

  }

  else {

    for(i in 1:nrow(params_bds)){
      params_bds$low.bds[i] <- param_bds[(2*i)-1]
      params_bds$high.bds[i] <- param_bds[2*i]
    }

  }

  # nlsLM controls - this can stay the same, potential to be overridden
  if(missing(control)) {
    control <- minpack.lm::nls.lm.control(maxiter = 1000, ftol = .Machine$double.eps, ptol = .Machine$double.eps)
  }


  # set up start values ####
  make_strt_values <- function(x, tries){
    strt_values <- data.frame(param = rep(x, times = tries),
                              value = stats::runif(tries, min = params_bds$low.bds[params_bds$param == x], max = params_bds$high.bds[params_bds$param == x]))
    return(strt_values)
  }

  strt <- plyr::ldply(as.list(params_est), make_strt_values, tries)

  # create a unique id vector
  if(is.character(data[,id_col]) == F){data[,id_col] <- as.character(data[,id_col])}
  id <- unique(data[,id_col])

  # create a dataframe to output model results into ####
  res <- data.frame(id_col = id)
  res[,2:(nrow(params_bds) + 1)] <- 0
  colnames(res) <- c(id_col, params_bds$param)
  res$AIC <- 0
  res$quasi.r2 <- 0

  # fit nls model using LM optimisation and using shotgun approach to get starting values ####
  for (i in 1:length(id)){
    cat('\n', i, 'of', length(id), ':', id[i], '\n')
    fit <- NULL
    # subset the dataframe to fit the model for each unique curve by id
    data.fit <- data[data[,id_col] == id[i],]

    # set count to 0
    count <- 0

    for (j in 1:tries){
      if((j/10) %% 1 == 0){cat(j, ' ')}
      # create start list
      start.vals <- list()
      for(k in 1:length(params_est)){
        start.vals[[params_est[k]]] <- strt[strt$param == params_est[k],]$value[j]
      }
      # try and fit the model for every set of searching parameters
      if(supp.errors == 'Y'){
        try(fit <- minpack.lm::nlsLM(formula,
                       start=start.vals,
                       control = control,
                       data=data.fit, ...),
          silent = TRUE)}
      if(supp.errors != 'Y'){
          try(fit <- minpack.lm::nlsLM(formula,
                       start=start.vals,
                       control = control,
                       data=data.fit, ...))} #<----- Took out ...

      # if it is the first fit of the model, output the results of the model in the dataframe
      # if the AIC score of the next fit model is < the AIC of the fit in the dataframe, replace
      # the output to ensure the best model is selected
      if(AICc == 'N'){
        if(is.null(fit) == T){
          count <-  0
        }
        else{
          count <- ifelse(res[i, 'AIC'] <= stats::AIC(fit), count + 1, 0)
        }
        if(count == 100) break

        if(!is.null(fit) && res[i, 'AIC'] == 0 | !is.null(fit) && res[i, 'AIC'] > AIC(fit)){
        res[i, 'AIC'] <- AIC(fit)
        if(r2 == 'Y') {res[i, 'quasi.r2'] <- nlsTools::quasi.rsq.nls(mdl = fit, y = data.fit[colnames(data.fit) == formula[[2]]], param = length(params_est))}
        for(k in 1:length(params_est)){
          res[i, params_est[k]] <- as.numeric(coef(fit)[k])
        }
        }
      }

      else{

        if(is.null(fit) == T){
          count <-  0
        }
        else{
          count <- ifelse(res[i, 'AIC'] <= MuMIn::AICc(fit), count + 1, 0)
        }
        if(count == 100) break

        if(!is.null(fit) && res[i, 'AIC'] == 0 | !is.null(fit) && res[i, 'AIC'] > MuMIn::AICc(fit)){

        res[i, 'AIC'] <- MuMIn::AICc(fit)
        if(r2 == 'Y') {res[i, 'quasi.r2'] <- nlsTools::quasi.rsq.nls(mdl = fit, y = data.fit[colnames(data.fit) == formula[[2]]], param = length(params_est))}
        for(k in 1:length(params_est)){
          res[i, params_est[k]] <- as.numeric(coef(fit)[k])
        }
      }
    }
  }
}
  # warnings for res ####
  if(r2 == 'N') {res <- res[,-grep('quasi.r2', colnames(res))]}
  if(supp.errors == 'Y'){warning('Errors have been suppressed from nlsLM()', call. = F)}
  if(r2 == 'Y'){warning('R squared values for non-linear models should be used with caution. See references in ?quasi.r2 for details.', call. = F)}

  # creating a predict dataframe ####
  predict.nlsLoop <- function(x, data. = data, params_ind. = params_ind, formula. = formula, params_est. = params_est, id_col. = id_col){

    # subset results data frame to contain just estimated parameters
    est.param.val <- x[,names(x) %in% params_est.]

    # identify y variable name
    y <- as.character(formula.[[2]])

    # subset data to just be the x variable
    dat <- data.[data.[,id_col.] == x[,id_col.],]
    dat <- dat[! is.na(dat[,y]),]
    x2 <- dat[,names(dat) %in% params_ind., drop = F]

    # create a predictions data frame
    predict_id <- data.frame(expand.grid(params_ind. = seq(min(x2, na.rm = T), max(x2, na.rm = T), length.out = 250), id_col = x[,id_col.]))
    colnames(predict_id) <- c(params_ind., id_col.)

    predict_id[, y] = eval(formula.[[3]], envir = c(est.param.val, predict_id[,params_ind., drop = F]))

    return(predict_id)

  }

  preds <- plyr::ldply(split(res, id), predict.nlsLoop)
  preds <- subset(preds, select = - c(.id))

  ### setting up a list return object
  val <- list(formula = formula, info = data.frame(id_col = id_col, params_ind = params_ind, param_dep = as.character(formula[[2]])), params = res, predictions = preds)
  class(val) <- 'nlsLoop'
  return(val)

}
