#' Loops through a non-linear model on many different curves to find the best
#' possible estimates
#'
#' Finds the best estimated model to each of a set of curves using non-linear
#' least squares regression using nlsLM(). Start parameters are found using nls2
#'
#' @param data a data frame containing an column to differentiate between curves and the response and predictor variables.
#' @param model a non-linear model formula, with the response on the left of a ~ operator and an expression involving parameters on the right.
#' @param tries number of combinations of starting parameters that
#' are tried on each curve.
#' @param id_col the column name that identifies each curve that is to be
#' fitted in "".
#' @param param_bds lower and upper boundaries for the start parameters. If
#' missing these default to +/- 1e+09. Need to specified as a vector as :
#' c(lower bound param 1, upper bound param 1, lower bound param 2, upper bound
#' param 2 etc)
#' @param r2 whether or not the quasi rsquared value is calculated and returned.
#' This defaults to no, to include the r2 values use \code{r2 = 'Y'}.
#' @param supp_errors if \code{supp_errors = 'Y'}, then no error messages will be shown
#' from the nlsLM function, reducing the number of error messages
#' printed while the model attempts to converge using poor starting parameters. Advised to only use \code{supp_errors = 'Y'} when you are confident in the bounds of your starting parameters.
#' @param AICc whether or not the small sample AIC should be used. Defaults to
#' \code{'Y'}. Override this using \code{AICc == 'N'}. AICc should be used instead of AIC
#' when sample size is small in comparison to the number of estimated
#' parameters (Burnham & Anderson 2002 recommend its use when n / n_param < 40).
#' @param control specific control can be specified using \code{\link[minpack.lm]{nls.lm.control}}.
#' @param return_preds whether predictions of each individual model fit are wanted alongside the parameters or. Defaults to yes ('Y'). Add 'N' to change to no.
#' @param alg the algorithm with which to scan for start parameters. See \code{\link[nls2]{nls2}} for details. Defaults to \code{plinear-random}.
#' @param \dots Extra arguments to pass to \code{\link[minpack.lm]{nlsLM}} if necessary.
#' @return returns a list of class \code{nlsLoop}. Notable elements within the list are \code{$params} and \code{$predictions} that give the best fit parameters and predictions based on these parameters for each individual fit.
#' @note Useful additional arguments for \code{\link[minpack.lm]{nlsLM}} include: \code{na.action = na.omit},
#' \code{lower/upper = c()} where these represent upper and lower boundaries for
#' parameter estimates
#' @author Daniel Padfield
#' @seealso \code{\link[nlsLoop]{quasi_rsq_nls}} for details on the calculation of r squared values for non linear models.
#'
#' \code{\link[minpack.lm]{nlsLM}} for details on additional arguments to pass to the nlsLM function.
#'
#' \code{\link[MuMIn]{AICc}} for application of AICc.
#' @examples
#' # load in data
#'
#' data("Chlorella_TRC")
#' Chlorella_TRC_test <- Chlorella_TRC[Chlorella_TRC$curve_id %in% c(1:10),]
#'
#' # run nlsLoop()
#'
#'# define the Sharpe-Schoolfield equation
#' schoolfield_high <- function(lnc, E, Eh, Th, temp, Tc) {
#'  Tc <- 273.15 + Tc
#'  k <- 8.62e-5
#'  boltzmann.term <- lnc + log(exp(E/k*(1/Tc - 1/temp)))
#'  inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp))))
#'  return(boltzmann.term + inactivation.term)
#'}
#'
#' fits <- nlsLoop2(ln.rate ~ schoolfield_high(lnc, E, Eh, Th, temp = K, Tc = 20),
#'                 data = Chlorella_TRC_test,
#'                 tries = 500,
#'                 id_col = 'curve_id',
#'                 param_bds = c(-10, 10, 0.1, 2, 0.5, 5, 285, 330),
#'                 lower = c(lnc=-10, E=0, Eh=0, Th=0))
#'
#' @export

nlsLoop2 <-
  # arguments needed for nlsLoop ####
function(model, data, id_col, tries, param_bds, r2 = c('Y', 'N'), supp_errors = c('Y', 'N'), AICc = c('Y', 'N'), return_preds, control, alg, ...){

  # set default values
  if(missing(r2)){r2 <- 'N'}
  if(missing(supp_errors)){supp_errors <- 'N'}
  if(missing(AICc)){AICc <- 'Y'}
  if(missing(alg)){alg <- 'plinear-random'}
  if(missing(return_preds)){return_preds <- 'Y'}

  # checking whether MuMIn is installed
  if (!requireNamespace("MuMIn", quietly = TRUE)){
    stop("The MuMIn package is needed for calculation of AICc. Please install. Can be bypassed by using classic AIC using AICc = 'N'",
         call. = FALSE)
  }

  # create model ####
  formula <- stats::as.formula(model)
  params_ind <- all.vars(formula[[3]])[all.vars(formula[[3]]) %in% colnames(data)]
  params_est <- all.vars(formula[[3]])[! all.vars(formula[[3]]) %in% colnames(data)]

  if(missing(param_bds)){
    cat('No boundaries specified for the sought parameters. \n',
        'Default values of +/- 1e+10 will be used. This is likely to slow the process of finding the best model. \n')
    r <- readline("Continue with default values [y/n]? ")
    if(tolower(r) == "y") {
      param_bds <- rep(c(-Inf, Inf), times = length(params_est))
    }
    if(tolower(r) == "n"){
      stop('Please enter upper and lower parameter boundaries as param_bds in function argument.')
    }

  }

  params_bds = data.frame(matrix(param_bds, 2, length(params_est),
                                 dimnames=list(c(), params_est)),
                          stringsAsFactors=F)

  # nlsLM controls - this can stay the same, potential to be overridden
  if(missing(control)) {
    control <- minpack.lm::nls.lm.control(maxiter = 1000, ftol = .Machine$double.eps, ptol = .Machine$double.eps)
  }

  # create a unique id vector
  if(is.character(data[,id_col]) == F){data[,id_col] <- as.character(data[,id_col])}
  id <- unique(data[,id_col])

  # create a dataframe to output model results into ####
  res <- data.frame(id_col = id)
  res[,1] <- as.character(res[,1])
  res[,2:(ncol(params_bds) + 1)] <- 0
  colnames(res) <- c(id_col, colnames(params_bds))
  res$AIC <- 0
  res$quasi_r2 <- 0

  # fit nls model using LM optimisation and using shotgun approach to get starting values ####
  pb <- progress::progress_bar$new(total = length(id), clear  = FALSE)
  pb$tick(0)

  for (i in 1:length(id)){
    pb$tick()
    fit <- NULL
    fit.nls2 <- NULL
    data.fit <- data[data[,id_col] == id[i],]
    data.fit2 <- data.fit[!is.na(data.fit[,as.character(formula[[2]])]) & !is.na(data.fit[,params_ind]),]

    if(nrow(data.fit2) >= length(params_est)){
      try(fit.nls2 <- nls2::nls2(formula, data = data.fit2, start = params_bds, alg = alg, control = stats::nls.control(maxiter = tries)), silent = T)
    }

    if(!is.null(fit.nls2)){

      if(supp_errors == 'Y'){
        try(fit <- minpack.lm::nlsLM(formula,
                                   start=stats::coef(fit.nls2)[params_est],
                                   control = control,
                                   data=data.fit, ...),
          silent = TRUE)}
      if(supp_errors != 'Y'){
        try(fit <- minpack.lm::nlsLM(formula,
                                   start=stats::coef(fit.nls2)[params_est],
                                   control = control,
                                   data=data.fit, ...))}
    }

    # if it is the first fit of the model, output the results of the model in the dataframe
    # if the AIC score of the next fit model is < the AIC of the fit in the dataframe, replace
    if(AICc == 'N'){
      if(!is.null(fit) && res[i, 'AIC'] == 0 | !is.null(fit) && res[i, 'AIC'] > stats::AIC(fit)){
        res[i, 'AIC'] <- stats::AIC(fit)
        if(r2 == 'Y') {res[i, 'quasi_r2'] <- nlsLoop::quasi_rsq_nls(mdl = fit, y = data.fit[colnames(data.fit) == formula[[2]]], param = length(params_est))}
        for(k in 1:length(params_est)){
          res[i, params_est[k]] <- as.numeric(stats::coef(fit)[k])
        }
      }
    }

    else{

      if(!is.null(fit) && res[i, 'AIC'] == 0 | !is.null(fit) && res[i, 'AIC'] > MuMIn::AICc(fit)){

        res[i, 'AIC'] <- MuMIn::AICc(fit)
        if(r2 == 'Y') {res[i, 'quasi_r2'] <- nlsLoop::quasi_rsq_nls(mdl = fit, y = data.fit[colnames(data.fit) == formula[[2]]], param = length(params_est))}
        for(k in 1:length(params_est)){
          res[i, params_est[k]] <- as.numeric(stats::coef(fit)[k])
        }
      }
    }
  }
  # warnings for res ####
  if(r2 == 'N') {res <- res[,-grep('quasi_r2', colnames(res))]}
  if(r2 == 'Y'){warning('R squared values for non-linear models should be used with caution. See references in ?quasi_rsq_nls for details.', call. = F)}

  # delete fits that simply have not worked
  # change quasi_r2 and AIC special odd values to NA
  if(r2 == 'Y'){res[, 'quasi_r2'][which(is.nan(res[, 'quasi_r2']) | is.infinite(res[, 'quasi_r2']))] <- NA}
  res[, 'AIC'][which(is.nan(res[, 'AIC']) | is.infinite(res[, 'AIC']))] <- NA

  # only create predictions for curves that have parameter values are different from 0
  res_edit <- res[!(rowSums(res[,2:(length(params_est)+1)] == 0) > 0),]
  id_edit <- unique(res_edit[,id_col])

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

  # whether to return predictions
  if(return_preds == 'Y'){

    # cant do predictions for more than one independent variable
    if(length(params_ind) > 1){

      ### setting up a list return object
      val <- list(formula = formula, info = data.frame(id_col = id_col, params_ind = params_ind, param_dep = as.character(formula[[2]])), params = res)
    }
    else{

      preds <- plyr::ldply(split(res_edit, id_edit), predict.nlsLoop)
      preds <- preds[,c(3,2,4)]
      preds[,1] <- as.character(preds[,1])

      ### setting up a list return object
      val <- list(formula = formula, info = data.frame(id_col = id_col, params_ind = params_ind, param_dep = as.character(formula[[2]]), params_est = params_est, stringsAsFactors = FALSE), params = res, predictions = preds)
    }
  }
  else{

    ### setting up a list return object
    val <- list(formula = formula, info = data.frame(id_col = id_col, params_ind = params_ind, param_dep = as.character(formula[[2]]), stringsAsFactors = FALSE), params = res)
  }


  class(val) <- 'nlsLoop'
  return(val)

}
