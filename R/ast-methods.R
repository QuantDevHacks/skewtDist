#' @title Methods for ast class
#'
#' @description Methods for ast S3 class
#'
#' @param object A AST fit object of class \code{\link{ast}}
#' @param x A AST fit object of class \code{\link{ast}}
#' @param method one of "numerical" and "analytical", calculating the moments using numerical integration / analytical formula
#' @param type one of "density" and "QQplot"
#' @param y not used
#' @param dist the distribution (either normal or ast)
#' @param envelope confidence interval
#' @param ... additional arguments passed to the underlying method
#'
#' @details should also add the empirical moments
#'
#' @name ast-methods
#' @aliases summary.ast
#' @aliases moments.ast
#' @aliases plots.ast
#'
#' @examples
#' pars <- c(0.12, 0.6, 0.6, 6, 5)
#' data <- rast(1000, pars = pars)
#' solver_control <- list(eval.max = 10^3, iter.max = 10^3)
#' fit <- astMLE(data, solver = 'nlminb', solver_control = solver_control)
#' summary(fit)
#' moments(fit)
#' plot(fit, type = "density")

#' @rdname ast-methods
#' @export
summary.ast <- function(object, ...) {
  dist <- ifelse(object$symmetric == TRUE, "SST", "AST")
  pars <- rbind(object$start_pars, object$fixed_pars)
  res <- rbind(object$fitted_pars, object$standard_errors)
  colnames(pars) <- colnames(res) <- names(object$fitted_pars)
  rownames(pars) <- c("start_pars", "fixed_pars")
  rownames(res) <- c("fitted_pars", "standard_errors")

  cat("Distribution: ", dist, "\n")
  cat("Observations: ", length(object$data), "\n")
  cat("\nResult:\n")
  print(res)
  cat("\nLog-likelihood", object$objective)
  cat("\n\nSolver: ", object$solver)
  cat("\n\n")
  print(pars)
  cat("\nTime elapsed: ", object$time_elapsed)
  cat("\nConvergence Message: ", object$message)
}

#' @rdname ast-methods
#' @export
moments.ast <- function(x, method = c("analytical", "numerical"), ...) {
  pars <- x$fitted_pars
  astMoments(pars = pars, method)
}

#' @rdname ast-methods
#' @export
print.ast <- function(x, ...) {
  dist <- ifelse(x$symmetric == TRUE, "SST", "AST")
  res <- rbind(x$fitted_pars, x$standard_errors)
  colnames(res) <- names(x$fitted_pars)
  rownames(res) <- c("fitted_pars", "standard_errors")

  cat("Distrifitbution: ", dist, "\n")
  cat("Observations: ", length(x$data), "\n")
  cat("\nResult:\n")
  print(res)
}

#' @rdname ast-methods
#' @export
plot.ast <- function(x, y = NULL, type = NULL, dist = "ast", envelope = 0.95, ...) {
  if (is.null(type)) {
    selection <- 1
    while (selection) {
      selection <- menu(c("Density", "qqplot"), title = "Make a plot selection (or 0 to exit)")
      if (selection == 1) {
        density_ast(x, ...)
      } else if(selection == 2) {
        qqplot_ast(x, dist, envelope = 0.95, ...)
      }
    }
  } else {
    if (type == "density") {
      density_ast(x, ...)
    } else if(type == "qqplot") {
      qqplot_ast(x, dist = dist, envelope = envelope, ...)
    }
  }
}
