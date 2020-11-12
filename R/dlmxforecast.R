## ####################################################################

##' @description Forecast for dlm and dlmFiltered objects
##' @name forecast
##' 
##' @title Forecast for dlm and dlmFiltered objects
##' @param object dlm or dlmFiltered objects
##' @param nAhead Number of steps ahead
##' @param offset Where to start from
##' @return A list with a, R, f, Q and other values
##' @author Søren Højsgaard

## ####################################################################


#' @rdname forecast
#' @export
dlmXForecast <- function(object, nAhead=1, offset=0){
    UseMethod("dlmXForecast")    
}

#' @rdname forecast
#' @export
dlmXForecast.dlm <- function(object, nAhead=1, offset=0){
    cat("dlmXForecast.dlm\n")
    offset <- 0       ## Overwrite offset
    m.start <- object$m0
    C.start <- object$C0
    forecast_worker(object, m.start, C.start, nAhead, offset)
}

#' @rdname forecast
#' @export
dlmXForecast.dlmFiltered <- function(object, nAhead=1, offset=0){
    cat("dlmXForecast.dlmFiltered\n")
    if (offset == 0)
        return(dlmXForecast(object$mod, nAhead=nAhead, offset=offset))


    m.start <- mm(object)[offset + 1,]
    C.start <- CC(object)[[offset + 1]]        
    utils::str(list(m.start=m.start, nAhead=nAhead, offset=offset))

    forecast_worker(object$mod, m.start, C.start, nAhead, offset)
}

forecast_worker <- function(mod, m.start, C.start, nAhead, offset){
    p <- length(m.start)
    m <- nrow(mod$FF)
    
    a <- rbind(m.start, matrix(0, nAhead, p))  
    R <- vector("list", nAhead + 1)
    R[[1]] <- C.start 
    f <- matrix(0, nAhead, m)
    Q <- vector("list", nAhead)
    
    for (it in 1:nAhead) {
        GGG <- getGG(mod, offset + it)
        WWW <- getWW(mod, offset + it)
        FFF <- getFF(mod, offset + it)
        VVV <- getVV(mod, offset + it)
        a[it + 1, ] <- GGG %*% a[it, ]
        R[[it + 1]] <- GGG %*% R[[it]] %*% t(GGG) + WWW
        f[it, ] <- FFF %*% a[it + 1, ]
        Q[[it]] <- FFF %*% R[[it + 1]] %*% t(FFF) + VVV
    }
    a <- a[-1, , drop = FALSE]
    R <- R[-1]

    fore <- list(a=a, R=R, f=f, Q=Q, offset=offset, idx=offset + (1:nAhead))
    ## fore$a # forecasted states
    ## fore$R # forecasted state variances
    ## fore$f # forecasted observations
    ## fore$Q # forecasted observations variances

    class(fore) <- "dlmXForecast_class"
    fore
}



## if (is.character(select)){
##     if (identical(select, "last")) select <- NROW(object$m)
## }


#' @export
summary.dlmXForecast_class <- function(object, ...){

    Q.diag <- do.call(rbind, lapply(object$Q, diag))
    R.diag <- do.call(rbind, lapply(object$R, diag))
    
    object$Q <- Q.diag
    object$R <- R.diag
    names(object)[c(2,4)] <- c("R.diag", "Q.diag")
    
    object   
}

#' @importFrom stats confint
#' @export
confint.dlmXForecast_class <- function (object, parm, level = 0.95, ...){
    ## Note: parm argument is ignored
    a <- (1 - level)/2
    a <- c(a, 1 - a)    
    fac <- stats::qnorm(a)
    Q.diag <- do.call(rbind, lapply(object$Q, diag))
    data.frame(fit=object$f,
               lwr=object$f - fac[2] * sqrt(Q.diag),
               upr=object$f + fac[2] * sqrt(Q.diag),
               idx=object$idx)
}
