## ####################################################################

##' @description Extractor for dlm and dlmFiltered objects
##' @name extract-dlm
##' @title  Extractor for dlm and dlmFiltered objects
##' @param  object dlm object
##' @param  name Which name in the object to select.
##' @param  select Row in X matrix 
##' @return The relevant result
##' @author Søren Højsgaard
##'
## ####################################################################

#' @rdname extract-dlm
#' @export 
getdlm <- function(object, name, select){
    UseMethod("getdlm")
}

#' @rdname extract-dlm
#' @export 
getdlm.dlm <- function(object, name, select){
    if (!is.numeric(select) || length(select) > 1) stop("invalid index select\n")
    switch(name,
           "FF"=,"F"={FFeval(object, select)},
           "GG"=,"G"={GGeval(object, select)},
           "VV"=,"V"={Veval(object, select)},
           "WW"=,"W"={Weval(object, select)}
           ) 
}

#' @rdname extract-dlm
#' @export 
FFeval <- function(object, select){UseMethod("FFeval")}

#' @rdname extract-dlm
#' @export 
GGeval <- function(object, select){UseMethod("GGeval")}

#' @rdname extract-dlm
#' @export 
Veval <- function(object, select){UseMethod("Veval")}

#' @rdname extract-dlm
#' @export 
Weval <- function(object, select){UseMethod("Weval")}


#' @export 
FFeval.dlm <- function(object, select){
    if (is.null(JFF(object))) return(FF(object))
    FFF <- FF(object)    
    JFFF <- JFF(object)
    FFF[JFFF !=0] <- X(object)[select,]
    FFF
}

#' @export 
GGeval.dlm <- function(object, select){
    if (is.null(JGG(object))) return(GG(object))
    GGG <- GG(object)
    JGGG <- JGG(object)
    GGG[JGGG !=0] <- X(object)[select,]
    GGG
}

#' @export 
Veval.dlm <- function(object, select){
    if (is.null(JV(object))) return(V(object))
}

#' @export 
Weval.dlm <- function(object, select){
    if (is.null(JW(object))) return(W(object))
}





pickr  <- function(m, select, drop=TRUE){
    ## Select rows from matrix / dataframe and entries from list / vector
    ##print(head(m)); print(select)
    if (inherits(m, c("matrix", "data.frame"))) m[select,, drop=drop]
    else if (inherits(m, "list")) m[select]
    else m[select]
}

## ####################################################################

##' @description Extractor for dlmFiltered objects
##' @name extract-dlmFiltered
##' 
##' @title Extractor for dlmFiltered objects
##' @param object A dlmFiltered object.
##' @param name Which name in the object to select.
##' @param select Which element to select.
##' @return The relevant result
##' @author Søren Højsgaard

## ####################################################################

#' @rdname extract-dlm
#' @export 
getdlm <- function(object, name, select=TRUE){
    UseMethod("getdlm")
}

#' @rdname extract-dlm
#' @export 
getdlm.dlmFiltered <- function(object, name, select=TRUE){
    switch(name,
           "mm"=,"m"={mm(object, select=select)},
           "aa"=,"a"={aa(object, select=select)},
           "CC"=,"C"={CC(object, select=select)},
           "RR"=,"R"={RR(object, select=select)},
           "sdRR"=,"sdR"={
               x <- lapply(RR(object, select=select), diag)
               sqrt(do.call(rbind, x))
           },
           "sdCC"=,"sdC"={
               x <- lapply(CC(object, select=select), diag)
               sqrt(do.call(rbind, x))
           },
           "ff"=,"f"={object$f[select]},
           "yy"=,"y"={object$y[select]},
           "model"=,"mod"={object$mod}           
           )
}

#' @rdname extract-dlm
#' @export 
mm <- function(object, select=TRUE){UseMethod("mm")}

#' @rdname extract-dlm
#' @export 
mm.dlmFiltered <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$m)
    }
    pickr(object$m, select, drop=TRUE)
}

#' @rdname extract-dlm
#' @export 
aa <- function(object, select=TRUE){UseMethod("aa")}

#' @export 
aa.dlmFiltered <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$a)
    }
    pickr(object$a, select, drop=TRUE)
}

#' @rdname extract-dlm
#' @export
CC <- function(object, select=TRUE){UseMethod("CC")}

#' @export 
CC.dlmFiltered <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$U.C)
    }
    out <- 
        dlmSvd2var(pickr(object$U.C, select, drop=FALSE),
                   pickr(object$D.C, select, drop=FALSE))
    if (is.numeric(select) && length(select) == 1) out[[1]] else out
}

#' @rdname extract-dlm
#' @export 
RR <- function(object, select=TRUE){UseMethod("RR")}

#' @export 
RR.dlmFiltered <- function(object, select=TRUE){
    if (is.character(select)){
        if (identical(select, "last")) select <- NROW(object$U.R)
    }
    out <- 
        dlmSvd2var(pickr(object$U.R, select, drop=FALSE),
                   pickr(object$D.R, select, drop=FALSE))
    if (is.numeric(select) && length(select) == 1) out[[1]] else out    
}


