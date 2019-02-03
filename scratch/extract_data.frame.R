
setGeneric("extract_data.frame", function(object,pars, ...) standardGeneric("extract_data.frame"))

setMethod("extract_data.frame", signature = "stanfit", function(object, pars, labels) {
    
    if (missing(pars)) {
        pars <- object@model_pars
    }
    
    object <- rstan::extract(object, pars = pars, permuted = FALSE, inc_warmup = FALSE)
    
    #############################################################
    # code for extraction of iterations from array object       #
    #############################################################
    dfr <- data.frame(variable = NULL, iteration = NULL, chain = NULL, value = NULL)
    
    for(par in pars) {
        m <- regexpr('\\[.+\\]',par)
        if(m>0) {
            i <- match(par,dimnames(object)$parameters)
            if(!is.na(i)) {
                dfr.tmp <- reshape2::melt(object[,,i, drop = FALSE])
                #if(ncol(dfr.tmp)>2) {
                    dfr <- rbind(dfr,data.frame(variable=dimnames(object)$parameters[i],iteration=dfr.tmp$iterations,chain=dfr.tmp$chains,value=dfr.tmp$value))
                #} else {
                #    dfr <- rbind(dfr,data.frame(variable=dimnames(object)$parameters[i],iteration=1:dim(dfr.tmp)[1],chain='chain:1',value=dfr.tmp$value))
                #}
            }
        } else {
            mm <- 0
            for(parname in dimnames(object)$parameters) {
                m  <- regexpr(par,parname)
                if(m>0) { 
                    i <- match(parname,dimnames(object)$parameters)
                    dfr.tmp <- reshape2::melt(object[,,i, drop = FALSE])
                    #if(ncol(dfr.tmp)>2) {
                        dfr <- rbind(dfr,data.frame(variable=parname,iteration=dfr.tmp$iterations,chain=dfr.tmp$chains,value=dfr.tmp$value)) 
                    #} else {
                    #    dfr <- rbind(dfr,data.frame(variable=parname,iteration=1:dim(dfr.tmp)[1],chain='chain:1',value=dfr.tmp$value)) 
                    #}
                    mm <- mm + 1
                } else {
                    if(mm>0) break
                }
            }
        }
    }
    if (!nrow(dfr) > 0) stop('parameter not found\n')
    
    dfr$chain <- unlist(lapply(strsplit(as.character(dfr$chain),split = ':'), function(x) x[2]))
    
    if (!missing(labels)) {
        if (nlevels(dfr$variable) == length(labels)) {
            dfr$label <- dfr$variable
            levels(dfr$label) <- labels
        }
    }
    
    return(dfr)
})

setMethod("extract_data.frame", signature = "numeric", function(object, pars, labels) {
    
    if (missing(pars)) {
        pars <- names(object)
    }
    
    m <- regexpr(pars, names(object))
    m <- object[m > 0]
    dfr <- data.frame(variable = names(m), value = as.numeric(m))
    
    if (!missing(labels)) {
        if (nlevels(dfr$variable) == length(labels)) {
            dfr$label <- dfr$variable
            levels(dfr$label) <- labels
        }
    }
    
    return(dfr)
})

