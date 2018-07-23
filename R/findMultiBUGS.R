findMultiBUGS <- function(){
    dir <- Sys.getenv("MultiBUGS_PATH")
    if(nchar(dir))
        return(dir)
        
    dir <- getOption("R2MultiBUGS.pgm")
    if(!is.null(dir))
        return(dir)
                
    if(.Platform$OS.type != "windows")
        return(Sys.which('MultiBUGS'))
        
    deps <- utils::packageDescription("R2MultiBUGS", fields="SystemRequirements")
    version.req <- gsub(".*MultiBUGS ?\\(>= ?(.+)\\).*", "\\1", deps)
    ob.reg <- try(utils::readRegistry("Software\\MultiBUGS", "HLM", view = "32-bit"), silent = TRUE)
    if (inherits(ob.reg, "try-error"))
        return(NA)
        
    rnames <- names(ob.reg)
    ver <- gsub("MultiBUGS ", "", rnames)
    version.inst <- gsub("(.+)e$","\\1", ver)

    if(length(version.inst > 1)){
        id <- which(apply(outer(version.inst, version.inst, Vectorize(compareVersion, c("a", "b"))), 1, function(x) all(x >= 0)))
        version.inst <- version.inst[id]
        rnames <- rnames[id]
    }
    
    if (compareVersion(max(version.inst), version.req) < 0)
        warning("Found MultiBUGS version ", version.inst, ".\n Requires ", version.req, " or greater.")

    utils::readRegistry(paste("Software", "MultiBUGS", rnames, sep="\\"), "HLM", view = "32-bit")[["InstallPath"]]
}
