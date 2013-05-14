# $Id: AllGenerics.R 104 2013-03-27 22:20:20Z sluque $

if (!isGeneric("trip"))
    setGeneric("trip",
               function(obj, TORnames) standardGeneric("trip"))

if (!isGeneric("points"))
    setGeneric("points",
               function(x, ...) standardGeneric("points"))

if (!isGeneric("lines"))
    setGeneric("lines",
               function(x, ...) standardGeneric("lines"))

if (!isGeneric("text"))
    setGeneric("text",
               function(x, ...) standardGeneric("text"))

if (!isGeneric("subset"))
    setGeneric("subset",
               function(x, ...) standardGeneric("subset"))

if (!isGeneric("as.trip"))
    setGeneric("as.trip",
               function(x, ...) standardGeneric("as.trip"))


###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
