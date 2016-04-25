library(magrittr)
mlapply <- function(FUN, ...)
    # mapply for all combinations of arguments
    # or lapply for multiple vectors/lists of arguments
    # for cases where mapply warns that
    # "longer argument not a multiple of length of shorter"
    # Returns a list of all evaluations of FUN.
    list(...) %>%
    expand.grid(stringsAsFactors=FALSE, KEEP.OUT.ATTRS=FALSE) %>%
    split(nrow(.) %>% seq_len) %>%
    lapply(function(x)
        x %>%
            lapply(function(x)
                if (x %>% is.list) x[[1]] else x) %>%
            do.call(FUN, .))

# example:
# mlapply(function(x,y,z) data.frame(x,y,z, sum = x + y + z, row.names="row1"),
#         x = 1, y = 1:2, z = 1:3)
#
#>  $`1`
#>       x y z sum
#>  row1 1 1 1   3
#>  
#>  $`2`
#>       x y z sum
#>  row1 1 2 1   4
#>  
#>  $`3`
#>       x y z sum
#>  row1 1 1 2   4
#>  
#>  $`4`
#>       x y z sum
#>  row1 1 2 2   5
#>  
#>  $`5`
#>       x y z sum
#>  row1 1 1 3   5
#>  
#>  $`6`
#>       x y z sum
#>  row1 1 2 3   6
