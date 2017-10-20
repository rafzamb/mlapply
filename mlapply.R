library(magrittr)

# A more efficient implementation avoiding
# expand.grid (which copies the parameters/arguments multiple times,
# which is inefficient for large parameters, e.g. data.frames).
mapply <- function(.Fun, ..., .Cluster=NULL) {
    `--List--` <-
        list(...)
    `--metadata--` <-
        data.frame(`--Name--` = paste0("`",names(`--List--`),"`"),
                   `--Len--` = lengths(`--List--`),
                   check.names=FALSE) %>% 
        `[`(order(.$`--Len--`),)
    `--env--` <-
        environment()
    eval(Reduce(function(previous,x)
        paste0('unlist(lapply(`--List--`$',x,',',
               'function(',x,')', previous,'),recursive=FALSE)'),
        x=`--metadata--`$`--Name--`,
        init =
            paste0(`--metadata--`$`--Name--`,'=',`--metadata--`$`--Name--`) %>%
            paste(collapse=",") %>%
            paste0('list(.Fun(',.,'))')) %>%
            parse(text=.))
}

# Example:
# 
# RESULT <-
#     mapp(function(x,y,z)
#         data.frame(x,y,z, sum = x + y + z, row.names="row1"),
#         x = 1, y = 1:2, z = 1:3)
# 
# str(RESULT)
# 
# #    List of 6
# #    $ :'data.frame':	1 obs. of  4 variables:
# #        ..$ x  : num 1
# #    ..$ y  : int 1
# #    ..$ z  : int 1
# #    ..$ sum: num 3
# #    $ :'data.frame':	1 obs. of  4 variables:
# #        ..$ x  : num 1
# #    ..$ y  : int 2
# #    ..$ z  : int 1
# #    ..$ sum: num 4
# #    $ :'data.frame':	1 obs. of  4 variables:
# #        ..$ x  : num 1
# #    ..$ y  : int 1
# #    ..$ z  : int 2
# #    ..$ sum: num 4
# #    $ :'data.frame':	1 obs. of  4 variables:
# #        ..$ x  : num 1
# #    ..$ y  : int 2
# #    ..$ z  : int 2
# #    ..$ sum: num 5
# #    $ :'data.frame':	1 obs. of  4 variables:
# #        ..$ x  : num 1
# #    ..$ y  : int 1
# #    ..$ z  : int 3
# #    ..$ sum: num 5
# #    $ :'data.frame':	1 obs. of  4 variables:
# #        ..$ x  : num 1
# #    ..$ y  : int 2
# #    ..$ z  : int 3
# #    ..$ sum: num 6
