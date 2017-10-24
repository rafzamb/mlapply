library(magrittr)

# A more efficient implementation avoiding
# expand.grid which copies the parameters/arguments multiple times,
# which is inefficient for large parameters (e.g. data.frames).
mlapply <- function(.Fun, ..., .Cluster=NULL, .parFun=parallel::parLapply) {
    `--List--` <-
        list(...)
    `--Names--` <-
        names(`--List--`)
    names(`--List--`) <-
        ifelse(`--Names--`=="",
               formatC(runif(length(`--Names--`))*1e8, digits=0, format='f', width=9, flag='0'),
               `--Names--`)
    `--metadata--` <-
        data.frame(`--Name--` = paste0("`",names(`--List--`),"`"),
                   `--Len--` = lengths(`--List--`),
                   check.names=FALSE) %>% 
        `[`(order(.$`--Len--`),)
    eval(Reduce(function(previous,x)
        paste0('unlist(lapply(`--List--`$',x,',',
               'function(',x,')', previous,'),recursive=FALSE)'),
        x=`--metadata--`$`--Name--`,
        init =
            paste0(`--metadata--`$`--Name--`,'=',`--metadata--`$`--Name--`) %>%
            paste(collapse=",") %>%
            paste0('list(.Fun(',.,'))')) %>% 
            sub('lapply(',
                ifelse(.Cluster %>% is.null,
                       'lapply(',
                       '.parFun(.Cluster,'),
                ., fixed=TRUE) %>% 
            parse(text=.))
}


# # Example:
# 
# RESULT <-
#     mlapply(function(x,y,z)
#            data.frame(x,y,z, sum = x + y + z, row.names="row1"),
#            x = 1, y = 1:2, z = 1:3)
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


# # Example of using parallel lapply:
# 
# cl <- parallel::makeCluster(parallel::detectCores())
# 
# RESULT_FROM_PARALLEL_1 <-
#     mlapply(function(x,y,z)
#         data.frame(x,y,z, sum = x + y + z, row.names="row1"),
#         x = 1, y = 1:2, z = 1:3,
#         .Cluster=cl)
# 
# identical(RESULT,
#           RESULT_FROM_PARALLEL_1)
# 
# #    [1] TRUE
# 
# RESULT_FROM_PARALLEL_2 <- # load-balancing version:
#     mlapply(function(x,y,z)
#         data.frame(x,y,z, sum = x + y + z, row.names="row1"),
#         x = 1, y = 1:2, z = 1:3,
#         .Cluster=cl,
#         .parFun=parallel::parLapplyLB)
