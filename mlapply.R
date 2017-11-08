library(magrittr)

# A more efficient implementation avoiding
# expand.grid which copies the parameters/arguments multiple times,
# which is inefficient for large parameters (e.g. data.frames).
mlapply <- function(.Fun, ..., .Cluster=NULL, .parFun=parallel::parLapply) {
    `--List--` <-
        list(...)
    names(`--List--`) <-
        names(`--List--`) %>% 
        `if`(is.null(.),
             rep.int("", length(`--List--`)),
             .) %>% 
        ifelse(.=="", # for unnamed args in ...
               seq_along(.) %>% 
                   paste0(ifelse(.==1 | .>20 & .%%10==1, 'st', ""),
                          ifelse(.==2 | .>20 & .%%10==2, 'nd', ""),
                          ifelse(.==3 | .>20 & .%%10==3, 'rd', ""),
                          ifelse(.>3 & .<=20 | !(.%%10 %in% 1:3), 'th', "")) %>% 
                   paste("argument in mlapply's ..."),
               .)
    `--metadata--` <-
        data.frame(Name = paste0("`",names(`--List--`),"`"),
                   Len = lengths(`--List--`),
                   OriginalOrder = seq_len(length(`--List--`)),
                   stringsAsFactors=FALSE)
    eval(Reduce(function(previous,x)
        paste0('unlist(lapply(`--List--`$',x,',',
               'function(',x,')', previous,'),recursive=FALSE)'),
        x =
            `--metadata--` %>% 
            `[`(order(.$Len),) %>% 
            `$`(Name),
        init =
            `--metadata--` %>% 
            `[`(order(.$OriginalOrder),) %>% 
            `$`(Name) %>% 
            ifelse(grepl("argument in mlapply's ...",.,fixed=TRUE),
                   ., paste0(.,'=',.)) %>% 
            paste(collapse=',') %>%
            paste0('list(.Fun(',.,'))')) %>% print %>% 
            ifelse(.Cluster %>% is.null,
                   .,
                   sub('lapply(',
                       '.parFun(.Cluster,',
                       ., fixed=TRUE)) %>% 
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
