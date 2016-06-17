library(parallel)

qluster <- function(used_cores = NULL, export = NULL, ...) {
  if (is.null(used_cores)) used_cores <- detectCores(logical = FALSE)
  cluster <- makePSOCKcluster(used_cores)
  clusterPackages(cluster, ...)
  clusterExport(cluster, export)
  nodeSink(cluster, ...)
  cluster
}

clusterPackages <- function(cluster, pkgs = NULL, ...) {
  if (is.null(pkgs)) return(NULL)
  clusterExport(cluster, "pkgs", environment())
  clusterEvalQ(cluster, sapply(pkgs, require, character.only = TRUE))
}

nodeSink <- function(cluster, log_path = ".", open = "a", dump_files = FALSE,
                     ...) {
  clusterExport(cluster, ls(), environment())
  clusterApply(cluster, seq_along(cluster), function(i) .node_n <<- i)
  clusterEvalQ(cluster, {
    .node_path <<- file.path(log_path, paste0(.node_n, "Node"))
    .node_previous <<- options(warn = 1)
    if (dump_files) { # Is this possible?
      tmp <- options(error = quote(dump.frames(to.file = TRUE)))
      .node_previous <<- c(.node_previous, tmp)
    }
    .node_logfile <<- file(paste0(.node_path, ".log"), open = open)
    sink(.node_logfile, type = "output", split = TRUE)
    sink(.node_logfile, type = "message")
  })
}

nodeUnsink <- function(cluster) {
  clusterEvalQ(cluster, {
    sink(type = "message")
    sink()
    close(.node_logfile)
    options(.node_previous)
  })
}

unQluster <- function(cluster) {
  nodeUnsink(cluster)
  stopCluster(cluster)
}


cluster <- qluster(export = "breaks", dump_files = TRUE)
clusterEvalQ(cluster, options()$error)
clusterEvalQ(cluster, getwd())
clusterEvalQ(cluster, {
  breaks("This is a test")
  print("boo")
})

unQluster(cluster)