setup_crew_controller <- function(name) {
  if (Sys.info()["nodename"] == "shadow") {
    crew.cluster::crew_controller_sge(
      name = sprintf("%s-sge", name),
      workers = 40,
      seconds_idle = 30
    )
  } else {
    crew::crew_controller_local(
      name = sprintf("%s-local", name),
      workers = 16,
      seconds_idle = 10
    )
  }
}

setup_parallel_plan <- function() {
  options(clustermq.scheduler = "sge")
  options(clustermq.template = "clustermq.tmpl")
  if (Sys.info()["nodename"] == "shadow") {
    future::plan(future.batchtools::batchtools_sge, template = "future.tmpl")
  } else if (Sys.info()["sysname"] == "Linux") {
    future::plan(future::multicore)
  } else {
    future::plan(future.callr::callr)
  }
  invisible()
}
