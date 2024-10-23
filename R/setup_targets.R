setup_crew_controller <- function(name) {
  if (Sys.info()["nodename"] %in% c("shadow", "hippocampus")) {
    crew.cluster::crew_controller_sge(
      name = sprintf("%s-sge", name),
      workers = 25,
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
