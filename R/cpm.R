perform_cpm <- function(fc, behav, confounds, ...) {
  # ensure behav is a vector
  if (is.matrix(behav)) {
    stopifnot(ncol(behav) == 1)
    behav <- behav[, 1]
  }
  if (anyNA(behav)) {
    warning("Found missing behavioral scores, will remove them.")
    behav <- behav[!is.na(behav)]
  }
  subjs_to_keep <- intersect(names(behav), rownames(fc))
  cpmr::cpm(
    fc[subjs_to_keep, ],
    behav[subjs_to_keep],
    confounds = confounds[subjs_to_keep, ],
    ...
  )
}

perform_cpm_perm <- function(fc, behav, confounds, ...) {
  # shuffle subject labels
  rownames(fc) <- sample(rownames(fc))
  perform_cpm(fc, behav, confounds, ...)
}

extract_cpm_performance <- function(result) {
  as_tibble(
    cor(result$pred, result$real),
    rownames = "include",
    .name_repair = ~"r"
  )
}

calc_dice_pairs <- function(result, level) {
  edges_sel <- lapply(
    result[1:2],
    \(x) x$edges > level * length(unique(x$folds))
  )
  proxy::simil(
    edges_sel[[1]],
    edges_sel[[2]],
    method = "Dice",
    by_rows = FALSE
  ) |>
    diag() |>
    enframe(name = "network", value = "dice")
}

binarize_edges <- function(result, level = 0.5) {
  result$edges > level * length(unique(result$folds))
}

calc_edges_degree <- function(edges) {
  apply(
    edges, 2,
    \(x) colSums(Rfast::squareform(x)),
    simplify = FALSE
  ) |>
    enframe(name = "network", value = "degree")
}

calc_edges_enrich <- function(edges, atlas_dseg) {
  labels <- with(atlas_dseg, coalesce(network_label, atlas_name)) |>
    fct_collapse(Subcortical = c("CIT168Subcortical", "SubcorticalHCP")) |>
    as.character()
  network_pairs <- as_tibble(
    t(combn(labels, 2)),
    .name_repair = ~ c("row", "col")
  )
  apply(
    edges, 2,
    \(x) {
      cbind(network_pairs, val = x) |>
        drop_na() |>
        mutate(
          label_x = pmin(row, col),
          label_y = pmax(row, col),
          .keep = "unused"
        ) |>
        summarise(
          n = sum(val),
          total = n(),
          .by = c(label_x, label_y)
        ) |>
        mutate(
          prop = n / total,
          enrich = (n / sum(n)) / (total / sum(total))
        )
    }
  ) |>
    list_rbind(names_to = "network")
}

match_confounds <- function(users_confounds, fd_mean) {
  subjs <- intersect(rownames(users_confounds), rownames(fd_mean))
  cbind(users_confounds[subjs, ], fd_mean[subjs, ])
}
