library(dplyr)

generate_label <- function(prefix, n, type = "character") {
  if(type == "character") {
    sprintf(
      paste0(prefix, "%0", nchar(as.character(n)), "d"),
      seq_len(n)
    )
  } else {
    seq_len(n)
  }
}

generate_assay <- function(distribution = runif, par_list = list(n = 10)) {
  do.call(distribution, par_list)
}

generate_design <- function(n_categories_per_group = 2,
                            n_grouping_factors = 1,
                            category_labels = NULL,
                            n_repeats = 10,
                            repeat_type = "id",
                            n_obs = 2,
                            n_var = 2,
                            n_measurements = 1,
                            distrib = runif,
                            distrib_pars = "list()",
                            percent_missing = 0) {
  
  distrib_pars <- as.list(str2lang(distrib_pars))[-1]
  # Prepare supergroups
  # supergroups <- generate_label("supergroup", n_supergroups)
  # n_groups <- n_groups * n_supergroups
  
  # Prepare groups
  if(is.null(group_categories)) {
    groups <- generate_label("Group_", n_groups)
  } else {
    groups <- group_categories
  }
  group_list <- lapply(seq_len(n_grouping_factors), function(x) {groups})
  names(group_list) <- paste0("Group_", seq_len(n_grouping_factors))
  group_combn <- expand.grid(group_list)
  
  # Prepare samples
  if(repeat_type == "series") {
    repeats <- generate_label("Sample_", n_repeats, type = "numeric")
  } else {
    repeats <- generate_label("Sample_", n_repeats)
  }
  
  # Prepare variables
  vars <- generate_label("Var_", n_var)
  
  # Prepare design table
  df_tmp <- group_combn %>%
    merge(data.frame(ID = repeats))
  
  df_out <- do.call("rbind", replicate(n_obs, df_tmp, simplify = FALSE)) %>%
    arrange(across(everything())) %>%
    mutate(Observation = generate_label("Obs_", nrow(.))) %>%
    merge(data.frame(Variable = vars)) %>%
    arrange(across(everything()))
  
  data_design <- df_out
  for(i in seq_len(n_measurements)) {
    measurement_name <- paste0("Measurement_", i)
    data_design[[measurement_name]] = generate_assay(distrib, c(
      list(n = nrow(data_design)),
      distrib_pars
    ))
  }
  data_design
}
