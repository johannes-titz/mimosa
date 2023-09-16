#' @noRd
glue_sys_reqs <- function() {
  deps <- desc::desc_get_deps()
  deps_vec <- deps[deps$type == "Imports", "package"]
  base_pkgs <- rownames(installed.packages(priority = "base"))
  deps_vec <- deps_vec[!(deps_vec %in% base_pkgs)]
  cmd <- vetiver:::glue_sys_reqs(deps_vec)
}