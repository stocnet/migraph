#' Simulating multiple diffusion processes
#' @name make_play
#' @param .data An object of a manynet-consistent class:
#'   \itemize{
#'   \item matrix (adjacency or incidence) from `{base}` R
#'   \item edgelist, a data frame from `{base}` R or tibble from `{tibble}`
#'   \item igraph, from the `{igraph}` package
#'   \item network, from the `{network}` package
#'   \item tbl_graph, from the `{tidygraph}` package
#'   }
#' @param ... Other parameters inherited from `manynet::play_diffusion()`.
#' @param times Integer indicating number of simulations. 
#'   By default `times=5`, but 1,000 - 10,000 simulations recommended for publication-ready results.
#' @param strategy If `{furrr}` is installed, then multiple cores can be used to accelerate the simulations. 
#'   By default "sequential", but if multiple cores available, then "multisession" or "multicore" may be useful. 
#'   Generally this is useful only when times > 1000. See `{furrr}` for more.
#' @param verbose Whether the function should report on its progress. 
#'   By default FALSE. See `{progressr}` for more.
#' @importFrom manynet play_diffusion
#' @examples 
#' play_diffusions(mpn_elite_mex, times = 10)
#' @export
play_diffusions <- function(.data,
                            ...,
                            times = 5,
                            strategy = "sequential",
                            verbose = FALSE) {
  thisRequires("future")
  thisRequires("furrr")
  oplan <- future::plan(strategy)
  on.exit(future::plan(oplan), add = TRUE)
  
  out <- furrr::future_map_dfr(1:times, function(j){
    data.frame(sim = j,
               manynet::as_diffusion(manynet::play_diffusion(.data, ...)))
  }, .progress = verbose, .options = furrr::furrr_options(seed = T))
  make_diffs_model(out, .data)
}

