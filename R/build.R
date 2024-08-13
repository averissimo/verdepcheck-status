#' Make internal request to GitHub API
#'
#' @param ... path in API
#' @param endpoint endpint in API
#' @param owner organization on owner
#'
#' @return a httr2 query
#' @export
#'
#' @examples
#' make_curl_req("repos", "insightsengineering", "teal", "actions", "workflows", "scheduled.yaml", "runs")
make_curl_req <- function(
    ...,
    endpoint = "repos",
    owner = "insightsengineering") {

  dots <- rlang::list2(...)

  url_path <- do.call(file.path, dots)

  url <- if (!is.null(endpoint) && !is.null(owner)) {
    sprintf("https://api.github.com/%s/%s", endpoint, owner)
  } else if (!is.null(endpoint)) {
    sprintf("https://api.github.com/%s", endpoint)
  }

  "!DEBUG query: `url`/`url_path`"

  httr2::request(file.path(url, url_path)) |>
    httr2::req_headers(
      "Accept" = "application/vnd.github+json",
      "Authorization" = sprintf("Bearer %s", Sys.getenv("GITHUB_PAT")),
      "X-GitHub-Api-Version" = "2022-11-28"
    )
}


#' Build package table information on jobs
#'
#' @param pkg_name name of package
#' @param yaml_files yaml files to query
#' @param workflows job workflows to query (placeholder on failure)
#' @param keep_regex regex to keep on jobs
#'
#' @return a tibble with package, name, status, url and icon
#' @export
#'
#' @examples
#' build_pkg_table("teal")
#' build_pkg_table(
#'   "teal",
#'   workflows = c("windows (r-devel-windows-x86_64)","macos-arm64 (r-release-macos-arm64)","ubuntu-clang","macos (r-release-macos-x86_64)","ubuntu-gcc12","windows (r-release-windows-x86_64)","macos-arm64 (r-oldrel-macos-arm64)","ubuntu-next","macos (r-oldrel-macos-x86_64)","ubuntu-release","windows (r-oldrel-windows-x86_64)","linux","nosuggests"),
#'   keep_regex = "R-hub 🌐 / (.*) \\(.*\\)"
#' )
build_pkg_table <- function(
    pkg_name,
    yaml_files = c("scheduled.yaml", "scheduled.yml"),
    workflows = c("min_cohort", "min_isolated", "max", "release"),
    keep_regex = "Dependency Test - (.*) 🔢 .*"
) {

  for (workflow_name in yaml_files) {
    res <- tryCatch({
      make_curl_req(pkg_name, "actions", "workflows", workflow_name, "runs") |>
        httr2::req_url_query(
          branch = "main"
        ) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
    }, error = function(e) {
      return(NULL)
    })
    if (!is.null(res)) break
  }

  if (is.null(res) || length(res$workflow_runs) == 0L) {
    return(tibble::tibble(
      package = pkg_name,
      name = workflows,
      status = NA_character_,
      url = NA_character_,
      icon = NA_character_
    ))
  }

  "!DEBUG Retrieving `pkg_name`..."

  "!DEBUG Get jobs for workflow `res$workflow_runs[[1]]$id`"

  jobs <- list(jobs = list(), total_count = 1L, page = 1L)
  while (jobs$page <= 10 && jobs$total_count > length(jobs$jobs)) {
    "!DEBUG Get page `jobs$page` of jobs"
    new_jobs <- make_curl_req(
      pkg_name,
      "actions",
      "runs",
      res$workflow_runs[[1]]$id,
      "jobs"
    ) |>
      httr2::req_url_query(per_page = 100, page = jobs$page) |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    jobs$jobs <- c(jobs$jobs, new_jobs$jobs)
    jobs$total_count <- new_jobs$total_count
    jobs$page <- jobs$page + 1L
  }

  processed <- jobs$jobs |>
    purrr::keep(~ grepl(keep_regex, .x$name)) |>
    purrr::map(
      ~ list(
        package = pkg_name,
        name = gsub(keep_regex, "\\1", .x$name),
        status = .x$conclusion,
        url = .x$html_url
      )
    )


  if (identical(length(processed), 0L)) {
    return(tibble::tibble(
      package = pkg_name,
      name = workflows,
      status = NA_character_,
      url = NA_character_,
      icon = NA_character_
    ))
  }

  processed <- dplyr::bind_rows(processed)

  "!DEBUG Keeping `length(processed)`: `paste(processed$name, processed$status, sep = '=', collapse = ', ')`"

  processed |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      icon = dplyr::case_when(
        status == "success" ~ "✅",
        status == "failure" ~ "❌",
        status == "cancelled" ~ "🚫",
        is.na(status) ~ "❓",
        .default = "",
      ),
      status = dplyr::if_else(is.na(status), "unkown", status)
    )
}
