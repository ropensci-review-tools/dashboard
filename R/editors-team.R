#' Get current and past rOpenSci editors from the rOpenSci website
#'
#' Editors are obtained from the rOpenSci website's team data, at
#' \url{https://github.com/ropensci/roweb3/blob/main/data/team/team.json}.
#' Current editors are entries whose "roles" include "editor", and past editors
#' are entries whose "past_roles" include "editor". Entries without a GitHub
#' handle are ignored. Guest editors are not listed there; see
#' \link{all_editors} for the full list of current editors.
#'
#' @return A `data.frame` with columns `name`, `github`, `stats`, `general`,
#' and `current` (`FALSE` for past editors).
#' @noRd
website_editors <- function () {

    u <- "https://raw.githubusercontent.com/ropensci/roweb3/main/data/team/team.json"
    req <- httr2::request (u) |>
        httr2::req_retry (max_tries = 3)
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)
    team <- httr2::resp_body_json (resp, check_type = FALSE)

    has_role <- function (field) {
        vapply (team, function (i) {
            "editor" %in% unlist (i [[field]]) && !is.null (i$github)
        }, logical (1L))
    }
    current <- has_role ("roles")
    past <- has_role ("past_roles") & !current
    team <- team [which (current | past)]

    stats <- vapply (team, function (i) isTRUE (i$stats), logical (1L))
    general <- vapply (team, function (i) isTRUE (i$generalreview), logical (1L))

    data.frame (
        name = vapply (team, function (i) i$name, character (1L)),
        github = vapply (team, function (i) i$github, character (1L)),
        stats = stats,
        general = general | !stats,
        current = current [which (current | past)]
    )
}

m_website_editors <- memoise::memoise (website_editors)

#' Get logins of all members of a GitHub team
#'
#' @param stats If `TRUE`, get members of the "stats-editors" team, otherwise
#' the "editors" team.
#' @noRd
gh_team_members <- function (stats = FALSE) {
    q <- gh_editors_team_qry (stats = stats)
    res <- gh::gh_gql (query = q)
    res <- res$data$organization$team$members$nodes
    vapply (res, function (i) i$login, character (1L))
}

#' Get full list of current rOpenSci editors
#'
#' Editors are those listed with an "editor" role in the rOpenSci website's team
#' data, at
#' \url{https://github.com/ropensci/roweb3/blob/main/data/team/team.json},
#' plus guest editors, who are members of the GitHub "editors" team but are not
#' listed on the website. Statistical software ("stats") and general status are
#' taken from the website data, falling back to membership of the GitHub
#' "stats-editors" team for guest editors. Results are cached for the duration
#' of the R session.
#'
#' @return A `data.frame` with one row per editor, and columns of:
#' \itemize{
#'    \item `login` The GitHub handle of the editor
#'    \item `name` The full name of the editor (`NA` for guest editors)
#'    \item `stats` `TRUE` for statistical software editors
#'    \item `general` `TRUE` for editors of general submissions
#'    \item `guest` `TRUE` for guest editors
#' }
#' @export
all_editors <- function () {
    m_all_editors ()
}

get_all_editors <- function () {
    web <- m_website_editors ()
    web <- web [which (web$current), ]
    gh_eds <- gh_team_members (stats = FALSE)
    gh_stats <- gh_team_members (stats = TRUE)

    # Use GitHub spelling of logins, to match issue assignees:
    index <- match (tolower (web$github), tolower (gh_eds))
    web$github [which (!is.na (index))] <- gh_eds [index [which (!is.na (index))]]

    guests <- gh_eds [which (!tolower (gh_eds) %in% tolower (web$github))]
    guest_stats <- tolower (guests) %in% tolower (gh_stats)

    data.frame (
        login = c (web$github, guests),
        name = c (web$name, rep (NA_character_, length (guests))),
        stats = c (web$stats, guest_stats),
        general = c (web$general, !guest_stats),
        guest = rep (c (FALSE, TRUE), c (nrow (web), length (guests)))
    )
}

m_all_editors <- memoise::memoise (get_all_editors)
