#' Get current slack status of all editors
#'
#' The API endpoints for slack are described at \url{https://api.slack.com/web}.
#' All API endpoints are in the form
#' `https://slack.com/api/METHOD_FAMILY.method`.
#'
#' This function requires an environment variable, "SLACK_TOKEN", to
#' authenticate; see \url{https://api.slack.com/docs/token-types}. The current
#' token is a "bot token" for the "status-app" app,
#' \url{https://api.slack.com/apps/A06TMRCU40J/oauth}.
#'
#' @return A `data.frame` with one row row each editor, and columns of:
#' \itemize{
#'    \item `id` The Slack ID of the editor
#'    \item `name` The name of the editor
#'    \item `real_name` The real name of the editor
#'    \item `status` The current status of the editor
#' }
#'
#' @noRd
get_slack_editors_status <- function () {

    editors <- get_editors_user_group_members ()

    tok <- get_slack_token ()

    u <- "https://slack.com/api/users.list"
    # req_retry is to avoid transitent 429 errors from Slack API requests via
    # GitHub
    req <- httr2::request (u) |>
        httr2::req_headers ("Authorization" = paste0 ("Bearer ", tok)) |>
        httr2::req_method ("GET") |>
        httr2::req_retry (
            max_tries = 3,
            is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 503)
        )
    resp <- httr2::req_perform (req)

    if (httr2::resp_is_error (resp)) {

        members <- list (
            id = NULL,
            name = NULL,
            real_name = NULL,
            status = NULL
        )

    } else {

        x <- httr2::resp_body_json (resp, simplifyVector = TRUE)
        members <- x$members [which (x$members$id %in% editors), ]

    }

    data.frame (
        id = members$id,
        name = members$name,
        real_name = members$profile$real_name,
        status = members$profile$status_text
    )
}

get_slack_token <- function () {

    envvars <- names (Sys.getenv ())
    tok_name <- grep ("slack", envvars, value = TRUE, ignore.case = TRUE)

    check_tok_exists <- function (tok_name) {
        if (length (tok_name) == 0) {
            stop (
                "Unable to find environment variable for Slack API token.",
                call. = FALSE
            )
        }
    }
    check_tok_exists (tok_name)
    if (length (tok_name) > 1L) {
        tok_name <-
            grep ("^slack\\_token$", envvars, value = TRUE, ignore.case = TRUE)
        check_tok_exists (tok_name)
    }
    return (Sys.getenv (tok_name))
}

#' Get the Slack ID of the "User Group" of editors
#'
#' @return A single character value containing the Slack ID of the editors user
#' group.
#' @noRd
get_editors_user_group_id <- function () {

    tok <- get_slack_token ()

    u <- "https://slack.com/api/usergroups.list"
    req <- httr2::request (u) |>
        httr2::req_headers ("Authorization" = paste0 ("Bearer ", tok)) |>
        httr2::req_method ("GET")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)
    x <- httr2::resp_body_json (resp, simplifyVector = TRUE)
    usergroup <- x$usergroups$id [x$usergroups$handle == "editors"]

    return (usergroup)
}

#' Get the Slack IDs of all members of the "User Group" of editors
#'
#' @return A list of Slack ID (character) values of each editor.
#' @noRd
get_editors_user_group_members <- function () {

    tok <- get_slack_token ()

    usergroup <- get_editors_user_group_id ()

    u <- "https://slack.com/api/usergroups.users.list"
    req <- httr2::request (u) |>
        httr2::req_headers ("Authorization" = paste0 ("Bearer ", tok)) |>
        httr2::req_url_query (usergroup = usergroup) |>
        httr2::req_method ("GET")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)
    x <- httr2::resp_body_json (resp, simplifyVector = TRUE)

    return (x$users)
}
