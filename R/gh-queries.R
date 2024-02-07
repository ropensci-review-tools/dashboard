get_gh_token <- function (token = "") {

    tryCatch (
        gitcreds::gitcreds_get ()$password,
        error = function (e) ""
    )
}

#' The GitHub GraphQL query.
#'
#' @param org The GitHub organization.
#' @param repo The GitHub repository.
#' @param open_only Include only open issues?
#' @param end_cursor The end cursor from the previous query.
#'
#' @return The GraphQL query to pass to a `gh::gh_gql()` call.
#' @noRd
gh_issues_qry <- function (org = "ropensci",
                           repo = "software-review",
                           open_only = TRUE,
                           end_cursor = NULL) {

    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }

    open_txt <- ifelse (open_only, ", states: [OPEN]", "")

    q <- paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
                   issues (first: 100", open_txt, after_txt, ") {
                       pageInfo {
                           hasNextPage
                           endCursor
                       }
                       edges {
                           node {
                               ... on Issue {
                                   number
                                   createdAt
                                   state
                                   lastEditedAt
                                   updatedAt
                                   assignees (first: 100) {
                                       nodes {
                                           name
                                       }
                                   }
                                   title
                                   labels (first: 100) {
                                       edges {
                                           node {
                                               name,
                                           }
                                       }
                                   }
                                   url
                                   comments (last: 100) {
                                       nodes {
                                           createdAt,
                                           author {
                                               login
                                           },
                                           body
                                       }
                                   }
                                   timelineItems (itemTypes: LABELED_EVENT, first: 100) {
                                       nodes {
                                           ... on LabeledEvent {
                                               actor {
                                                   login
                                               },
                                               createdAt,
                                               label {
                                                   name
                                               }
                                           }
                                       }
                                   }
                               }
                           }
                       }
                   }
                }
        }")

    return (q)
}