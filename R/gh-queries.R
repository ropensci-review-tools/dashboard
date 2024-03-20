#' The GitHub GraphQL query to extract information from all software-review
#' issues.
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
                                   body
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

#' The GitHub GraphQL query to extract members of 'editors' team
#'
#' @return The GraphQL query to pass to a `gh::gh_gql()` call.
#' @noRd
gh_editors_team_qry <- function (stats = FALSE) {

    team <- ifelse (stats, "stats-editors", "editors")

    q <- paste0 ("{
        organization(login:\"ropensci\") {
            team(slug: \"", team, "\") {
                members(first: 100, membership: IMMEDIATE) {
                    nodes {
                        login
                    }
                }
            }
        }
    }")

    return (q)
}

#' Reduced version of 'gh_issues_qry()' that only returns issue assignees (=
#' editors).
#'
#' @param org The GitHub organization.
#' @param repo The GitHub repository.
#' @param end_cursor The end cursor from the previous query.
#'
#' @return The GraphQL query to pass to a `gh::gh_gql()` call.
#' @noRd
gh_issue_assignees_qry <- function (org = "ropensci",
                                    repo = "software-review",
                                    end_cursor = NULL) {

    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }
    # See issue #5:
    order_txt <-
        ", orderBy: {field: CREATED_AT, direction: ASC}"

    q <- paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
                   issues (first: 100", after_txt, order_txt, ") {
                       pageInfo {
                           hasNextPage
                           endCursor
                       }
                       nodes {
                           number
                           state
                           createdAt
                           closedAt
                           updatedAt
                           assignees (first: 100) {
                               nodes {
                                   name
                                   login
                               }
                           }
                           title
                           url
                       }
                   }
                }
        }")

    return (q)
}

#' Reduced version of 'gh_issues_qry()' that only returns dates and states, but
#' for all issues (open and clsoed).
#'
#' @param org The GitHub organization.
#' @param repo The GitHub repository.
#' @param end_cursor The end cursor from the previous query.
#'
#' @return The GraphQL query to pass to a `gh::gh_gql()` call.
#' @noRd
gh_issues_qry_dates_states <- function (org = "ropensci",
                                        repo = "software-review",
                                        end_cursor = NULL) {

    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }

    q <- paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
                   issues (first: 100", after_txt, ") {
                       pageInfo {
                           hasNextPage
                           endCursor
                       }
                       edges {
                           node {
                               ... on Issue {
                                   number
                                   createdAt
                                   closedAt
                                   state
                                   body
                                   labels (first: 100) {
                                       edges {
                                           node {
                                               name,
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
