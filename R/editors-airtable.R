#' Get current vacation status of all rOpenSci editors.
#'
#' @return A `data.frame` with one row per editor and information on current
#' vacation status obtained from rOpenSci's airtable database.
#' @export
editor_vacation_status <- function (quiet = FALSE) {

    editor_vacation <- airtabler::airtable (
        base = "app8dssb6a7PG6Vwj", table = "editor-vacation-status"
    )
    fields <- list ("editor", "Action", "Start Date", "Return Date")
    editor_vacation <- editor_vacation$`editor-vacation-status`$select_all (fields = fields)
    index <- vapply (editor_vacation$editor, function (i) length (i) > 0L, logical (1L))
    edvac <- editor_vacation [which (index), ]

    rev_prod <- airtabler::airtable (
        base = "app8dssb6a7PG6Vwj", table = "reviewers-prod"
    )
    rev_prod <- rev_prod$`reviewers-prod`$select_all (fields = list ("github", "name"))
    rev_prod <- rev_prod [match (edvac$editor, rev_prod$id), ]
    edvac$github <- rev_prod$github
    edvac$name <- rev_prod$name

    edvac$away <- FALSE
    s <- edvac$`Start Date` [2]
    index <- (!is.na (edvac$`Start Date`) & is.na (edvac$`Return Date`)) |
        as.Date (edvac$`Return Date`) > Sys.Date ()
    index [which (is.na (index))] <- FALSE
    edvac$away [index] <- TRUE

    return (edvac)
    editors_on_vacation <- edvac$github [which (edvac$away)]
    ed_status$status [ed_status$editor %in% editors_on_vacation] <- "ON LEAVE"
}
