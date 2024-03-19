#' Get current vacation status of all rOpenSci editors.
#'
#' @return A `data.frame` with one row per editor and information on current
#' vacation status obtained from rOpenSci's airtable database.
#' @export
editor_vacation_status <- function () {

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

#' Add additional columns to 'editors' data from rOpenSci's airtable database.
#'
#' @param editors The `data.frame` of editors returned as the "status" component
#' from \link{editor_status}.
#' @return A modified version of `editors` with additional columns.
#' @export
add_editor_airtable_data <- function (editors) {
    rev_prod <- airtabler::airtable (
        base = "app8dssb6a7PG6Vwj", table = "reviewers-prod"
    )
    fields <- list ("github", "name", "other_langs", "domain_expertise")
    rev_prod <- rev_prod$`reviewers-prod`$select_all (fields = fields)

    editors$other_langs <- editors$domain_expertise <- NA_character_
    index <- match (editors$editor, rev_prod$github)

    editors$other_langs <- rev_prod$other_langs [index]
    editors$other_langs <- vapply (editors$other_langs, function (i) {
        paste0 (i [which (!i == "English")], collapse = ", ")
    }, character (1L))

    editors$domain_expertise <- rev_prod$domain_expertise [index]
    editors$domain_expertise <- vapply (editors$domain_expertise, function (i) {
        paste0 (i [which (!grepl ("^Other", i))], collapse = ", ")
    }, character (1L))

    return (editors)
}
