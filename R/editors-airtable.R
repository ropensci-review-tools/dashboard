#' Get current vacation status of all rOpenSci editors.
#'
#'
#' Data are obtained both from airtable and current slack status. If either of
#' these indicate that an editor is away, on vacation, or otherwise unavailable,
#' then the `away` column will be set to `TRUE`.
#'
#' @return A `data.frame` with one row per editor and information on current
#' vacation status.
#' @export
editor_vacation_status <- function () {

    edvac_airtable <- edvac_status_airtable ()
    slack_status <- get_slack_editors_status ()
    vacation_ptn <- "away|vacation|holiday|unavailable"
    vacation <- grep (vacation_ptn, slack_status$status, ignore.case = TRUE)
    slack_status$away <- FALSE
    slack_status$away [vacation] <- TRUE

    if (!all (edvac_airtable$github %in% slack_status$name)) {
        # This warning should indicate any slack names which do not match github
        # names - it does not yet do exactly that, and should be fixed.
        index <- which (!edvac_airtable$github %in% slack_status$name)
        warning (
            "Not all airtable editor names match slack names. ",
            "GitHub handles of mis-matches: ",
            paste0 (edvac_airtable$github [index], collapse = ", ")
        )
    }

    edvac <- slack_status
    edvac_airtable <- edvac_airtable [which (edvac_airtable$away), ]
    index <- match (edvac_airtable$github, slack_status$name)
    edvac$away [index] <- TRUE

    return (edvac)
}

edvac_status_airtable <- function () {
    editor_vacation <- airtabler::airtable (
        base = "app8dssb6a7PG6Vwj", table = "editor-vacation-status"
    )
    fields <- list ("editor", "Action", "Start Date", "Return Date")
    editor_vacation <-
        editor_vacation$`editor-vacation-status`$select_all (fields = fields)
    index <- vapply (
        editor_vacation$editor,
        function (i) length (i) > 0L,
        logical (1L)
    )
    edvac <- editor_vacation [which (index), ]

    rev_prod <- airtabler::airtable (
        base = "app8dssb6a7PG6Vwj", table = "reviewers-prod"
    )
    rev_prod <-
        rev_prod$`reviewers-prod`$select_all (fields = list ("github", "name"))
    rev_prod <- rev_prod [match (edvac$editor, rev_prod$id), ]
    edvac$github <- rev_prod$github
    edvac$name <- rev_prod$name

    edvac$away <- FALSE
    index <- (!is.na (edvac$`Start Date`) & is.na (edvac$`Return Date`)) |
        as.Date (edvac$`Return Date`) > Sys.Date ()
    index [which (is.na (index))] <- FALSE
    edvac$away [index] <- TRUE

    return (edvac)
}

#' Add additional columns to 'editors' data from rOpenSci's airtable database,
#' and remove any "emeritus" editors.
#'
#' @param editors The `data.frame` of editors returned as the "status" component
#' from \link{editor_status}.
#' @return A modified version of `editors` with additional columns.
#' @export
add_editor_airtable_data <- function (editors) {
    rev_prod <- airtabler::airtable (
        base = "app8dssb6a7PG6Vwj", table = "reviewers-prod"
    )
    fields <- list ("github", "name", "editor", "other_langs", "domain_expertise")
    rev_prod <- rev_prod$`reviewers-prod`$select_all (fields = fields)

    editors$other_langs <- editors$domain_expertise <- NA_character_
    index <- match (tolower (editors$editor), tolower (rev_prod$github))

    editors$other_langs <- vapply (
        rev_prod$other_langs [index],
        function (i) paste0 (unlist (i), collapse = ", "),
        character (1L)
    )

    editors$domain_expertise <- rev_prod$domain_expertise [index]
    editors$domain_expertise <- vapply (editors$domain_expertise, function (i) {
        paste0 (i [which (!grepl ("^Other", i))], collapse = ", ")
    }, character (1L))

    # Finally, remove "emeritus" editors
    emeritus <- vapply (rev_prod$editor, function (i) {
        res <- length (i) > 0L
        if (res) res <- any (grepl ("emeritus", i, ignore.case = TRUE))
        return (res)
    }, logical (1L))
    emeritus <- rev_prod$github [which (emeritus)]
    index <- which (editors$editor %in% emeritus)
    if (length (index) > 0L) {
        editors <- editors [-index, ]
    }

    return (editors)
}
