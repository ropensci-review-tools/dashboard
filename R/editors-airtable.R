airtable_base_id <- "app8dssb6a7PG6Vwj"

#' Get current vacation status of all rOpenSci editors.
#'
#' The list of current editors is obtained from \link{all_editors}, and
#' vacation status from airtable. An
#' editor indicated by airtable as being away, on vacation, or otherwise
#' unavailable will have the `away` column set to `TRUE`.
#'
#' @param airtable_id The 'id' string of the airtable table.
#' @return A `data.frame` with one row per editor and information on current
#' vacation status.
#' @export
editor_vacation_status <- function (airtable_id) {

    edvac_airtable <- edvac_status_airtable (airtable_id)
    edvac <- m_all_editors () [, c ("name", "login")]
    names (edvac) [2] <- "github"

    away <- edvac_airtable$github [which (edvac_airtable$away)]
    edvac$away <- tolower (edvac$github) %in% tolower (away)

    return (edvac)
}

#' Get GitHub handles of all past and present guest editors from airtable
#'
#' @param airtable_id The 'id' string of the airtable table.
#' @return A character vector of unique GitHub handles.
#' @noRd
guest_editors_airtable <- function (airtable_id) {
    guests <- airtabler::airtable (base = airtable_id, table = "guest-editors")
    guests <- guests$`guest-editors`$select_all (fields = list ("github"))
    github <- unlist (guests$github)
    unique (github [which (!is.na (github) & nzchar (github))])
}
m_guest_editors_airtable <- memoise::memoise (guest_editors_airtable)

#' Get GitHub handles of all guest editors who are not regular editors
#'
#' Current guest editors are members of the GitHub "editors" team who are not
#' listed as editors on the rOpenSci website (see \link{all_editors}). Past
#' guest editors are listed in the airtable "guest-editors" table. Editors who
#' were guest editors before becoming regular editors are not included.
#'
#' @param airtable_id The 'id' string of the airtable table.
#' @return A character vector of GitHub handles.
#' @noRd
guest_editors <- function (airtable_id) {
    all_eds <- m_all_editors ()
    current_guests <- all_eds$login [which (all_eds$guest)]

    past_guests <- m_guest_editors_airtable (airtable_id)
    website_eds <- m_website_editors ()$github
    past_guests <- past_guests [which (!tolower (past_guests) %in% tolower (website_eds))]

    unique (c (current_guests, past_guests))
}

eic_rev_prod_table <- function (airtable_id) {
    rev_prod <- airtabler::airtable (
        base = airtable_id, table = "reviewers-prod"
    )
    rev_prod <-
        rev_prod$`reviewers-prod`$select_all (fields = list ("github", "name"))
}
m_eic_rev_prod_table <- memoise::memoise (eic_rev_prod_table)

edvac_status_airtable <- function (airtable_id) {
    editor_vacation <- airtabler::airtable (
        base = airtable_id, table = "editor-vacation-status"
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

    rev_prod <- m_eic_rev_prod_table (airtable_id)
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
#' @param airtable_id The 'id' string of the airtable table.
#' @return A modified version of `editors` with additional columns.
#' @export
add_editor_airtable_data <- function (editors, airtable_id) {
    rev_prod <- airtabler::airtable (
        base = airtable_id, table = "reviewers-prod"
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

eic_airtable_data <- function (airtable_id) {

    eic_table <- airtabler::airtable (
        base = airtable_id, table = "editor-in-chief-rotation"
    )
    eic <- eic_table$`editor-in-chief-rotation`$select_all ()

    eic$period_start <- as.Date (eic$period_start)
    eic$period_end <- as.Date (eic$period_end)
    eic <- eic [order (eic$period_start), ]

    today <- Sys.Date ()
    i_now <- i_next <- which (eic$period_start <= today & eic$period_end >= today)
    if (nrow (eic) > i_now) {
        i_now <- i_now + 0:1
    }
    eic_current <- eic [i_now, ] # Current and next EiC
    eic_names <- unlist (eic_current$acting_eic_name)
    eic_ids <- eic_current$acting_eic
    eic_start_dates <- eic_current$period_start

    rev_prod <- m_eic_rev_prod_table (airtable_id)
    eic_in_rev_table <- match (eic_ids, rev_prod$id)
    eic_github <- rev_prod$github [eic_in_rev_table]

    data.frame (
        name = eic_names,
        github = eic_github,
        start_date = eic_start_dates,
        what = c ("current", "next") [seq_along (eic_names)]
    )
}
