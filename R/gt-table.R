open_gt_table <- function (dat) {

    requireNamespace ("gt")
    u <- "https://github.com/ropensci/software-review/issues/"
    dat_url <- dat
    add_html <- function (dat_url, what = "number") {
        dat_url [[what]] <- paste0 (
            "<p><a href=",
            u,
            dat_url$number,
            ">",
            dat_url [[what]],
            "</a>"
        )
        dat_url [[what]] <- lapply (dat_url [[what]], gt::html)
        return (dat_url)
    }
    # Number has to come last here!!
    dat_url <- add_html (dat_url, "titles")
    dat_url <- add_html (dat_url, "number")
    dat_url$stage_elapsed <- Sys.Date () - dat_url$stage_date

    # Then create an "urgency" column used to highlight rows needing urgent
    # attention. Note that colours have to be individually hand-coded in the
    # `gt` code below, so use of any value other than `ncols = 5` requires the
    # subsequent colors to be re-coded.
    ncols <- 5L
    dat_url$urgency <- 0L

    # time scales for each stage, in days
    time_scales <- list (
        c (0, 7), # initial editorial handling
        c (1, 7), # editor checks
        c (2, 14), # seeking reviewers
        c (3, 21), # reviews
        c (4, 21), # author responses
        c (5, 21) # reviewer responses
    )
    for (i in time_scales) {
        index <- grep (paste0 ("^", i [1]), dat_url$stage)
        dat_url$urgency [index] <-
            floor (as.numeric (dat_url$stage_elapsed [index]) / i [2])
    }

    dat_url$urgency [dat_url$urgency > ncols] <- ncols
    dat_url$urgency [grepl ("holding", dat_url$labels)] <- 0

    gt::gt (
        dat_url,
        groupname_col = "stage"
    ) |>
        gt::tab_header ("rOpenSci submission overview") |>
        gt::cols_hide (c (stage_elapsed, urgency, has_multiple_stages)) |>
        gt::tab_spanner (
            label = "Editor",
            id = "ed_span",
            columns = c (`editor`, `editor_date`, `assignees`)
        ) |>
        gt::tab_spanner (
            label = "Dates",
            columns = c (`created_at`, `last_edited_at`, `updated_at`)
        ) |>
        gt::tab_style (
            style = list (gt::cell_fill (color = "#EEEEEE")),
            locations = gt::cells_body (
                columns = c (`created_at`, `last_edited_at`, `updated_at`)
            )
        ) |>
        gt::tab_spanner (
            label = "Reviewer #1",
            id = "rev1span",
            columns = c (`rev1`, `rev1_assigned`, `rev1_due`)
        ) |>
        gt::tab_spanner (
            label = "Reviewer #2",
            columns = c (`rev2`, `rev2_assigned`, `rev2_due`)
        ) |>
        gt::tab_style (
            style = list (gt::cell_fill (color = "#EEEEEE")),
            locations = gt::cells_body (
                columns = c (`rev2`, `rev2_assigned`, `rev2_due`)
            )
        ) |>
        gt::tab_style (
            # Alternative dark separators to distinguish group titles
            style = list (
                gt::cell_borders (
                    side = "bottom",
                    color = "#666666",
                    weight = gt::px (3)
                )
            ),
            locations = gt::cells_column_spanners (
                spanners = c ("ed_span", "rev1span")
            )
        ) |>
        gt::tab_style (
            # Finally styles for "stage_date" columns to highlight need for
            # action. Each grade of "urgency" has to be individually specified.
            style = list (gt::cell_fill (color = "#FFFF8088")),
            locations = gt::cells_body (
                columns = c (`number`, `title`, `stage_date`, `labels`),
                rows = urgency == 1
            )
        ) |>
        gt::tab_style (
            style = list (gt::cell_fill (color = "#FFFF0088")),
            locations = gt::cells_body (
                columns = c (`number`, `title`, `stage_date`, `labels`),
                rows = urgency == 2
            )
        ) |>
        gt::tab_style (
            style = list (gt::cell_fill (color = "#FFAA0088")),
            locations = gt::cells_body (
                columns = c (`number`, `title`, `stage_date`, `labels`),
                rows = urgency == 3
            )
        ) |>
        gt::tab_style (
            style = list (gt::cell_fill (color = "#FF550088")),
            locations = gt::cells_body (
                columns = c (`number`, `title`, `stage_date`, `labels`),
                rows = urgency == 4
            )
        ) |>
        gt::tab_style (
            style = list (gt::cell_fill (color = "#FF000088")),
            locations = gt::cells_body (
                columns = c (`number`, `title`, `stage_date`, `labels`),
                rows = urgency == 5
            )
        ) |>
        gt::tab_options (
            heading.background.color = "#ACEACE",
            row_group.background.color = "#ACEACE",
            column_labels.background.color = "#9BD9BD",
            heading.title.font.size = "200%"
        )
}
