add_bg_colours <- function (tab) {
    tab |>
        gt::tab_options (
            heading.background.color = "#ACEACE",
            row_group.background.color = "#ACEACE",
            column_labels.background.color = "#9BD9BD",
            heading.title.font.size = "200%"
        )
}
