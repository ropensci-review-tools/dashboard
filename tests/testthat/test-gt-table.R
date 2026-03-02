# ---------------------------------------------------------------------------
# define_urgency_colours()
# ---------------------------------------------------------------------------

test_that ("define_urgency_colours returns 5 hex colour strings", {

    cols <- define_urgency_colours ()

    expect_type (cols, "character")
    expect_length (cols, 5L)
    expect_true (all (grepl ("^#[0-9A-Fa-f]{6,8}$", cols)))
})

# ---------------------------------------------------------------------------
# add_gt_html()
# ---------------------------------------------------------------------------

test_that ("add_gt_html wraps target column in anchor HTML", {

    dat <- data.frame (
        number = c (1L, 42L),
        title = c ("pkg1", "pkg2"),
        stringsAsFactors = FALSE
    )
    res <- add_gt_html (dat, u = "https://github.com/issues/", what = "title")

    # Column is now a list of gt::html objects
    expect_type (res$title, "list")
    expect_length (res$title, 2L)

    # Each element contains an anchor tag
    html_txt <- as.character (res$title [[1]])
    expect_match (html_txt, "<a href=")
    expect_match (html_txt, "pkg1")
})

test_that ("add_gt_html preserves number of rows", {

    dat <- data.frame (
        number = 1:5L, title = letters [1:5],
        stringsAsFactors = FALSE
    )
    res <- add_gt_html (dat, u = "https://example.com/", what = "title")

    expect_equal (nrow (res), 5L)
})

test_that ("add_gt_html embeds issue number in href", {

    dat <- data.frame (number = 99L, title = "mypkg", stringsAsFactors = FALSE)
    res <- add_gt_html (dat, u = "https://github.com/issues/", what = "title")

    html_txt <- as.character (res$title [[1]])
    expect_match (html_txt, "99")
})

# ---------------------------------------------------------------------------
# open_gt_table() / add_urgency_cols() / add_bg_colours()
# ---------------------------------------------------------------------------

# Minimal data.frame satisfying all column references inside open_gt_table()
make_review_dat <- function (n = 1L) {
    data.frame (
        stage = rep ("1/editor-checks", n),
        number = seq_len (n),
        title = paste0 ("pkg", seq_len (n)),
        stage_date = rep (as.Date ("2024-01-01"), n),
        labels = rep ("needs-attention", n),
        has_multiple_stages = rep (FALSE, n),
        elapsed_days = rep (30L, n),
        editor = rep ("mpadge", n),
        editor_date = rep ("2024-01-15", n),
        assignees = rep ("rev1", n),
        rev1 = rep ("reviewer1", n),
        rev1_assigned = rep ("2024-01-20", n),
        rev1_due = rep ("2024-02-20", n),
        rev2 = rep ("", n),
        rev2_assigned = rep ("", n),
        rev2_due = rep ("", n),
        created_at = rep (as.Date ("2024-01-01"), n),
        last_edited_at = rep (as.Date ("2024-01-15"), n),
        updated_at = rep (as.Date ("2024-01-20"), n),
        elapsed = rep ("30 days", n),
        stringsAsFactors = FALSE
    )
}

test_that ("open_gt_table returns a gt_tbl object", {

    res <- open_gt_table (make_review_dat ())
    expect_s3_class (res, "gt_tbl")
})

test_that ("open_gt_table works with multiple rows and stages", {

    dat <- rbind (
        make_review_dat (2L),
        transform (make_review_dat (1L), stage = "2/seeking-reviewers")
    )
    res <- open_gt_table (dat)
    expect_s3_class (res, "gt_tbl")
})

test_that ("add_urgency_cols returns a gt_tbl", {

    base_tbl <- gt::gt (make_review_dat ())
    base_tbl$`_data`$urgency <- 0L # urgency col expected by the style rules
    res <- add_urgency_cols (base_tbl, c ("number", "title"))
    expect_s3_class (res, "gt_tbl")
})

test_that ("add_bg_colours returns a gt_tbl", {

    base_tbl <- gt::gt (make_review_dat ())
    res <- add_bg_colours (base_tbl)
    expect_s3_class (res, "gt_tbl")
})
