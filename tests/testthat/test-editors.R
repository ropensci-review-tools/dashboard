# ---------------------------------------------------------------------------
# clean_assignees()
# ---------------------------------------------------------------------------

test_that ("clean_assignees returns vector of same length as input", {

    eds <- data.frame (login = c ("edA", "edB"), stringsAsFactors = FALSE)
    assignees <- list (c ("edA"), c ("edB"), c ("edA", "edB"))

    res <- clean_assignees (assignees, eds)
    expect_length (res, 3L)
    expect_type (res, "character")
})

test_that ("clean_assignees picks first editor-team member from each element", {

    eds <- data.frame (login = c ("edA", "edB"), stringsAsFactors = FALSE)
    assignees <- list (
        c ("external", "edA"), # edA is the editor (second)
        c ("edB"), # edB only
        c ("external1", "external2") # no editor match
    )

    res <- clean_assignees (assignees, eds)
    expect_equal (res [1], "edA")
    expect_equal (res [2], "edB")
    expect_true (is.na (res [3]))
})

# ---------------------------------------------------------------------------
# editor_latest_issue()
# ---------------------------------------------------------------------------

test_that ("editor_latest_issue returns data.frame with correct columns", {

    eds <- data.frame (
        login = c ("edA", "edB"),
        stats = c (FALSE, TRUE),
        general = c (TRUE, FALSE),
        stringsAsFactors = FALSE
    )
    res <- editor_latest_issue (
        eds,
        assignees  = c ("edA"),
        number     = c (1L),
        state      = c ("OPEN"),
        updated_at = c ("2024-01-01T00:00:00Z")
    )

    expect_s3_class (res, "data.frame")
    expect_named (res, c ("editor", "stats", "general", "number", "state", "updated_at"))
    expect_equal (nrow (res), 2L) # one row per editor
})

test_that ("editor_latest_issue selects most-recently-updated issue per editor", {

    eds <- data.frame (
        login = c ("edA", "edB"),
        stats = c (FALSE, FALSE),
        general = c (TRUE, TRUE),
        stringsAsFactors = FALSE
    )
    # edA has issues 1 (Jan) and 3 (Mar); edB has issue 2 (Feb)
    res <- editor_latest_issue (
        eds,
        assignees = c ("edA", "edB", "edA"),
        number = c (1L, 2L, 3L),
        state = c ("CLOSED", "OPEN", "OPEN"),
        updated_at = c (
            "2024-01-01T00:00:00Z",
            "2024-02-01T00:00:00Z",
            "2024-03-01T00:00:00Z"
        )
    )

    row_edA <- res [res$editor == "edA", ]
    expect_equal (row_edA$number, 3L) # most recent for edA is issue 3
    expect_equal (row_edA$state, "OPEN")

    row_edB <- res [res$editor == "edB", ]
    expect_equal (row_edB$number, 2L)
})

test_that ("editor_latest_issue gives NA number for editor with no issues", {

    eds <- data.frame (
        login = c ("edA", "edC"),
        stats = c (FALSE, FALSE),
        general = c (TRUE, TRUE),
        stringsAsFactors = FALSE
    )
    res <- editor_latest_issue (
        eds,
        assignees  = c ("edA"),
        number     = c (1L),
        state      = c ("OPEN"),
        updated_at = c ("2024-01-01T00:00:00Z")
    )

    row_edC <- res [res$editor == "edC", ]
    expect_true (is.na (row_edC$number))
})

# ---------------------------------------------------------------------------
# editor_reviews()
# ---------------------------------------------------------------------------

test_that ("editor_reviews returns data.frame with correct columns", {

    res <- editor_reviews (
        assignees  = c ("edA"),
        number     = c (1L),
        titles     = c ("pkg1"),
        state      = c ("OPEN"),
        opened_at  = c ("2024-01-01T00:00:00Z"),
        updated_at = c ("2024-06-01T00:00:00Z"),
        closed_at  = c ("")
    )

    expect_s3_class (res, "data.frame")
    expect_named (res, c (
        "editor", "number", "title", "state",
        "opened_at", "closed_at"
    ))
})

test_that ("editor_reviews sorts by editor name then issue number", {

    res <- editor_reviews (
        assignees = c ("edB", "edA", "edA"),
        number = c (5L, 3L, 1L),
        titles = c ("pkg2", "pkg3", "pkg1"),
        state = c ("CLOSED", "OPEN", "CLOSED"),
        opened_at = c (
            "2024-03-01T00:00:00Z",
            "2024-02-01T00:00:00Z",
            "2024-01-01T00:00:00Z"
        ),
        updated_at = c (
            "2024-06-01T00:00:00Z",
            "2024-05-01T00:00:00Z",
            "2024-04-01T00:00:00Z"
        ),
        closed_at = c ("2024-05-01T00:00:00Z", "", "2024-03-01T00:00:00Z")
    )

    expect_equal (res$editor, c ("edA", "edA", "edB"))
    expect_equal (res$number, c (1L, 3L, 5L))
    expect_equal (res$title, c ("pkg1", "pkg3", "pkg2"))
})

test_that ("editor_reviews uses updated_at as closed_at for open issues", {

    res <- editor_reviews (
        assignees  = c ("edA", "edA"),
        number     = c (1L, 2L),
        titles     = c ("closed-pkg", "open-pkg"),
        state      = c ("CLOSED", "OPEN"),
        opened_at  = c ("2024-01-01T00:00:00Z", "2024-02-01T00:00:00Z"),
        updated_at = c ("2024-04-01T00:00:00Z", "2024-06-01T00:00:00Z"),
        closed_at  = c ("2024-04-01T00:00:00Z", "")
    )

    expect_s3_class (res$closed_at, "Date")
    # open issue: closed_at should equal updated_at date
    open_row <- res [res$number == 2L, ]
    expect_equal (open_row$closed_at, as.Date ("2024-06-01"))
})

# ---------------------------------------------------------------------------
# editor_timeline()
# ---------------------------------------------------------------------------

# Shared fixtures: 3 issues, 2 editors
tl_assignees <- list ("edA", "edB", "edA")
tl_state <- c ("CLOSED", "CLOSED", "OPEN")
tl_opened <- c (
    "2024-01-15T00:00:00Z",
    "2024-04-15T00:00:00Z",
    "2024-07-15T00:00:00Z"
)
tl_updated <- c (
    "2024-05-01T00:00:00Z",
    "2024-06-01T00:00:00Z",
    "2024-09-01T00:00:00Z"
)
tl_closed <- c ("2024-05-01T00:00:00Z", "2024-06-01T00:00:00Z", "")

test_that ("editor_timeline returns list with issues_total and issues_new data.frames", {

    res <- editor_timeline (
        tl_assignees, tl_state, tl_opened, tl_updated, tl_closed,
        aggregation_period = "quarter"
    )

    expect_type (res, "list")
    expect_named (res, c ("issues_total", "issues_new"))
    expect_s3_class (res$issues_total, "data.frame")
    expect_s3_class (res$issues_new, "data.frame")
    # columns = editors
    expect_named (res$issues_total, c ("edA", "edB"))
    expect_named (res$issues_new, c ("edA", "edB"))
})

test_that ("editor_timeline quarter: row names are YYYY-MM with quarter-start months only", {

    res <- editor_timeline (
        tl_assignees, tl_state, tl_opened, tl_updated, tl_closed,
        aggregation_period = "quarter"
    )

    rn <- rownames (res$issues_total)
    expect_true (all (grepl ("^[0-9]{4}-(01|04|07|10)$", rn)))
})

test_that ("editor_timeline quarter: issues_total counts all active quarters", {

    res <- editor_timeline (
        tl_assignees, tl_state, tl_opened, tl_updated, tl_closed,
        aggregation_period = "quarter"
    )

    tot <- res$issues_total
    # Issue 1 (edA, Jan-May 2024) spans Q1 and Q2 → 2 quarter entries
    expect_equal (tot ["2024-01", "edA"], 1L)
    expect_equal (tot ["2024-04", "edA"], 1L)
    # Issue 3 (edA, Jul-Sep 2024) is in Q3
    expect_equal (tot ["2024-07", "edA"], 1L)
    # Issue 2 (edB, Apr-Jun 2024) only in Q2
    expect_equal (tot ["2024-04", "edB"], 1L)
    # edB not active in Q1 or Q3
    expect_equal (tot ["2024-01", "edB"], 0L)
    expect_equal (tot ["2024-07", "edB"], 0L)
})

test_that ("editor_timeline quarter: issues_new counts only opening quarter", {

    res <- editor_timeline (
        tl_assignees, tl_state, tl_opened, tl_updated, tl_closed,
        aggregation_period = "quarter"
    )

    nw <- res$issues_new
    # edA opened issues in Q1 (issue 1) and Q3 (issue 3)
    expect_equal (nw ["2024-01", "edA"], 1L)
    expect_equal (nw ["2024-04", "edA"], 0L) # issue 1 spans here but didn't start here
    expect_equal (nw ["2024-07", "edA"], 1L)
    # edB opened issue in Q2
    expect_equal (nw ["2024-04", "edB"], 1L)
    expect_equal (nw ["2024-01", "edB"], 0L)
})

test_that ("editor_timeline month: row names are YYYY-MM for every month", {

    res <- editor_timeline (
        list ("edA", "edB"),
        c ("CLOSED", "CLOSED"),
        c ("2024-01-01T00:00:00Z", "2024-02-01T00:00:00Z"),
        c ("2024-03-01T00:00:00Z", "2024-02-28T00:00:00Z"),
        c ("2024-03-01T00:00:00Z", "2024-02-28T00:00:00Z"),
        aggregation_period = "month"
    )

    rn <- rownames (res$issues_total)
    expect_true (all (grepl ("^[0-9]{4}-[0-9]{2}$", rn)))
    expect_true ("2024-01" %in% rn)
    expect_true ("2024-02" %in% rn)
    expect_true ("2024-03" %in% rn)
})

test_that ("editor_timeline semester: row names contain only Jan or Jul months", {

    res <- editor_timeline (
        list ("edA", "edB"),
        c ("CLOSED", "CLOSED"),
        c ("2024-02-15T00:00:00Z", "2024-07-15T00:00:00Z"),
        c ("2024-08-01T00:00:00Z", "2024-11-01T00:00:00Z"),
        c ("2024-08-01T00:00:00Z", "2024-11-01T00:00:00Z"),
        aggregation_period = "semester"
    )

    rn <- rownames (res$issues_total)
    expect_true (all (grepl ("^[0-9]{4}-(01|07)$", rn)))
    expect_true ("2024-01" %in% rn)
    expect_true ("2024-07" %in% rn)
})

test_that ("editor_timeline errors on invalid aggregation_period", {

    expect_error (
        editor_timeline (
            tl_assignees, tl_state, tl_opened, tl_updated, tl_closed,
            aggregation_period = "annual"
        )
    )
})

# ---------------------------------------------------------------------------
# full names query, using httptest2 mocks
# ---------------------------------------------------------------------------

test_that ("editor names", {

    q <- gh_editors_team_qry (stats = FALSE)
    editors <- httptest2::with_mock_dir ("editors", {
        gh::gh_gql (query = q)
    })
    editors <- editors$data$organization$team$members$nodes
    expect_true (length (editors) > 10L)
    nms <- unique (unlist (lapply (editors, names)))
    expect_identical (nms, "login") # should have 'login' only
    editors <- vapply (editors, function (i) i$login, character (1L))
    expect_type (editors, "character")
    expect_true (length (editors) > 10L)

    q <- gh_editors_team_qry (stats = TRUE)
    editors_stats <- httptest2::with_mock_dir ("editors-stats", {
        gh::gh_gql (query = q)
    })
    editors_stats <- editors_stats$data$organization$team$members$nodes
    expect_true (length (editors_stats) >= 5L)
    expect_true (length (editors_stats) < length (editors))
    nms <- unique (unlist (lapply (editors_stats, names)))
    expect_identical (nms, "login") # should have 'login' only
    editors_stats <- vapply (editors_stats, function (i) i$login, character (1L))
    expect_type (editors_stats, "character")
    expect_true (length (editors_stats) >= 5L)
})
