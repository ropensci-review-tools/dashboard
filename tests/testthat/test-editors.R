test_that ("editor names", {

    q <- gh_editors_team_qry (stats = FALSE)
    editors <- gh::gh_gql (query = q)
    editors <- editors$data$organization$team$members$nodes
    expect_true (length (editors) > 10L)
    nms <- unique (unlist (lapply (editors, names)))
    expect_identical (nms, "login") # should have 'login' only
    editors <- vapply (editors, function (i) i$login, character (1L))
    expect_type (editors, "character")
    expect_true (length (editors) > 10L)

    q <- gh_editors_team_qry (stats = TRUE)
    editors_stats <- gh::gh_gql (query = q)
    editors_stats <- editors_stats$data$organization$team$members$nodes
    expect_true (length (editors_stats) > 5L)
    expect_true (length (editors_stats) < length (editors))
    nms <- unique (unlist (lapply (editors_stats, names)))
    expect_identical (nms, "login") # should have 'login' only
    editors_stats <- vapply (editors_stats, function (i) i$login, character (1L))
    expect_type (editors_stats, "character")
    expect_true (length (editors_stats) > 5L)
})
