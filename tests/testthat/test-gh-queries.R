test_that ("gh_issues_qry default args", {

    q <- gh_issues_qry ()

    expect_type (q, "character")
    expect_length (q, 1L)

    # Default org / repo embedded in query
    expect_true (grepl ('owner:\\"ropensci\\"', q))
    expect_true (grepl ('name:\\"software-review\\"', q))

    # open_only = TRUE by default → states filter present
    expect_true (grepl ("states:\\s*\\[OPEN\\]", q))

    # No pagination cursor by default
    expect_false (grepl ("after:", q))

    # Required GraphQL fields
    for (field in c (
        "pageInfo", "hasNextPage", "endCursor", "edges",
        "number", "createdAt", "state", "assignees",
        "title", "labels", "body", "comments",
        "timelineItems", "LABELED_EVENT"
    )) {
        expect_true (grepl (field, q), label = paste0 ("field '", field, "' present"))
    }
})

test_that ("gh_issues_qry open_only = FALSE omits states filter", {

    q <- gh_issues_qry (open_only = FALSE)

    expect_false (grepl ("states:", q))
})

test_that ("gh_issues_qry with end_cursor includes after clause", {

    cursor <- "abc123XYZ"
    q <- gh_issues_qry (end_cursor = cursor)

    expect_true (grepl (paste0 ('after:\\"', cursor, '\\"'), q))
})

test_that ("gh_issues_qry with end_cursor and open_only = FALSE", {

    cursor <- "pageCursorValue"
    q <- gh_issues_qry (open_only = FALSE, end_cursor = cursor)

    expect_false (grepl ("states:", q))
    expect_true (grepl (paste0 ('after:\\"', cursor, '\\"'), q))
})

test_that ("gh_issues_qry custom org and repo", {

    q <- gh_issues_qry (org = "myorg", repo = "myrepo")

    expect_true (grepl ('owner:\\"myorg\\"', q))
    expect_true (grepl ('name:\\"myrepo\\"', q))
})

# ---------------------------------------------------------------------------

test_that ("gh_editors_team_qry string content — stats = FALSE", {

    q <- gh_editors_team_qry (stats = FALSE)

    expect_type (q, "character")
    expect_length (q, 1L)

    # Correct team slug
    expect_true (grepl ('"editors"', q))
    expect_false (grepl ("stats-editors", q))

    # Required GraphQL structure
    for (field in c ("organization", "team", "members", "login")) {
        expect_true (grepl (field, q), label = paste0 ("field '", field, "' present"))
    }
})

test_that ("gh_editors_team_qry string content — stats = TRUE", {

    q <- gh_editors_team_qry (stats = TRUE)

    expect_true (grepl ("stats-editors", q))

    for (field in c ("organization", "team", "members", "login")) {
        expect_true (grepl (field, q), label = paste0 ("field '", field, "' present"))
    }
})

# ---------------------------------------------------------------------------

test_that ("gh_issue_assignees_qry default args", {

    q <- gh_issue_assignees_qry ()

    expect_type (q, "character")
    expect_length (q, 1L)

    # Default org / repo
    expect_true (grepl ('owner:\\"ropensci\\"', q))
    expect_true (grepl ('name:\\"software-review\\"', q))

    # orderBy clause always present (see issue #5)
    expect_true (grepl ("orderBy", q))
    expect_true (grepl ("CREATED_AT", q))

    # No cursor by default
    expect_false (grepl ("after:", q))

    # Required fields
    for (field in c (
        "pageInfo", "hasNextPage", "endCursor", "nodes",
        "number", "state", "createdAt", "closedAt",
        "updatedAt", "assignees", "title", "url"
    )) {
        expect_true (grepl (field, q), label = paste0 ("field '", field, "' present"))
    }
})

test_that ("gh_issue_assignees_qry with end_cursor", {

    cursor <- "cursor99"
    q <- gh_issue_assignees_qry (end_cursor = cursor)

    expect_true (grepl (paste0 ('after:\\"', cursor, '\\"'), q))
    # orderBy still present alongside the cursor
    expect_true (grepl ("orderBy", q))
})

test_that ("gh_issue_assignees_qry custom org and repo", {

    q <- gh_issue_assignees_qry (org = "testorg", repo = "testrepo")

    expect_true (grepl ('owner:\\"testorg\\"', q))
    expect_true (grepl ('name:\\"testrepo\\"', q))
})

# ---------------------------------------------------------------------------

test_that ("gh_issues_qry_dates_states default args", {

    q <- gh_issues_qry_dates_states ()

    expect_type (q, "character")
    expect_length (q, 1L)

    # Default org / repo
    expect_true (grepl ('owner:\\"ropensci\\"', q))
    expect_true (grepl ('name:\\"software-review\\"', q))

    # No cursor by default
    expect_false (grepl ("after:", q))

    # Required fields
    for (field in c (
        "pageInfo", "hasNextPage", "endCursor", "edges",
        "number", "createdAt", "closedAt", "state",
        "body", "labels", "assignees"
    )) {
        expect_true (grepl (field, q), label = paste0 ("field '", field, "' present"))
    }
})

test_that ("gh_issues_qry_dates_states with end_cursor", {

    cursor <- "dateCursor42"
    q <- gh_issues_qry_dates_states (end_cursor = cursor)

    expect_true (grepl (paste0 ('after:\\"', cursor, '\\"'), q))
})

test_that ("gh_issues_qry_dates_states custom org and repo", {

    q <- gh_issues_qry_dates_states (org = "anotherorg", repo = "anotherrepo")

    expect_true (grepl ('owner:\\"anotherorg\\"', q))
    expect_true (grepl ('name:\\"anotherrepo\\"', q))
})
