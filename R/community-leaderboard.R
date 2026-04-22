#' Data for community leaderboard
#'
#' This is only slightly modified from \link{review_history}, through adding
#' additional Airtable data to identify current editorial team.
#'
#' @inheritParams review_history
#' @param airtable_id ID of Airtable base
#' @return A named list with one element for each community member and integer
#' values for total number of packages submitted; total number of reviews
#' conducted; and an additional logical flag indicating whether or not that
#' person is or was a member of the editorial team. These data are returned as
#' a raw list, because they are processed directly in Javascript.
#'
#' @export
community_data <- function (airtable_id, quiet = FALSE) {

    h <- m_review_history (quiet = quiet)

    rev_prod <- airtabler::airtable (
        base = airtable_id, table = "reviewers-prod"
    )
    fields <- list ("github", "name", "editor", "other_langs", "domain_expertise")
    rev_prod <- rev_prod$`reviewers-prod`$select_all (fields = fields)

    index <- which (!vapply (rev_prod$editor, is.null, logical (1L)))
    eds_gh <- rev_prod$github [index]

    members <- unique (c (h$aut_gh, h$editor, h$reviewer1, h$reviewer2))
    members <- members [which (!is.na (members))]
    community <- lapply (members, function (i) {
        index <- unique (na.omit (c (
            match (i, h$aut_gh),
            match (i, h$reviewer1),
            match (i, h$reviewer2)
        )))
        list (
            gh_handle = i,
            pkgs = h$number [which (h$aut_gh == i)],
            revs = h$number [which (h$reviewer1 == i | h$reviewer2 == i)],
            stats = any (h$stats [index]),
            ed = i %in% h$editor || i %in% eds_gh
        )
    })
    names (community) <- members

    ntot <- vapply (
        community,
        function (i) length (i$pkgs) + length (i$revs),
        integer (1L)
    )
    community <- community [which (ntot > 0)]

    return (community)
}
