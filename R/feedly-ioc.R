#' @export
feedly_ioc <- function(stream_id,
                          newer_than = NULL,
                          older_than = NULL,
                          count = 1000L,
                          continuation = NULL,
                          feedly_token = feedly_access_token()) {

  ct <- as.integer(count)

  if (!is.null(continuation)) ct <- 1000L

  if (ct > 1000L) ct <- 1000L
  if (ct < 1L) ct <- 1000L

  #ranked <- match.arg(ranked[1], c("newest", "oldest"))

  # if (!is.null(newer_than)) {
  #   newer_than <- as.numeric(as.POSIXct(newer_than))*1000
  # }

  if (!is.null(newer_than)){
    newer_than <- as.integer(newer_than)
  }
  if (!is.null(older_than)){
    older_than <- as.integer(older_than)
  }

  httr::GET(
    url = "https://api.feedly.com/v3/enterprise/ioc",
    .seymour_ua,
    if (!is.null(feedly_token)) {
      httr::add_headers(
        `Authorization` = sprintf("OAuth %s", feedly_token)
      )
    },
    query = list(
      streamId = stream_id,
      #ranked = ranked,
      #unreadOnly = if (unread_only) "true" else "false",
      newerThan = newer_than,
      olderThan = older_than,
      count = ct,
      continuation = continuation
    )
  ) -> res

  httr::stop_for_status(res)

  res <- httr::content(res, as="text")
  res <- jsonlite::fromJSON(res)

  if (length(res$items) > 0) {
    if (nrow(res$items) > 0) {

      cn <- colnames(res$items)

      if ("updated" %in% cn) res$items$updated <- as.POSIXct(res$items$updated/1000, origin = "1970-01-01")
      if ("crawled" %in% cn) res$items$crawled <- as.POSIXct(res$items$crawled/1000, origin = "1970-01-01")
      if ("published" %in% cn) res$items$published <- as.POSIXct(res$items$published/1000, origin = "1970-01-01")
      if ("actionTimestamp" %in% cn) res$items$actionTimestamp <- as.POSIXct(res$items$actionTimestamp/1000, origin = "1970-01-01")

      # column bind all the data frame columns so we can make res$items a tbl
      # also prepend the old column name in front so we can ensure they won't
      # be overwritten or have conflicts (e.g. 'summary' has a 'content' component)

      isdf <- sapply(res$items, class)
      df_cols <- names(isdf[isdf == "data.frame"])

      for (cname in df_cols) {
        cn <- cn[cn != cname]
        x <- res$items[[cname]]
        colnames(x) <- sprintf("%s_%s", cname, colnames(x))
        res$items <- cbind.data.frame(res$items[,cn], x)
        cn <- c(cn, colnames(x))
      }

      colnames(res$items) <- tolower(colnames(res$items))

      class(res$items) <- c("tbl_df", "tbl", "data.frame")

    }
  }

  res

}
