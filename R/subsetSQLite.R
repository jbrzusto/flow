#' Create a new SQLite database by querying an existing one.
#'
#' Given a sequence of select queries, apply them to an
#' existing SQLite database, and save the resulting tables in a new
#' SQLite database.  This is done quickly by creating a temporary
#' in-memory database, writing results to it, then copying that
#' database to the destination .sqlite file.
#'
#' @param inDB: character scalar, path to existing .sqlite database
#' 
#' @param queries: named character vector, items are queries, names
#' are names of tables to create in new database.  The new table is
#' filled with the results from the corresponding query on the
#' existing database.
#' 
#' @param outDB: character scalar, path to new .sqlite database
#'
#' @return TRUE on success.  FALSE otherwise.
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

subsetSQLite = function(inDB, queries, outDB) {
    if (! file.exists(inDB))
        stop("non-existent input database")
    if (! is.character(queries) || length(names(queries)) == 0)
        stop("queries must be a character vector with names")
    con = dbConnect("SQLite", inDB)
    dbGetQuery(con, "attach database 'file::memory:?cache=shared' as tmp")
    for (q in names(queries))
        dbGetQuery(con, sprintf("create table tmp.%s as %s", q, queries[[q]]))
    ocon = dbConnect("SQLite", "file::memory:?cache=shared")
    sqliteCopyDatabase(ocon, outDB)
    dbDisconnect(ocon)
    dbDisconnect(con)
}
