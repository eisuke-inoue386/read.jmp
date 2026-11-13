# Run in a fresh session from the repository root, without installing read.jmp.
source("read.jmp.R", encoding = "UTF-8")
stopifnot(!"package:read.jmp" %in% search())
x <- read.jmp("inst/extdata/upstream/example1.jmp", metadata = "quiet",
                   missing_codes = FALSE, factors = FALSE)
stopifnot(is.data.frame(x), identical(as.integer(x$ints), 1:4),
          identical(x[["char utf8"]][4], "\u8f9b\u53e3"),
          nrow(jmp_metadata(x)$diagnostics) > 0L)
x <- read_jmp("inst/extdata/upstream/compressed.jmp", metadata = "quiet")
stopifnot(nrow(x) == 10000L, all(x$numeric == 1), is.factor(x$character1))
y <- read.jmp("inst/extdata/embedded-example.jrp", metadata = "quiet")
stopifnot(is.data.frame(y), nrow(y) > 0L, is.na(y$score[2]),
          is.factor(y$group), is.ordered(y$severity))
check_rejection <- function(text, error_class) {
  path <- tempfile(fileext = ".jrp")
  on.exit(unlink(path))
  writeLines(text, path)
  error <- tryCatch(read.jmp(path), error = identity)
  stopifnot(inherits(error, error_class))
}
check_rejection('Open("external.jmp");', "jrp_external_reference_error")
check_rejection('Eval(Parse("New Table()"));', "jrp_dynamic_error")
cat("Standalone JMP/JRP reads and error handling passed.\n")

legacy <- new.env(parent = globalenv())
new <- new.env(parent = globalenv())
sys.source("jmp2r.R", envir = legacy)
sys.source("read.jmp.R", envir = new)
p <- "inst/extdata/embedded-example.jrp"
stopifnot(identical(legacy$read_jmp_file(p, metadata = "quiet"),
                    new$read.jmp(p, metadata = "quiet")))
