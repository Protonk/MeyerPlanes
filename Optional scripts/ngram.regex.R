ngram.in <- scan(file = file.path(getwd(), "Data", "ngram", "ngramtable.txt"), what = "list", sep = "\n")

terms.ngram <- mapply(function(x) unlist(strsplit(testl[seq(x, 33, by =  3)], ", +")), x = 1:3, SIMPLIFY = FALSE)
names(terms.ngram) <- c("English", "French", "German")
terms.ngram <- lapply(terms.ngram, tolower)

ngram.regex <- lapply(terms.ngram, function(X) paste(X, collapse = "|"))
