(with-open-file (file "~/dict/lemmatized/2+2+3lem.txt")
    (loop for line = (read-line file nil)
        while line collect line))



