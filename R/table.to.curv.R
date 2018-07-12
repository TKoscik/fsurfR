table.to.curv <- function(in.table,
                          n,
                          surf.file,
                          coords=NULL,
                          save.dir,
                          prefix,
                          model) {
  
  # ----------------------------------------------------------------------------
  # Copyright (C) 2018 Koscik, Timothy R. All Rights Reserved
  # ----------------------------------------------------------------------------
  
  var.name <- deparse(subsitute(in.table)) # retriev table name
  
  if (!is.matrix(in.table) | !is.data.frame(in.table)) {
    in.table <- as.matrix(in.table)
  }
  
  log.name <- paste0(save.dir, "/", prefix, ".", var.name, ".log.txt")
  if (!file.exists(log.name)) {
    if (!missing(model)) {
      write.table(paste0("Model: ", deparse(formula(model))), log.name,
                  append=TRUE, quote=FALSE, sep=",",
                  row.names=FALSE, col.names=FALSE)
    } else {
      write.table("Model: NA", log.name,
                  append=TRUE, quote=FALSE, sep=",",
                  row.names=FALSE, col.names=FALSE)
    }
    write.table(paste0("Table: ", var.name), log.name,
                append=TRUE, quote=FALSE, sep=",",
                row.names=FALSE, col.names=FALSE)
    write.table(paste0("NII File: ", colnames(in.table)), log.name,
                append=TRUE, quote=FALSE, sep=",",
                row.names=FALSE, col.names=FALSE)
    write.table(paste0("Volume ", 1:nrow(in.table), ": ", rownames(in.table)),
                log.name,
                append=TRUE, quote=FALSE, sep=",",
                row.names=FALSE, col.names=FALSE)
  }
  
  for (i in 1:ncol(in.table)) {
    for (j in 1:nrow(in.table)) {
      if (!is.null(colnames(in.table))) {
        col.suffix <- colnames(in.table)[i]
        col.suffix <- gsub("[[:punct:]]", "", col.suffix) # Remove invalid characters
        col.suffix <- gsub("[[:space:]]", "", col.suffix)
      } else {
        col.suffix <- paste0("X", i) # if table is unnamed call it X
      }
      
      if (!is.null(rownames(in.table))) {
        row.suffix <- rownames(in.table)[j]
        row.suffix <- gsub("[:]", "X", row.suffix)
        row.suffix <- gsub("[[:punct:]]", "", row.suffix) # Remove invalid characters
        row.suffix <- gsub("[[:space:]]", "", row.suffix)
      } else {
        row.suffix <- paste0("X", i) # if table is unnamed call it X
      }
      
      fname <- paste0(save.dir, "/", prefix, ".", var.name, ".", col.suffix, ".", row.suffix)
      
      if (!file.exists(fname)) {
        init.curv(file.name=fname, surf.file = surf.file)
      }
      if (!is.na(in.table[j,i])) {
        if (is.null(coords)) {
          write.curv.vertex(fname, n, value=in.table[j,i])
        } else {
          write.curv.vertex(fname, value=in.table[j,i],
                            surf.file=surf.file, coords = coords)
        }
      }
    }
  }
}