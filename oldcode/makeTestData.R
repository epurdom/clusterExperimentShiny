library(scRNAseq)
data(fluidigm)
se <- fluidigm[,colData(fluidigm)[,"Coverage_Type"]=="High"]
wh_zero <- which(rowSums(assay(se))==0)
pass_filter <- apply(assay(se), 1, function(x) length(x[x >= 10]) >= 10)
se <- se[pass_filter,]
fq <- round(limma::normalizeQuantiles(assay(se)))
assays(se) <- list(normalized_counts=fq)
#make small:
se<-se[1:100,1:10]
save(se,file="testSE.rda")
saveRDS(se,file="testSE_rds.rda",compress=FALSE)