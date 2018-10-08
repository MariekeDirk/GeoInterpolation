
model_path<-"/nobackup/users/dirksen/data/Temperature/Aux_results/linear_mod/Model/"

lm_files<-list.files(model_path,pattern = ".rds",full.names = TRUE)

get_coef<-function(model){
lm_model<-readRDS(model)
df<-t(data.frame(lm_model$finalModel$coefficients))
return(df)
}

ls_coef<-lapply(lm_files,get_coef)
ls_coef<-do.call("rbind",ls_coef)

write.table(ls_coef,
            file = "/nobackup/users/dirksen/data/Temperature/Aux_results/linear_mod/beta_coef.txt",
            row.names = FALSE,
            col.names = TRUE,
            sep = ","
            )

library(data.table)
ls_coef<-fread("/nobackup/users/dirksen/data/Temperature/Aux_results/linear_mod/beta_coef.txt")
ls_coef<-data.frame(ls_coef)
ls_coef_abs<-abs(ls_coef)
ls_coef_abs$`(Intercept)`<-NULL

coef_sum<-colSums(ls_coef_abs,na.rm=TRUE)
rel_var_imp<-(coef_sum/sum(coef_sum))*100
