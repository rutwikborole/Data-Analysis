

#Write a function to convert data frame into List.
conv_df_to_list <- function (myvar){

  my_list <- vector("list", nrow(myvar))
  
  for (i in 1:nrow(myvar)){
    names(my_list) <- paste0("R-", seq_len(i))
    
    my_list[[i]] <- as.list(myvar[i,])
  }
  str(my_list)
}

#print information stored in the Formaldehyde Data set.
conv_df_to_list(Formaldehyde)

#print information stored in the mtcars Data set.
conv_df_to_list(mtcars)


