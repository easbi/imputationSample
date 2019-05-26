#' Mengambil sampel di tiap kabupaten/kota 
#' @description Mengambil sampel di tiap kabupaten/kota dan memberi flag
#' @param data_awal Dataset yang digunakan.
#' @param filterku Filter data yang bisa diambil dari fungsi create_filter() dengan nama myfilter sebelumnya
#' @param weight_aggregate Aggregate weight yang user definisikan/tetapkan
#' @param weight_col Kolom yang berisi weight yang ingin digunakan
#' @param iter Jumlah Iterasi/Looping untuk menemukan susunan optimal sampel
#' @param sample_flag Penanda untuk yang terpilih sebagai sampel
#' @param ... Daftar mapping atribut dan nilai baru yang diinginkan (dapat dibuat sebanyak mungkin).
#' @return outputData = sebagai data yang sudah ditambahkan flag. passFilter = Kabupaten/Kota yang lolos Filter
#' @examples
#' my_filter <- create_filter(r612 == 3 & r610 == 1)
#' sakernas_dummy = mutate_sample(data_awal = X3_KOR18IND_untuk_reval_REV_EDUC_2, filterku = my_filter, weight_aggregate=4500, weight_col=FWT, iter=10, sample_flag=1)
#' @export
sampleEachArea <- function(data_awal, filterku, weight_aggregate, weight_col, iter, sample_flag=1){
  library(imputationSample)
  library(tidyr)
  weight_col = enquo(weight_col)
  results <- list(outputData = NA, passFilter = NA)
  
  dataSplit <- data_awal  %>% nest(-R101, -R102)
  tampung <- list() 
  my_filter <- filterku
  prov_pass <- vector()
  kab_pass <- vector()
  newdata = data.frame()
  for (i in 1:nrow(dataSplit)) {
    print(i)
    tampung[[i]] <- try(imputation_sample(as.data.frame(dataSplit$data[[i]]), filters = my_filter, weight_aggregate = weight_aggregate, weight_col = !!weight_col, iter = iter, sample_flag=1))
    #tampung[[i]] <- try(imputation_sample(as.data.frame(dataSplit$data[[i]]), filters = my_filter, weight_aggregate = 5000, weight_col = FWT, iter = 10, sample_flag=1))
    if (!is.data.frame(tampung[[i]])) {
      print("KAB/KOTA INI TIDAK MEMENUHI FILTER")
      prov_pass[i] <- dataSplit[i,]$R101
      kab_pass[i] <- dataSplit[i,]$R102
    }
    if (is.data.frame(tampung[[i]])) {
      newdata <- rbind(newdata, tampung[[i]])
    }
    
  }
  results$outputData = newdata
  passFilter = data.frame(
    KodeProv = prov_pass,
    KodeKab  = kab_pass
  )
  results$passFilter <- passFilter %>% drop_na()
  return(results)
}
