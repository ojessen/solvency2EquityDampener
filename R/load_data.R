options(stringsAsFactors = FALSE)
setInternet2(TRUE)

load_data = function(to = Sys.Date(), index_para){
  library(quantmod)

  from = seq.Date(to, by = "-3 years", length.out = 2)[2] + 1

  for(nm in 1:nrow(index_para)){
    tmp = getSymbols(index_para$symbol[nm], to = to, from = from, src = index_para$src[nm], auto.assign = FALSE)
    tmp_close = tmp[,4]

    if(any(is.na(index(tmp_close)))){
      for(idx in which(is.na(index(tmp_close)))){
        index(tmp_close)[idx]=getNextWorkday(index(tmp_close)[idx-1])
      }
    }


    if(nm == 1){
      res = tmp_close
    } else{
      res = merge(res,tmp_close)
    }
  }
  names(res) = index_para$name

  for(cl in 1:ncol(res)){
    if(any(is.na(res[,cl]))){
      for(idx in which(is.na(res[,cl]))){
        if(idx == 1) {
          res[idx,cl]=res[min(which(!is.na(res[,cl]))),cl]
        }else{
          res[idx,cl]=res[idx-1,cl]
        }

      }
    }
  }

  res_2 = apply(res,2,function(x){x/x[1]})

  res_3 = apply(res_2,1,function(x){sum(x*as.numeric(index_para$wheight))})
  res_3 = xts(res_3, order.by =as.Date(names(res_3)))
  res_3
}

load_index_para = function(){
  read.csv2("inst/index_para.csv")
}

getNextWorkday = function(prior_day){
  prior_day = as.POSIXlt(prior_day)
  wday = 0
  while(!wday %in% 1:5){
    next_day = seq(from=prior_day,length.out = 2, by = "1 day")[2]
    prior_day = next_day
    wday = as.POSIXlt(next_day)$wday
  }
  as.character(next_day)
}


