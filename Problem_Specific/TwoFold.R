twofold = function() {
  accounts_original=runif(10000,100,100000) #Original accounts with untruncated values
  accounts =  floor(accounts_original*100)/100 #Accounts with truncated values
  diff = sum(accounts_original - accounts) #truncated fractions for all accounts
  illegal = 0 #the illegal account has initial value of 0
  
  days = 0 #This count the days
  
  while (illegal < 1000000){
    illegal = illegal+diff
    accounts_original = accounts+accounts*(.05/365)
    accounts =  floor(accounts_original*100)/100
    #We does not need to truncate illegal because it gets its own truncated money bac
    diff = sum(accounts_original - accounts)
    days = days+1
    #print(days) for debugging
  } 
  return(days)
}