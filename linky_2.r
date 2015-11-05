final_df<-data.frame(name="",current_location="",current_job_title="",industry="",bsc_university="",bsc_major="",bsc_duration="",first_job_title="",first_company="",first_job_duration="",msc_university="",msc_major="",msc_duration="",job_hops="",summary="")
#now converting all of these into character format first.
final_df[,1:ncol(final_df)]<-sapply(final_df[,1:ncol(final_df)],as.character)

#write.table(final_df,file="linkedin_results_fast_it_pk_1.csv",sep=",",row.names=FALSE,col.names=FALSE)

require(RSelenium)
RSelenium::checkForServer()
RSelenium::startServer()
remdr<-remoteDriver()
open_page<-remdr$open() #will open the browser
navigate_page<-remdr$navigate("https://www.linkedin.com/vsearch/p?openAdvancedForm=true&locationType=Y&rsid=597444871438863819012&orig=FCTD&pt=people&f_I=84,118,3,5,109,96&f_ED=15865&f_G=pk%3A0&page_num=3&f_N=S&openFacets=N,G,CC,I,ED")

# FAST: -COMPUTERhttps://www.linkedin.com/vsearch/p?openAdvancedForm=true&locationType=Y&rsid=597444871438862749033&orig=FCTD&pt=people&f_I=84,118,3,5,109,96&f_ED=15865&f_G=pk%3A0&openFacets=N,G,CC,I,ED&f_N=S

#https://www.linkedin.com/vsearch/p?openAdvancedForm=true&locationType=Y&f_ED=15891&rsid=597444871434112710513&orig=ADVS&page_num=2&pt=people&openFacets=N,G,CC,ED&f_N=F,S,A,O")


total_results<-tryCatch({
  remdr$findElement(using="xpath",value="//div[@class='search-info']//strong")
},error=function(err){
  print("Error in total_results")
  total_results<-1000
})


total_results<-unlist(total_results$getElementText())
total_results<-sub(",","",total_results)
total_results<-as.integer(total_results)
#pages_number<-total_results/10
current_page=3
pages_number<-current_page+100

user_result_param_1="//li[@data-li-position='"
user_result_param_2="']//a[@class='title main-headline']" #added main-headline in this parameter. 8/6/2015

next_page_param="//*/a[@title='Page "
p_num=3
next_page_param_2="']"
next_page_param_full<-paste0(next_page_param,p_num,next_page_param_2)
wait_time<-c(1000,1500,2500,2000,3000,3500,4000,4500,5000,5500,7000,3800)
current_url<-unlist(remdr$getCurrentUrl())
k=0

while(current_page<pages_number){
  remdr$setImplicitWaitTimeout(milliseconds = sample(wait_time,1))  
  k=0
  while(k<9){
    final_user_result_param<-paste0(user_result_param_1,k,user_result_param_2)
    
    user_names<-tryCatch({
      remdr$findElement(using="xpath",value=final_user_result_param)
    },error=function(err){
      print("error in user_names. Replacing with NA")
      NA
    })
    
    user_names$clickElement()
    
    #now you are in a particular user's profile:
    #user_profile_scrape<-function(){
    remdr$setImplicitWaitTimeout(milliseconds = sample(wait_time,1))
    
    
    user_name<-tryCatch({
      remdr$findElement(using="xpath",value="//h1")
    },error=function(err){
      NA
      
    })
    if(remdr$status==0){
      print(user_name<-unlist(user_name$getElementText()))
    }
    #Industry:
    industry<-tryCatch({
      remdr$findElement(using="xpath",value="//dd[@class='industry']//a")
      
    },error=function(err){
      NA
      
    })
    
    if(remdr$status==0){
    print(industry<-unlist(industry$getElementText()))
    }
    
    current_location<-tryCatch({
      
      remdr$findElement(using="xpath",value="//div[@id='location']//span[@class='locality']//a")
    }, error=function(err){
      NA
    })
    if (remdr$status==0){
      current_location<-unlist(current_location$getElementText())
    }
    
    summary<-tryCatch({
      remdr$findElement(using="xpath",value="//div[@id='summary-item']")
      
    },error=function(err){
      NA
      })
    
    if(remdr$status==0){
    print(summary<-unlist(summary$getElementText()))
    }
    
    experience_container<-tryCatch({
      remdr$findElement(using="xpath",value="//div[@id='background-experience']")
     
    },error=function(err){
      NA      
    })
    
    if(remdr$status==0){
      experience_content<-experience_container$getElementText()
      experience_split<-strsplit(experience_content[[1]],"\n")
      experience_split<-unlist(experience_split)
      print(job_hops<-length(grep("month",experience_split)))
    }
   
    job_titles<-tryCatch({
      remdr$findElements(using="xpath", value="//div[@id='background-experience']//h4")
     
    },error=function(err){
      NA
    })
    
    if(remdr$status==0){
      job_titles_list<-unlist(lapply(job_titles,function(x)x$getElementText()))
      print(current_job_title<-job_titles_list[1])
      print(first_job_title<-job_titles_list[length(job_titles_list)])
    }else{
      current_job_title<-NA
      first_job_title<-NA
    }
    
    
    companies<-tryCatch({
      remdr$findElements(using="xpath",value="//div[@id='background-experience']//h5//a[@dir='auto']")
     
    },error=function(err){
      NA
    })
    
    if(remdr$status==0){
      companies_list<-unlist(lapply(companies,function(x)x$getElementText()))
      current_company<-companies_list[1]
      first_company<-companies_list[length(companies_list)]
    }else{
      current_company<-NA
      first_company<-NA
    }
    

    job_duration<-tryCatch({
      remdr$findElements(using="xpath",value="//div[@id='background-experience']//span[@class='experience-date-locale']")
     
    },error=function(err){
      NA
    })
    
    if(remdr$status==0){
      job_duration_list<-unlist(lapply(job_duration,function(x)x$getElementText()))
      first_job_duration<-job_duration_list[1]
    }else{
      first_job_duration<-NA
    }
        

    universities<-tryCatch({
      remdr$findElements(using="xpath",value="//div[@id='background-education']//h4")
    
    },error=function(err){
      NA
    })
    
    if(remdr$status==0){
      universities_list<-unlist(lapply(universities,function(x) x$getElementText()))
    }
    
    degrees<-tryCatch({
      remdr$findElements(using="xpath",value="//div[@id='background-education']//span[@class='degree']")
      #degrees_list<-unlist(lapply(degrees,function(x)x$getElementText()))
    
    },error=function(err){
      NA
    })
    if(remdr$status==0){
        degrees_list<-unlist(lapply(degrees,function(x)x$getElementText()))
    }
    
    majors<-tryCatch({
      remdr$findElements(using="xpath",value="//div[@id='background-education']//span[@class='major']")
   
    },error=function(err){
      NA
    })
    if(remdr$status==0){
      majors_list<-unlist(lapply(majors,function(x)x$getElementText()))
    }
    
    
    education_duration<-tryCatch({
      remdr$findElements(using="xpath",value="//div[@id='background-education']//span[@class='education-date']")
    
    },error=function(err){
      NA
    })
    if(remdr$status==0){
      education_duration_list<-unlist(lapply(education_duration,function(x)x$getElementText()))
    }
    
    #finding BSc degree details:
    bsc_index<-which(regexpr("[Bb][.][Ss][Cc]|[Bb][Sc][Cc]|[Bb]achelors|[Bb][,][Ss][Cc]|[Bb].[Ss][ ][Cc]|[Bb][.][Ss][.][Ee]|[Bb][.][Ss][.][ ][Ee]|^[Bb][Ee]",degrees_list)>=1)
    if(length(bsc_index)>0){
      bsc_university<-universities_list[bsc_index]
      bsc_major<-majors_list[bsc_index]
      bsc_duration<-education_duration_list[bsc_index]
    }else if(length(grep("University of the Punjab, Lahore",universities_list))>=1){
      bsc_index<-which(regexpr("University of the Punjab, Lahore",universities_list)>=1)
      if(nchar(education_duration_list[bsc_index])>0){
          bsc_time_string<-education_duration_list[bsc_index]
          bsc_time_split<-strsplit(bsc_time_string," ")
          bsc_time_split2<-bsc_time_split[[1]]
          bsc_time_start<-as.numeric(bsc_time_split2[1])
          bsc_time_end<-as.numeric(bsc_time_split2[3])
          if (!is.na(bsc_time_end) & !is.na(bsc_time_start)){
              if ((bsc_time_end-bsc_time_start)==4){
                bsc_university<-universities_list[bsc_index]
                bsc_major<-majors_list[bsc_index]
                bsc_duration<-education_duration_list[bsc_index]
                
              } 
          }
    }
    }else {
      bsc_university<-NA
      bsc_major<-NA
      bsc_duration<-NA
    }
    print(c(bsc_university,bsc_major))
    #find MSc degree details:
    msc_index<-which(regexpr("M[.]SC|[Mm][Sc][Cc]|[Mm]aster|[Mm][,][Ss][Cc]|^[Mm][Ss]",degrees_list)>=1)
    
    if (length(msc_index)>0){
      msc_university<-universities_list[msc_index]
      msc_major<-majors_list[msc_index]
      msc_duration<-education_duration_list[msc_index]
    }else {
      msc_university<-NA
      msc_major<-NA
      msc_duration<-NA
    }
    print(c(msc_index,msc_major))
    user_row<-c(user_name,current_location,current_job_title,industry,bsc_university,bsc_major,bsc_duration,first_job_title,first_company,first_job_duration,msc_university,msc_major,msc_duration,job_hops,summary)
    #user_row #value returned
    final_df<-rbind(final_df,user_row) #user row added to the final dataframe.
    write.table(final_df[nrow(final_df),],append=TRUE,file="linkedin_results_fast_it_pk_1.csv",sep=",",row.names=FALSE,col.names=FALSE)
    paste(user_name,"scraped successfully")
    #remdr$goBack()
    remdr$setImplicitWaitTimeout(milliseconds = sample(wait_time,1))
    remdr$navigate(current_url)
    
    
    k=k+1 #grab next user.
  }
  #proceed to next page:
  current_page<-current_page+1
  next_page_param_full<-paste0(next_page_param,current_page,next_page_param_2)
  
  next_page<-tryCatch({
    remdr$findElement(using="xpath",value=next_page_param_full)
  },error=function(err){
    NA
  })
  next_page$clickElement()
  remdr$setImplicitWaitTimeout(milliseconds = sample(wait_time,1))  
  current_url<-unlist(remdr$getCurrentUrl())
}