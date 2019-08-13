###Datasets are loaded in before hand  


### Determining classses of high revenue within haccs

rev=comb%>%select(class_id,haccs,rev,class_nm,cpi)

acc_rev=rev%>%filter(haccs=='Accessories')%>%mutate(total_rev=sum(rev,na.rm=T),pct_rev=rev/total_rev)

hard_rev=rev%>%filter(haccs=='Hardware')%>%mutate(total_rev=sum(rev,na.rm=T),pct_rev=rev/total_rev)

x=rbind(acc_rev,hard_rev)
top_classes_rev=x%>%filter(pct_rev >= .005)



####Is previous WoW change greater than avg Wow change


test=data1%>%select(haccs,class_id,class_nm,fisc_wk_of_mth_id,fisc_wk_of_yr_nbr,r_product,bby_unit_product)%>%group_by(haccs,class_id,class_nm,fisc_wk_of_mth_id,fisc_wk_of_yr_nbr)%>%
  summarize(r_product=sum(r_product,na.rm=T),bby_unit_product=sum(bby_unit_product,na.rm=T))%>%mutate(cpi=round((r_product/bby_unit_product)*100,digits = 2))#%>%

test$diff=lag(test$cpi)

test=test%>%mutate(diff=ifelse(fisc_wk_of_mth_id==min(test$fisc_wk_of_mth_id),NA,diff))%>%drop_na(diff)%>%
  mutate(change=cpi-diff)

abs_change=test%>%group_by(class_id,class_nm,haccs)%>%mutate(change=abs(change))%>%drop_na(change)
Q1=quantile(abs_change$change)[2]
Q3=quantile(abs_change$change)[4]


avg_change=test%>%group_by(class_id,class_nm,haccs)%>%mutate(avg_WoW=mean(abs(change)))
this_week=max(test$fisc_wk_of_mth_id)

lrg_chng=avg_change%>%filter(fisc_wk_of_mth_id==this_week)%>%
  filter(abs(change)>avg_WoW & abs(change)>2)


##Getting database of all the classes

#top_classes=top_classes_rev%>%filter(class_id %in% lrg_chng$class_id)%>%arrange(desc(pct_rev))%>%head(10)

top_classes=merge(lrg_chng,top_classes_rev,by.y='class_id', by.x='class_id')%>%arrange(desc(pct_rev))%>%head(5)%>%
  rename(haccs=haccs.x,class_nm=class_nm.x,cpi=cpi.x)

###Output for top class contributions

wow0=matrix()
for (i in unique(top_classes$haccs)){
  print(i)
  select_haccs=top_classes%>%filter(haccs==i)%>%arrange(desc(abs(change)))
  haccs_change=summary%>%filter(haccs==i)%>%arrange(desc(abs(pct_rev_tw)))
  class_contributors=data.frame()
  for (j in unique(select_haccs$class_id)){
    select_class=select_haccs%>%filter(class_id==j)
    
    haccs=unique(select_class$haccs)
    pct_rev=round(unique(select_class$pct_rev),4)
    class_id=unique(select_class$class_id)
    class_nm=unique(select_class$class_nm)
    class_cpi_chng=round(unique(select_class$change),1)
    sentence=print(paste0(" Class ",class_id," ",class_nm," with WoW CPI change of ",class_cpi_chng," points, accounting for ",pct_rev*100, "% of revenue in ", haccs,"."))
    class_contributors=paste(class_contributors,sentence,sep="")
  }
  haccs_1=unique(select_haccs$haccs)
  cpi_cng=round(unique(haccs_change$diff_haccs),2)
  sentence_2=print(paste0("HACCS ",haccs_1," CPI changed WoW by ",cpi_cng," points. Top Classes that contributed to CPI change are: ",class_contributors))
  wow=rbind(wow0,sentence_2)
}



###Determing vendors driving revenue 
min_wk=min(data$fisc_wk_of_mth_id)
max_wk=max(data$fisc_wk_of_mth_id)

top_10=data%>%group_by(haccs,class_id,class_nm)%>%filter(haccs != 'Other')%>%summarize(rev=sum(rev,na.rm=T))%>%arrange(desc(rev))%>%head(n=10)

lw_cpi_class=data%>%select(fisc_wk_of_mth_id,class_id,class_nm,haccs,rev,bby_product,comp_product)%>%filter(fisc_wk_of_mth_id==min_wk & haccs != 'Other')%>%
  group_by(class_id,class_nm,haccs)%>%summarize(bby_product=sum(bby_product,na.rm=T),comp_product=sum(comp_product,na.rm=T),rev=sum(rev,na.rm=T))%>%
  mutate(lw_class_cpi=(comp_product/bby_product)*100)%>%rename(lw_class_rev=rev)%>%
  drop_na(lw_class_cpi)%>%select(class_id,class_nm,haccs,lw_class_cpi,lw_class_rev)

lw_cpi_vndr=data%>%select(fisc_wk_of_mth_id,class_id,class_nm,haccs,mstr_vndr_nm,rev,bby_product,comp_product)%>%
  filter(fisc_wk_of_mth_id==min_wk & haccs != 'Other')%>%
  group_by(class_id,class_nm,mstr_vndr_nm,haccs)%>%summarize(bby_product=sum(bby_product,na.rm=T),comp_product=sum(comp_product,na.rm=T),rev=sum(rev,na.rm=T))%>%
  mutate(lw_vndr_cpi=(comp_product/bby_product)*100)%>%rename(lw_vndr_rev=rev)%>%drop_na(lw_vndr_cpi)%>%select(class_id,class_nm,haccs,mstr_vndr_nm,lw_vndr_rev,lw_vndr_cpi)


tw_cpi_class=data%>%select(fisc_wk_of_mth_id,class_id,class_nm,haccs,rev,bby_product,comp_product)%>%filter(fisc_wk_of_mth_id==max_wk & haccs != 'Other')%>%
  group_by(class_id,class_nm,haccs)%>%summarize(bby_product=sum(bby_product,na.rm=T),comp_product=sum(comp_product,na.rm=T),rev=sum(rev,na.rm=T))%>%
  mutate(tw_class_cpi=(comp_product/bby_product)*100)%>%rename(tw_class_rev=rev)%>%
  drop_na(tw_class_cpi)%>%select(class_id,class_nm,haccs,tw_class_cpi,tw_class_rev)


tw_cpi_vndr=data%>%select(fisc_wk_of_mth_id,class_id,class_nm,haccs,mstr_vndr_nm,rev,bby_product,comp_product)%>%
  filter(fisc_wk_of_mth_id==max_wk & haccs != 'Other')%>%
  group_by(class_id,class_nm,mstr_vndr_nm,haccs)%>%summarize(bby_product=sum(bby_product,na.rm=T),comp_product=sum(comp_product,na.rm=T),rev=sum(rev,na.rm=T))%>%
  mutate(tw_vndr_cpi=(comp_product/bby_product)*100)%>%rename(tw_vndr_rev=rev)%>%drop_na(tw_vndr_cpi)%>%select(class_id,class_nm,haccs,mstr_vndr_nm,tw_vndr_rev,tw_vndr_cpi)


diff_cpi_class=merge(lw_cpi_class,tw_cpi_class)%>%mutate(cpi_diff_class=round((tw_class_cpi-lw_class_cpi),2))

diff_cpi_vndr=merge(lw_cpi_vndr,tw_cpi_vndr)%>%mutate(cpi_diff_vndr=round((tw_vndr_cpi-lw_vndr_cpi),2))

######### Variance
test=data1%>%group_by(haccs,class_id,class_nm,fisc_wk_of_mth_id,mstr_vndr_nm)%>%
  summarize(r_product=sum(r_product,na.rm=T),bby_unit_product=sum(bby_unit_product,na.rm=T))%>%mutate(cpi=round((r_product/bby_unit_product)*100,digits = 2))%>%
  mutate(diff=lag(cpi),change=(cpi-diff))%>%drop_na(change)


abs_change=test%>%group_by(class_id,class_nm,haccs)%>%mutate(change=abs(change))

avg_change=test%>%group_by(class_id,class_nm,haccs,mstr_vndr_nm)%>%mutate(avg_WoW=mean(abs(change)))
this_week=max(test$fisc_wk_of_mth_id)

lrg_chng=avg_change%>%filter(fisc_wk_of_mth_id==this_week)%>%
  filter(abs(change)>avg_WoW & abs(change)>2)

##### Top Vendors
fnl_vndr_cpi=merge(diff_cpi_class,diff_cpi_vndr)%>%select(class_id,class_nm,haccs,mstr_vndr_nm,lw_vndr_rev,lw_vndr_cpi,tw_vndr_rev,tw_vndr_cpi,cpi_diff_vndr,lw_class_cpi,lw_class_rev,tw_class_rev,tw_class_cpi,cpi_diff_class)%>%filter(class_id %in% top_10$class_id )%>%
  mutate(tw_pct_vndr_rev=(tw_vndr_rev/tw_class_rev),lw_pct_vndr_rev=(tw_vndr_rev/tw_class_rev))%>%
  mutate(contrib=cpi_diff_vndr*tw_pct_vndr_rev)

fnl_vndr_cpi=fnl_vndr_cpi%>%filter(class_id %in% lrg_chng$class_id)


top_vendors=fnl_vndr_cpi%>%filter(abs(cpi_diff_vndr) > 2 & abs(contrib) > .5)

##### Output for Vendors

biggest_vendors=matrix()
for (i in unique(top_vendors$class_id)){
  print(i) 
  select_class=top_vendors%>%filter(class_id==i)%>%arrange(desc(abs(contrib)))
  class_contributors=data.frame()
  for (j in unique(select_class$mstr_vndr_nm)){
    select_vndr=select_class%>%filter(mstr_vndr_nm==j) 
    
    cpi_diff_vndr=unique(select_vndr$cpi_diff_vndr)
    vndr_rev=round(unique(select_vndr$tw_pct_vndr_rev),3)
    mstr_vndr_nm=unique(select_vndr$mstr_vndr_nm)
    sentence=(paste0(" Vendor ",mstr_vndr_nm," with WoW CPI change of ",cpi_diff_vndr," points and accounting for " ,vndr_rev*100, "% of revenue in the class this week." ))
    class_contributors=paste(class_contributors,sentence,sep="")
  }
  class_cpi_chng=unique(select_vndr$cpi_diff_class)
  class_id=unique(select_vndr$class_id)
  class_nm=unique(select_vndr$class_nm)
  sentence_2=print(paste0("Class ",class_id, " ", class_nm, " CPI changed WoW by ",class_cpi_chng," points. Top Vendor(s) that contributed to CPI change are: ",class_contributors))
  results=rbind(biggest_vendors,sentence_2)
}


### Top 10 sku's 
min_wk=min(data$fisc_wk_of_mth_id)
max_wk=max(data$fisc_wk_of_mth_id)


lw_sku_cpi=data%>%select(sku_id,fisc_wk_of_mth_id,class_id,class_nm,haccs,bby_product,comp_product)%>%filter(fisc_wk_of_mth_id==min_wk & haccs != 'Other')%>%
  group_by(sku_id)%>%summarize(bby_product=sum(bby_product,na.rm=T),comp_product=sum(comp_product,na.rm=T))%>%
  mutate(lw_sku_cpi=(comp_product/bby_product)*100)%>%drop_na(lw_sku_cpi)%>%select(-bby_product,-comp_product)

tw_sku_cpi=data%>%select(sku_id,fisc_wk_of_mth_id,class_id,class_nm,haccs,rev,bby_product,comp_product)%>%filter(fisc_wk_of_mth_id==max_wk & haccs != 'Other')%>%
  group_by(sku_id)%>%summarize(bby_product=sum(bby_product,na.rm=T),comp_product=sum(comp_product,na.rm=T))%>%
  mutate(tw_sku_cpi=(comp_product/bby_product)*100)%>%drop_na(tw_sku_cpi)%>%select(-bby_product,-comp_product)

diff_sku_cpi=merge(lw_sku_cpi,tw_sku_cpi)%>%mutate(diff_sku_cpi=(tw_sku_cpi-lw_sku_cpi))%>%filter(abs(diff_sku_cpi) < 5)%>%group_by(sku_id)

total_rev=data%>%group_by(class_id)%>%summarize(total_rev=sum(rev))%>%drop_na(total_rev)


sku_rev=data%>%group_by(sku_id,class_id)%>%summarize(rev=sum(rev,rm.na=T))

sku_pct_rev=merge(sku_rev,total_rev)

#x=sku_pct_rev%>%group_by(sku_id)%>%summarize(count=n())

sku_pct_rev=sku_pct_rev%>%drop_na(rev)%>%mutate(pct_rev=rev/total_rev)

index_sku=merge(sku_pct_rev,diff_sku_cpi)%>%group_by(sku_id)%>%mutate(index=pct_rev*diff_sku_cpi)

index_sku$pct_rev[is.infinite(index_sku$pct_rev)]=0
index_sku$index[is.infinite(index_sku$index)]=0

#####Variance

test=data1%>%group_by(haccs,class_id,class_nm,fisc_wk_of_mth_id,mstr_vndr_nm,sku_id)%>%
  summarize(r_product=sum(r_product,na.rm=T),bby_unit_product=sum(bby_unit_product,na.rm=T))%>%mutate(cpi=round((r_product/bby_unit_product)*100,digits = 2))%>%
  mutate(diff=lag(cpi),change=(cpi-diff))%>%drop_na(change)

avg_change=test%>%group_by(class_id,class_nm,haccs,mstr_vndr_nm,sku_id)%>%mutate(avg_WoW=mean(abs(change)))
this_week=max(test$fisc_wk_of_mth_id)

lrg_chng=avg_change%>%filter(fisc_wk_of_mth_id==this_week)%>%
  filter(abs(change)>avg_WoW & abs(change)>2)

#######For Loop for variance
sku=matrix()
for (i in unique(top_sku$sku_id)){
  select_sku=top_sku%>%filter(sku_id==i)
  pct_rev=round(unique(select_sku$pct_rev),4)
  cpi=round(unique(select_sku$tw_sku_cpi),2)
  sku_id=unique(select_sku$sku_id)
  cpi_diff=round(unique(select_sku$diff_sku_cpi), 2)
  class_id=unique(select_sku$class_id)
  
  sentence=print(paste0("Sku ",sku_id ," acounts for " , pct_rev*100 , "% of revenue in class ",class_id, " with a current CPI of  " , cpi , " and a WoW difference of ",  cpi_diff, "."))
  sku=rbind(sku,sentence)
  
}
##Output files for Weekly Report use 
#setwd("\\\\cs01corp/root/Files/Corp/MKT/950240/PUB-DB/Pricing Strategy/Artem/CPI insights")
write.table(wow, 'classes.csv')
write.table(results, 'vendors.csv')
write.table(sku, 'SKU.txt')
