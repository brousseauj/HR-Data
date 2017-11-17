library(needs)
needs(
  tidyverse,
  plotly,
  rpart,
  rpart.plot,
  viridis,
  Metrics,
  glmnet,
  formattable,
  data.table,
  ggplot2,htmlTable,randomForest,
  sjPlot,
  sjmisc
)

df = fread('~/Desktop/HR_comma_sep.csv')
df$sales = as.factor(df$sales)
df$salary = factor(df$salary,levels = c('low','medium','high'),ordered=T)

htmlTable(summary(df))
htmlTable(as.data.frame(prop.table(table(df$salary))))

salaryPlot = ggplot(df,aes(x=salary))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme_fivethirtyeight()+ggtitle('Percentage of Salaries')
ggsave(filename = 'salaryPlot.png',plot = salaryPlot,dpi=400)

salesPlot = ggplot(df,aes(x=sales))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme_fivethirtyeight()+ggtitle('Percentage of Departments')+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave(filename = 'salesPlot.png',salesPlot,dpi=400)

salesSalaryPlot = ggplot(df,aes(x=sales))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme_fivethirtyeight()+ggtitle('Percentage of Salaries by Department')+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  facet_grid(.~salary)
ggsave(filename = 'salesSalaryPlot.png',salesSalaryPlot,dpi=400)


df$Work_accident=factor(df$Work_accident)
df$left=factor(df$left,labels = c("Employed",'Left'))
df$promotion_last_5years=factor(df$promotion_last_5years,labels = c('No','Yes'))



satPlot=ggplot(df,aes(x=satisfaction_level,fill=left))+geom_histogram(alpha=.7)+
      theme_fivethirtyeight()+
     # facet_grid(.~left)+
      ggtitle('Satisfaction Level')+
      theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave(filename = 'satPlot_left.png',satPlot,dpi=400)

lastEval =ggplot(df,aes(x=last_evaluation,fill=left))+geom_histogram(alpha=.7)+
  theme_fivethirtyeight()+
  #facet_grid(.~left)+
  ggtitle('Last Evaluation')+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave(filename = 'evalPlot_left.png',lastEval,dpi=400)

numProj = ggplot(df,aes(x=number_project,fill=left))+geom_bar(alpha=.7)+
  theme_fivethirtyeight()+
  ggtitle('Number of Projects')+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave(filename = 'numProj_left.png',numProj,dpi=400)

monthyHours = ggplot(df,aes(x=average_montly_hours,fill=left))+geom_histogram(alpha=.7)+
  theme_fivethirtyeight()+
  ggtitle('Monthly Hours')+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave(filename = 'monthlyHours_left.png',monthlyHours,dpi=400)

timeCompany=ggplot(df,aes(x=time_spend_company,fill=left))+geom_bar(alpha=.7)+
  theme_fivethirtyeight()+
  ggtitle('Years at Company')+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave(filename = 'YearsCompany_let=ft.png',timeCompany,dpi=400)

promo=ggplot(df,aes(x=promotion_last_5years,fill=left))+geom_bar(alpha=.7)+
  theme_fivethirtyeight()+
  ggtitle('Promotion in the last 5 years')+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave(filename = 'promo_left.png',promo,dpi=400)

sales_left = ggplot(df,aes(x=sales,fill=left))+geom_bar(alpha=.7)+
  theme_fivethirtyeight()+
  ggtitle('Department')+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave(filename = 'department_left.png',sales_left,dpi=400)

salary_left = ggplot(df,aes(x=salary,fill=left))+geom_bar(alpha=.7)+
  theme_fivethirtyeight()+
  ggtitle('Salary')+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave(filename = 'salary_left.png',salary_left,dpi=400)


##


satvslast=ggplot(df,aes(last_evaluation, satisfaction_level)) +
  geom_point(alpha = 0.1,color='blue') +
  facet_wrap( ~ left, ncol = 2)+ggtitle('Satisfaction Level VS Last Evaluation')
ggsave(filename = 'satvslast.png',satvslast,dpi=400)

satSalaryvslast=ggplot(df,aes(last_evaluation, satisfaction_level)) +
  geom_point(alpha = 0.1,color='blue') +
  facet_wrap(salary ~ left, ncol = 2)+ggtitle('Satisfaction Level + Salary VS Last Evaluation')
ggsave(filename = 'satSalaryvslast.png',satSalaryvslast,dpi=400)

satSalesvslast=ggplot(df,aes(last_evaluation, satisfaction_level)) +
  geom_point(alpha = 0.1,color='blue') +
  facet_grid(sales ~ left)+ggtitle('Satisfaction Level + Department VS Last Evaluation')
ggsave(filename = 'satSalesvslast.png',satSalesvslast,dpi=400)


## Most predictive of satisfaction level
rf_model = randomForest(satisfaction_level~.,df)
importance=as.data.table(rf_model$importance,keep.rownames = T)
impPlot=ggplot(importance,aes(x=rn,y=(IncNodePurity)))+
  geom_bar(stat='identity')+coord_flip()+
  xlab('Variable')+ylab('Measure of importance')
ggsave(filename = 'impPlot.png',impPlot,dpi=400)

x = lm(satisfaction_level~.,data=df)
summary(x)
sjt.lm(x)
step(x)


