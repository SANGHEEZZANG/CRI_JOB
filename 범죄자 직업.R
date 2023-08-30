library(dplyr)
library(ggplot2)

df1 <- read.csv('criminal_job.csv',fileEncoding = "euc-kr")

df1
head(df1)

dim(df1)

str(df1)

names(df1)

# 컬럼 두개 삭제!!!
df2 <- df1 %>%
  select(-범죄별.1.,-범죄별.2.)

#
head(df2)
str(df2)

df3 <- df2 %>%
  slice(-1,-2)

# 컬럼명 변경
head(df4)

df_names <- df2 %>%
  slice(2)

df_names = data.frame(df_names)

df_names[2]
length(df_names)

i <- 2
for(i in 2:length(df_names)) {
  colnames(df4)[i] <- df_names[i]
}

df4 <- df4 %>%
  rename(범죄유형 = '범죄별(3)')

colnames(df4)
head(df4)
str(df4)


# 결측값 처리
i <- 2
for(i in 2:length(df_names)) {
  df4[i] <- replace(df4[i],df4[i] == '-',"0")
}

str(df4)
names(df4[-1])

df5 <- df4 %>%
  mutate(across(names(df4[-1]),as.numeric))

str(df5)
# 범죄유형별 합계, 평균
df6 <- df5 %>%
  rowwise() %>%
  mutate(sum_crim = sum(c_across(농임수산업:미상))) %>%
  mutate(mean_crim = mean(c_across(농임수산업:미상)))

df7 <- df6 %>%
  select(범죄유형,sum_crim,mean_crim)

df7

print(df7,n=38)
ggplot(data=df7, aes(x=reorder(범죄유형,sum_crim), y=sum_crim)) +
  geom_col(bg="skyblue", size=2 ) +
  geom_smooth(method="lm",color="red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(label=rownames(df7),hjust=0,vjust=0,nudge_y=0.7,size=2) +
  labs(title="범죄현황황",x="범죄유형형",y="합계")

ggplot(data=df7, aes(x=reorder(범죄유형,sum_crim), y=sum_crim)) +
  geom_col(bg="skyblue", size=2 ) +
  coord_flip() +
  geom_text(aes(label = sum_crim), hjust = 0, nudge_y=1)
  labs(title="범죄현황황",x="범죄유형형",y="합계")


summary(df7$sum_crim)

df8 <- df7 %>%
  filter(범죄유형 != "사기" & 범죄유형 != "절도" & 범죄유형 != "폭행")

ggplot(data=df8, aes(x=범죄유형, y=sum_crim)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="범죄현황황",x="범죄유형형",y="합계")

summary(df8$sum_crim)
# 정렬하고 자르면 될거 같은데, 피시방 갔다가 운동할거임임
df7
