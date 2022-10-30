################################################################################
################################################################################
########   BC2406 Analytics I: Visual and Predictive Analytics   ###############
########   PROJECT: Say NO to Myocardial Infarction Complications  #############
################################################################################
################################################################################

########################
#### Data Cleaning  ####
########################

#  1700 rows, 124 variables 
#  113 x variables, 11 y Variables (Target)

## Cleaning Approach 
#  1. Extract key x variables which supports our business solution
#  2. Check for NA/ Missing Values
#  3. Handle NA/ missing values and provide Justification
#     - Replace with 0 
#     - Remove rows completely (Missing data not insightful)

# Importing libraries
library("data.table")
library("dplyr")
library("caTools")
library("ggplot2")
library("plyr")

df = fread("data/raw/Myocardial Infarction Complications Database.csv", stringsAsFactors = TRUE)
summary(df)


########################
###  Rename Columns  ###
########################

## Columns 1 - 20 

setnames(df, "ID", "ID")
setnames(df, "AGE", "AGE")
setnames(df, "SEX", "SEX")
setnames(df, "INF_ANAM", "H_QTY_MI")
setnames(df, "STENOK_AN", "H_EXT_HeartPain")

setnames(df, "FK_STENOK", "H_FC_HeartPain")
setnames(df, "IBS_POST", "CHD_BA")
setnames(df, "IBS_NASL", "Heredity_CHD")
setnames(df, "GB", "Essential_HT")
setnames(df, "SIM_GIPERT", "Symptomatic_HT")

setnames(df, "DLIT_AG", "Duration_ART_HT")
setnames(df, "ZSN_A", "H_CN_HF")
setnames(df, "nr_11", "H_Irregular_HB")
setnames(df, "nr_01", "H_Premature_AC")
setnames(df, "nr_02", "H_Premature_VC")

setnames(df, "nr_03", "H_Sudden_AF")
setnames(df, "nr_04", "H_Persistent_AF")
setnames(df, "nr_07", "H_VF")
setnames(df, "nr_08", "H_Sudden_VT")
setnames(df, "np_01", "H_1DF_AVB")

## Columns 21 - 40

setnames(df, "np_04", "H_3DG_AVB")
setnames(df, "np_05", "H_LBBB")
setnames(df, "np_07", "H_Incomplete_LBBB")
setnames(df, "np_08", "H_Complete_LBBB")
setnames(df, "np_09", "H_Incomplete_RBBB")

setnames(df, "np_10", "H_Complete_RBBB")
setnames(df, "endocr_01", "H_DM")
setnames(df, "endocr_02", "H_OB")
setnames(df, "endocr_03", "H_TX")
setnames(df, "zab_leg_01", "H_CN_BC")

setnames(df, "zab_leg_02", "H_OBS_CN_BC")
setnames(df, "zab_leg_03", "H_BCA")
setnames(df, "zab_leg_04", "H_CN_PN")
setnames(df, "zab_leg_06", "H_PUM_TB")
setnames(df, "S_AD_KBRIG", "SYS_BP_ECT")

setnames(df, "D_AD_KBRIG", "DIA_BP_ECT")
setnames(df, "S_AD_ORIT", "SYS_BP_ICU")
setnames(df, "D_AD_ORIT", "DIA_BP_ICU")
setnames(df, "O_L_POST", "PUM_ED_TOA_ICU")
setnames(df, "K_SH_POST", "CDA_TOA_ICU")

## Columns 41 - 60
setnames(df,"MP_TP_POST","Sudden_AF_TOA_ICU")
setnames(df,"SVT_POST","Sudden_ST_TOA_ICU")
setnames(df,"GT_POST","Sudden_VT_TOA_ICU")
setnames(df,"FIB_G_POST","VF_TOA_ICU")

setnames(df,"ant_im","Anterior_MI_LV_ECG")
setnames(df,"lat_im","Lateral_MI_LV_ECG")
setnames(df,"inf_im","Inferior_MI_LV_ECG")
setnames(df,"post_im","Posterior_MI_LV_ECG")
setnames(df,"IM_PG_P","MI_VR")

setnames(df,"ritm_ecg_p_01","ECG_TOA_SNS")
setnames(df,"ritm_ecg_p_02","ECG_TOA_AF")
setnames(df,"ritm_ecg_p_04","ECG_TOA_AT")
setnames(df,"ritm_ecg_p_06","ECG_TOA_ID")
setnames(df,"ritm_ecg_p_07","ECG_TOA_SNS_TC")
setnames(df,"ritm_ecg_p_08","ECG_TOA_SNS_BR")

setnames(df,"n_r_ecg_p_01","Premature_AC_ECG_TOA")
setnames(df,"n_r_ecg_p_02","Frequent_Premature_AC_ECG_TOA")
setnames(df,"n_r_ecg_p_03","Premature_VC_ECG_TOA")
setnames(df,"n_r_ecg_p_04","Frequent_Premature_VC_ECG_TOA")
setnames(df,"n_r_ecg_p_05","Sudden_AF_ECG_TOA")

## Columns 61 - 80
setnames(df, "n_r_ecg_p_06", "Persistent_AF_ECG_TOA") 
setnames(df, "n_r_ecg_p_08", "Sudden_ST_ECG_TOA")
setnames(df, "n_r_ecg_p_09", "Sudden_VT_ECG_TOA")
setnames(df, "n_r_ecg_p_10", "VF_ECG_TOA")
setnames(df, "n_p_ecg_p_01", "SB_ECG_TOA")

setnames(df, "n_p_ecg_p_03", "T_1DG_AVB_ECG_TOA")
setnames(df, "n_p_ecg_p_04", "Type1_2DG_AVB_ECG_TOA")
setnames(df, "n_p_ecg_p_05", "Type2_2DF_AVB_ECG_TOA")
setnames(df, "n_p_ecg_p_06", "T_3DF_AVB_ECG_TOA")
setnames(df, "n_p_ecg_p_07", "LBBB_AB_ECG_TOA")

setnames(df, "n_p_ecg_p_08", "LBBB_PB_ECG_TOA")
setnames(df, "n_p_ecg_p_09", "Incomplete_LBBB_ECG_TOA")
setnames(df, "n_p_ecg_p_10", "Complete_LBBB_ECG_TOA")
setnames(df, "n_p_ecg_p_11", "Incomplete_RBBB_ECG_TOA")
setnames(df, "n_p_ecg_p_12", "Complete_RBBB_ECG_TOA")

setnames(df, "fibr_ter_01", "FT_C750k")
setnames(df, "fibr_ter_02", "FT_C1M")
setnames(df, "fibr_ter_03", "FT_C3M")
setnames(df, "fibr_ter_05", "FT_S")
setnames(df, "fibr_ter_06", "FT_C500k")

## Columns 81 - 100
setnames(df, "fibr_ter_07", "FT_C250k")
setnames(df, "fibr_ter_08", "FT_S1.5m")
setnames(df, "GIPO_K", "Hypokalemia")
setnames(df, "GIPER_NA", "Increase_NA")
setnames(df, "ROE", "ESR")

setnames(df, "TIME_B_S", "HOSPITAL_TIME")
setnames(df, "R_AB_1_n", "RELAPSE_1H")
setnames(df, "R_AB_2_n", "RELAPSE_2D")
setnames(df, "R_AB_3_n", "RELAPSE_3D")
setnames(df, "NA_KB", "OPIOID_ECT")

setnames(df, "NOT_NA_KB", "NSAID_ECT")
setnames(df, "LID_KB", "LIDOCAINE_ECT")
setnames(df, "NITR_S", "NITRATE_ECT")
setnames(df, "NA_R_1_n", "OPIOID_1H")


## Columns 101 - 112
setnames(df, "NA_R_2_n", "OPIOID_2D")
setnames(df, "NA_R_3_n", "OPIOID_3D")
setnames(df, "NOT_NA_1_n", "NSAID_1H")
setnames(df, "NOT_NA_2_n", "NSAID_2D")
setnames(df, "NOT_NA_3_n", "NSAID_3D")

setnames(df, "LID_S_n", "LIDOCAINE_ICU")
setnames(df, "B_BLOK_S_n", "BB_ICU")
setnames(df, "ANT_CA_S_n", "CAB_ICU")
setnames(df, "GEPAR_S_n", "HERAPIN_ICU")
setnames(df, "ASP_S_n", "A_ACID_ICU")
setnames(df, "TIKL_S_n", "TICLID_ICU")
setnames(df, "TRENT_S_n", "TRENTAL_ICU")

## Columns 113 - 124
setnames(df, "FIBR_PREDS", "Target_AF")
setnames(df, "PREDS_TAH", "Target_ST")
setnames(df, "JELUD_TAH", "Target_VT")
setnames(df, "FIBR_JELUD", "Target_VF")
setnames(df, "A_V_BLOK", "Target_3DG_AVB")
setnames(df, "OTEK_LANC", "Target_PUM_ED")
setnames(df, "RAZRIV", "Target_Myocardial_Rupture")
setnames(df, "DRESSLER", "Target_Dressler")
setnames(df, "ZSN", "Target_CN_HF")
setnames(df, "REC_IM", "Target_Relapse_MI")
setnames(df, "P_IM_STEN", "Target_Post_HeartPain")
setnames(df, "LET_IS", "Target_Lethal")

## Checking the new column names
colnames(df)



#####################################
#### Factor Categorical Columns  ####
#####################################

cols = c("SEX","H_QTY_MI","H_EXT_HeartPain","H_FC_HeartPain","CHD_BA","Heredity_CHD" 
         ,"Essential_HT","Symptomatic_HT","Duration_ART_HT","H_CN_HF","H_Irregular_HB" 
         ,"H_Premature_AC","H_Premature_VC","H_Sudden_AF","H_Persistent_AF","H_VF" 
         ,"H_Sudden_VT","H_1DF_AVB","H_3DG_AVB","H_LBBB","H_Incomplete_LBBB","H_Complete_LBBB","H_Incomplete_RBBB" 
         ,"H_Complete_RBBB","H_DM","H_OB","H_TX","H_CN_BC","H_OBS_CN_BC","H_BCA" 
         ,"H_CN_PN","H_PUM_TB","PUM_ED_TOA_ICU","CDA_TOA_ICU","Sudden_AF_TOA_ICU","Sudden_ST_TOA_ICU","Sudden_VT_TOA_ICU","VF_TOA_ICU","Anterior_MI_LV_ECG" 
         ,"Lateral_MI_LV_ECG","Inferior_MI_LV_ECG","Posterior_MI_LV_ECG","MI_VR" 
         ,"ECG_TOA_SNS","ECG_TOA_AF","ECG_TOA_AT","ECG_TOA_ID","ECG_TOA_SNS_TC","ECG_TOA_SNS_BR" 
         ,"Premature_AC_ECG_TOA","Frequent_Premature_AC_ECG_TOA","Premature_VC_ECG_TOA","Frequent_Premature_VC_ECG_TOA","Sudden_AF_ECG_TOA" 
         ,"Persistent_AF_ECG_TOA","Sudden_ST_ECG_TOA","Sudden_VT_ECG_TOA","VF_ECG_TOA" 
         ,"SB_ECG_TOA","T_1DG_AVB_ECG_TOA","Type1_2DG_AVB_ECG_TOA","Type2_2DF_AVB_ECG_TOA" 
         ,"T_3DF_AVB_ECG_TOA","LBBB_AB_ECG_TOA","LBBB_PB_ECG_TOA","Incomplete_LBBB_ECG_TOA" 
         ,"Complete_LBBB_ECG_TOA","Incomplete_RBBB_ECG_TOA","Complete_RBBB_ECG_TOA","FT_C750k" 
         ,"FT_C1M","FT_C3M","FT_S","FT_C500k" 
         ,"FT_C250k","FT_S1.5m","Hypokalemia","Increase_NA","HOSPITAL_TIME","RELAPSE_1H" 
         ,"RELAPSE_2D","RELAPSE_3D","OPIOID_ECT","NSAID_ECT","LIDOCAINE_ECT","NITRATE_ECT" 
         ,"OPIOID_1H" 
         ,"OPIOID_2D","OPIOID_3D","NSAID_1H","NSAID_2D","NSAID_3D","LIDOCAINE_ICU","BB_ICU" 
         ,"CAB_ICU","HERAPIN_ICU","A_ACID_ICU","TICLID_ICU","TRENTAL_ICU" 
         ,"Target_AF","Target_ST","Target_VT","Target_VF","Target_3DG_AVB","Target_PUM_ED" 
         ,"Target_Myocardial_Rupture","Target_Dressler","Target_CN_HF","Target_Relapse_MI" 
         ,"Target_Post_HeartPain","Target_Lethal") 


df[,(cols):=lapply(.SD, as.factor),.SDcols = cols] 
sapply(df,class)


##########################
#### Data Extraction  ####
##########################

# Key Categories: Patient's Info, Medical History, ICU/ ECG (Time of Admission)

colnames(df)

## ALL 124 COLUMNS
# [1] "ID"                            "AGE"                          
# [3] "SEX"                           "H_QTY_MI"                     
# [5] "H_EXT_HeartPain"               "H_FC_HeartPain"               
# [7] "CHD_BA"                        "Heredity_CHD"                 
# [9] "Essential_HT"                  "Symptomatic_HT"               
# [11] "Duration_ART_HT"               "H_CN_HF"                      
# [13] "H_Irregular_HB"                "H_Premature_AC"               
# [15] "H_Premature_VC"                "H_Sudden_AF"                  
# [17] "H_Persistent_AF"               "H_VF"                         
# [19] "H_Sudden_VT"                   "H_1DF_AVB"                    
# [21] "H_3DG_AVB"                     "H_LBBB"                       
# [23] "H_Incomplete_LBBB"             "H_Complete_LBBB"              
# [25] "H_Incomplete_RBBB"             "H_Complete_RBBB"              
# [27] "H_DM"                          "H_OB"                         
# [29] "H_TX"                          "H_CN_BC"                      
# [31] "H_OBS_CN_BC"                   "H_BCA"                        
# [33] "H_CN_PN"                       "H_PUM_TB"                     
# [35] "SYS_BP_ECT"                    "DIA_BP_ECT"                   
# [37] "SYS_BP_ICU"                    "DIA_BP_ICU"                   
# [39] "PUM_ED_TOA_ICU"                "CDA_TOA_ICU"                  
# [41] "Sudden_AF_TOA_ICU"             "Sudden_ST_TOA_ICU"            
# [43] "Sudden_VT_TOA_ICU"             "VF_TOA_ICU"                   
# [45] "Anterior_MI_LV_ECG"            "Lateral_MI_LV_ECG"            
# [47] "Inferior_MI_LV_ECG"            "Posterior_MI_LV_ECG"          
# [49] "MI_VR"                         "ECG_TOA_SNS"                  
# [51] "ECG_TOA_AF"                    "ECG_TOA_AT"                   
# [53] "ECG_TOA_ID"                    "ECG_TOA_SNS_TC"               
# [55] "ECG_TOA_SNS_BR"                "Premature_AC_ECG_TOA"         
# [57] "Frequent_Premature_AC_ECG_TOA" "Premature_VC_ECG_TOA"         
# [59] "Frequent_Premature_VC_ECG_TOA" "Sudden_AF_ECG_TOA"            
# [61] "Persistent_AF_ECG_TOA"         "Sudden_ST_ECG_TOA"            
# [63] "Sudden_VT_ECG_TOA"             "VF_ECG_TOA"                   
# [65] "SB_ECG_TOA"                    "T_1DG_AVB_ECG_TOA"            
# [67] "Type1_2DG_AVB_ECG_TOA"         "Type2_2DF_AVB_ECG_TOA"        
# [69] "T_3DF_AVB_ECG_TOA"             "LBBB_AB_ECG_TOA"              
# [71] "LBBB_PB_ECG_TOA"               "Incomplete_LBBB_ECG_TOA"      
# [73] "Complete_LBBB_ECG_TOA"         "Incomplete_RBBB_ECG_TOA"      
# [75] "Complete_RBBB_ECG_TOA"         "FT_C750k"                     
# [77] "FT_C1M"                        "FT_C3M"                       
# [79] "FT_S"                          "FT_C500k"                     
# [81] "FT_C250k"                      "FT_S1.5m"                     
# [83] "Hypokalemia"                   "K_BLOOD"                      
# [85] "Increase_NA"                   "NA_BLOOD"                     
# [87] "ALT_BLOOD"                     "AST_BLOOD"                    
# [89] "KFK_BLOOD"                     "L_BLOOD"                      
# [91] "ESR"                           "HOSPITAL_TIME"                
# [93] "RELAPSE_1H"                    "RELAPSE_2D"                   
# [95] "RELAPSE_3D"                    "OPIOID_ECT"                   
# [97] "NSAID_ECT"                     "LIDOCAINE_ECT"                
# [99] "NITRATE_ECT"                   "OPIOID_1H"                    
# [101] "OPIOID_2D"                     "OPIOID_3D"                    
# [103] "NSAID_1H"                      "NSAID_2D"                     
# [105] "NSAID_3D"                      "LIDOCAINE_ICU"                
# [107] "BB_ICU"                        "CAB_ICU"                      
# [109] "HERAPIN_ICU"                   "A_ACID_ICU"                   
# [111] "TICLID_ICU"                    "TRENTAL_ICU"                  
# [113] "Target_AF"                     "Target_ST"                    
# [115] "Target_VT"                     "Target_VF"                    
# [117] "Target_3DG_AVB"                "Target_PUM_ED"                
# [119] "Target_Myocardial_Rupture"     "Target_Dressler"              
# [121] "Target_CN_HF"                  "Target_Relapse_MI"            
# [123] "Target_Post_HeartPain"         "Target_Lethal"                


# Filtering total data 
colnames(df)
filtered_df_nb = df[, c(2:75, 113:124)] # Without Blood Content 

# Visualise total data
colnames(filtered_df_nb)
summary(filtered_df_nb)

## FILTERED DATAFRAME WITHOUT BLOOD CONTENT
# 
# [1] "AGE"                           "SEX"                          
# [3] "H_QTY_MI"                      "H_EXT_HeartPain"              
# [5] "H_FC_HeartPain"                "CHD_BA"                       
# [7] "Heredity_CHD"                  "Essential_HT"                 
# [9] "Symptomatic_HT"                "Duration_ART_HT"              
# [11] "H_CN_HF"                       "H_Irregular_HB"               
# [13] "H_Premature_AC"                "H_Premature_VC"               
# [15] "H_Sudden_AF"                   "H_Persistent_AF"              
# [17] "H_VF"                          "H_Sudden_VT"                  
# [19] "H_1DF_AVB"                     "H_3DG_AVB"                    
# [21] "H_LBBB"                        "H_Incomplete_LBBB"            
# [23] "H_Complete_LBBB"               "H_Incomplete_RBBB"            
# [25] "H_Complete_RBBB"               "H_DM"                         
# [27] "H_OB"                          "H_TX"                         
# [29] "H_CN_BC"                       "H_OBS_CN_BC"                  
# [31] "H_BCA"                         "H_CN_PN"                      
# [33] "H_PUM_TB"                      "SYS_BP_ECT"                   
# [35] "DIA_BP_ECT"                    "SYS_BP_ICU"                   
# [37] "DIA_BP_ICU"                    "PUM_ED_TOA_ICU"               
# [39] "CDA_TOA_ICU"                   "Sudden_AF_TOA_ICU"            
# [41] "Sudden_ST_TOA_ICU"             "Sudden_VT_TOA_ICU"            
# [43] "VF_TOA_ICU"                    "Anterior_MI_LV_ECG"           
# [45] "Lateral_MI_LV_ECG"             "Inferior_MI_LV_ECG"           
# [47] "Posterior_MI_LV_ECG"           "MI_VR"                        
# [49] "ECG_TOA_SNS"                   "ECG_TOA_AF"                   
# [51] "ECG_TOA_AT"                    "ECG_TOA_ID"                   
# [53] "ECG_TOA_SNS_TC"                "ECG_TOA_SNS_BR"               
# [55] "Premature_AC_ECG_TOA"          "Frequent_Premature_AC_ECG_TOA"
# [57] "Premature_VC_ECG_TOA"          "Frequent_Premature_VC_ECG_TOA"
# [59] "Sudden_AF_ECG_TOA"             "Persistent_AF_ECG_TOA"        
# [61] "Sudden_ST_ECG_TOA"             "Sudden_VT_ECG_TOA"            
# [63] "VF_ECG_TOA"                    "SB_ECG_TOA"                   
# [65] "T_1DG_AVB_ECG_TOA"             "Type1_2DG_AVB_ECG_TOA"        
# [67] "Type2_2DF_AVB_ECG_TOA"         "T_3DF_AVB_ECG_TOA"            
# [69] "LBBB_AB_ECG_TOA"               "LBBB_PB_ECG_TOA"              
# [71] "Incomplete_LBBB_ECG_TOA"       "Complete_LBBB_ECG_TOA"        
# [73] "Incomplete_RBBB_ECG_TOA"       "Complete_RBBB_ECG_TOA"        
# [75] "Target_AF"                     "Target_ST"                    
# [77] "Target_VT"                     "Target_VF"                    
# [79] "Target_3DG_AVB"                "Target_PUM_ED"                
# [81] "Target_Myocardial_Rupture"     "Target_Dressler"              
# [83] "Target_CN_HF"                  "Target_Relapse_MI"            
# [85] "Target_Post_HeartPain"         "Target_Lethal"


##################################################
##### Handling Missing Data (Justification)  #####
##################################################

#### Refer to Project Appendix 2


#### DATA FRAME WITHOUT BLOOD CONTENT

colnames(filtered_df_nb)

## Dropping MISSING rows 
filtered_df_nb = filtered_df_nb[complete.cases(filtered_df_nb[, 44:74])] # Dropped 244 NA cases (No ECG Readings)
filtered_df_nb = filtered_df_nb[complete.cases(filtered_df_nb[, c(1, 3:6, 11)])] # Dropped 126 NA cases (Key Historical readings)
summary(filtered_df_nb)

## Replacing with 0 
filtered_df_nb[, c(7:10, 12:43)][is.na(filtered_df_nb[, c(7:10, 12:43)])] = 0
summary(filtered_df_nb)

# ## Count of NA FUNCTION
# m = sapply(filtered_df_nb, function(x) sum(is.na(x)))
# m = sort(m[m!=0], decreasing = T) # Extract columns with missing values
# naCount = transpose(data.frame(as.list(m)), keep.names = "Columns")
# colnames(naCount) = c("Columns", "Count")

## Verify that NA values are removed
# naCount

## Error indicates that NA values are missing
## Error in seq_len(length(ans) - length(keep.names)) : 
## argument must be coercible to non-negative integer


#############################################
##### Visualizing Distribution of Data  #####
#############################################

# Done by Eugene Wee 

# Check Columns 
summary(filtered_df_nb)
colnames(filtered_df_nb)
str(filtered_df_nb[, 1:86])

# Count of each column
colCount = sapply(X = filtered_df_nb,
                  FUN = table)
summary(filtered_df_nb)
length(colCount)
colCount[c(1, 34:37)] = NULL # Remove Continuous variable
colCount = ldply(colCount, data.frame)
colnames(colCount) = c("Variable", "Category", "Count")
colCount = colCount[with(colCount, order(Variable, Count)), ]
colCount

# Only categorical columns
nrow(colCount)
c1 = colCount[0:50, ]
c2 = colCount[51:99, ]
c3 = colCount[100:150, ]
c4  = colCount[151:202, ]

# Visualizing Count (Col 1 - 50)
plot1 = ggplot(c1, aes(x=Variable, y=Count,  fill = Category)) + 
                geom_bar(position='stack', stat='identity') +
                geom_text(aes(x=Variable, label=Count), position = 'stack', 
                          vjust=0.4, angle=0,hjust=0.5, size=2.3) + 
                coord_flip() +
                scale_fill_brewer(palette="Pastel1", direction = 1) + 
                labs(title="Distribution of Feature Variables (Col 1 - 50)")

plot1

# Visualizing Count (Col 51 - 99)
plot2 =ggplot(c2, aes(x=Variable, y=Count,  fill = Category)) + 
          geom_bar(position='stack', stat='identity') +
          geom_text(aes(x=Variable, label=Count), position = 'stack', 
                    vjust=0.4, angle=0,hjust=0.5, size=2.3) + 
          coord_flip() +
          scale_fill_brewer(palette="Pastel1", direction = 1)+ 
          labs(title="Distribution of Feature Variables (Col 51 - 99)")

plot2   

# Visualizing Count (Col 100 - 150)
plot3 =ggplot(c3, aes(x=Variable, y=Count,  fill = Category)) + 
          geom_bar(position='stack', stat='identity') +
          geom_text(aes(x=Variable, label=Count), position = 'stack', 
                    vjust=0.4, angle=0,hjust=0.5, size=2.3) + 
          coord_flip() +
          scale_fill_brewer(palette="Pastel1", direction = 1) +
          labs(title="Distribution of Feature Variables (Col 100 - 150)")

plot3

# Visualizing Count (Col 151 - 202)
plot4 = ggplot(c4, aes(x=Variable, y=Count,  fill = Category)) + 
          geom_bar(position='stack', stat='identity') +
          geom_text(aes(x=Variable, label=Count), position = 'stack', 
                    vjust=0.4, angle=0,hjust=0.5, size=2.3) + 
          coord_flip() +
          scale_fill_brewer(palette="Pastel1", direction = 1)+
          labs(title="Distribution of Feature Variables (Col 151 - 202)")
  
plot4


## Conclusion: We can tell from the barplots that the distributions of categorical
#  classes are extremely imbalanced, with only a few columns having a balanced data.
#  Therefore, we sought to apply oversampling of minority classes together with undersampling
#  of majority classes to balance out the data.s


###########################################################
##### Applying Sampling to deal with unbalanced data  #####
###########################################################

# Done by Eugene 

# install.packages("ROSE")
library("ROSE")


### RELAPSE OF MI (Complication #1)
colnames(filtered_df_nb)
AF_df = filtered_df_nb[, c(1:74, 75)]
colnames(AF_df)

set.seed(2004)
sampled_AF_df=ovun.sample(Target_AF~., data=AF_df, method="both")$data
summary(sampled_AF_df) # Note that target variables has been balanced 

# Count of frequency (Drop insignificant columns)
AF_count = sapply(X = sampled_AF_df,
                   FUN = table)
AF_count = ldply(AF_count, data.frame) 

# Set benchmark as 5% for minority class, remove those that does not qualify
benchmark = round(nrow(sampled_AF_df) - nrow(sampled_AF_df)*0.05, 0)
drop = unique(AF_count[AF_count$Freq>benchmark, ]$.id) 
final_AF = sampled_AF_df[, !names(sampled_AF_df) %in% drop] # Drop columns
summary(final_AF)



### Chronic Heart Failure (Complication #2)
colnames(filtered_df_nb)
CN_HF_df = filtered_df_nb[, c(1:74, 83)]
colnames(CN_HF_df)

set.seed(2004)
sampled_CN_HF_df=ovun.sample(Target_CN_HF~., data=CN_HF_df, method="both")$data
summary(sampled_CN_HF_df) # Note that target variables has been balanced 

# Count of frequency (Drop insignificant columns)
CN_HF_count = sapply(X = sampled_CN_HF_df,
                   FUN = table)
CN_HF_count = ldply(CN_HF_count, data.frame) 


# Set benchmark as 5% for minority class, remove those that does not qualify
benchmark = round(nrow(sampled_CN_HF_df) - nrow(sampled_CN_HF_df)*0.05, 0)
drop = unique(CN_HF_count[CN_HF_count$Freq>benchmark, ]$.id) 
final_CN_HF = sampled_CN_HF_df[, !names(sampled_CN_HF_df) %in% drop] # Drop columns
summary(final_CN_HF)



### RELAPSE OF MI (Complication #3)
colnames(filtered_df_nb)
RMI_df = filtered_df_nb[, c(1:74, 84)]
colnames(RMI_df)

set.seed(2004)
sampled_RMI_df=ovun.sample(Target_Relapse_MI~., data=RMI_df, method="both")$data
summary(sampled_RMI_df) # Note that target variables has been balanced 

# Count of frequency (Drop insignificant columns)
RMI_count = sapply(X = sampled_RMI_df,
              FUN = table)
RMI_count = ldply(RMI_count, data.frame) 


# Set benchmark as 5% for minority class, remove those that does not qualify
benchmark = round(nrow(sampled_RMI_df) - nrow(sampled_RMI_df)*0.05, 0)
drop = unique(RMI_count[RMI_count$Freq>benchmark, ]$.id) 
final_RMI = sampled_RMI_df[, !names(sampled_RMI_df) %in% drop] # Drop columns
summary(final_RMI)

#############################
##### Exporting Charts  #####
#############################

ggsave("charts/Distribution_Col_1_to_50.png", plot = plot1, width=10, height=5)
ggsave("charts/Distribution_Col_51_to_99.png", plot = plot2, width=10, height=5)
ggsave("charts/Distribution_Col_100_to_150.png", plot = plot3, width=10, height=5)
ggsave("charts/Distribution_Col_151_to_202.png", plot = plot4, width=10, height=5)


##########################################
##### Save Cleaned DataFrame as CSV  #####
##########################################

# #### DATA FRAME WITH BLOOD CONTENT
# write.csv(filtered_df_nb, "Final_Data.csv", row.names = FALSE)


#### FINAL DATA FRAME (ATRIAL FIBRILLATION) 
write.csv(final_AF, "data/clean/Final_AF.csv", row.names = FALSE)


#### FINAL DATA FRAME (CHRONIC HEART FAILURE) 
write.csv(final_CN_HF, "data/clean/Final_CN_HF.csv", row.names = FALSE)


#### FINAL DATA FRAME (RELAPSE OF MI) 
write.csv(final_RMI, "data/clean/Final_RMI.csv", row.names = FALSE)