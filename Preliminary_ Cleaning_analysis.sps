* Encoding: UTF-8.

#cleaning, recodes and preliminary analysis part 2.

 # Import DHS dataset.
GET
  FILE='C:\Users\testi\Desktop\ADA FINAL\SPSS files\SPSS files\HTBR70SV\HTBR70FL.sav'.
DATASET NAME DataSet9 WINDOW=FRONT.

# Using codebook identifid relevant variable and save as new data set.

SAVE OUTFILE= 'C:\Users\testi\Desktop\ADA FINAL\ada4.19.sav'
  /KEEP  CASEID V012 V501 V531 V613 V218 V743B  V632  V743D V743A  V384A V384B V384C V212 V731 V741 V745A V745B V107 V158 V157 V190 V190A V133  V159 V701 V136 V130 V102 V505.

# open new subset data set.

GET
  FILE='C:\Users\testi\Desktop\ADA FINAL\ada4.19.sav'.
DATASET NAME DataSet9 WINDOW=FRONT.

# Create data subset data set married women, & women aged 35 and over.
DATASET COPY  Married.35.
DATASET ACTIVATE  Married.35.
FILTER OFF.
USE ALL.
SELECT IF (v501 = 1 AND V012 >= 35).
EXECUTE.


/* the married woman’s ability to have just the desired number of children.  computed by subtracting the ideal number of children from the number of living children.
DATASET ACTIVATE Married.35.
COMPUTE Ability_Desired=V218 - V613.
EXECUTE.

/*  Recode  Ability_Desired = If negative recode as less than ideal; If zero recode as achieves ideal;If negative recode as less than ideal; If positive recode as more than ideal.
RECODE Ability_Desired (0=1) (Lowest thru -1=2) (1 thru Highest=3) INTO RecoAbility.
VARIABLE LABELS  RecoAbility 'ability to have just the desired number of children'.
VALUE LABELS RecoAbility 1 'achieves ideal' 2 'less than ideal'  3 'more than idea'.
EXECUTE.

/* Recode missing values as missing.
DATASET ACTIVATE Married.35.
RECODE V613  V012 V501 V531 V613 V218 V743B  V632  V743D V743A  V384A V384B V384C  V731 V741 V745A V745B V133 V158 V157  V190A V159 V701 V136  V130 V102 V505 (99=SYSMIS).
RECODE V531 V613 V505  (98=SYSMIS).
RECODE V531 V133 (97=SYSMIS).
recode V701 (8=SYSMIS).
EXECUTE.

/* Recode number of other wives if greater than zero as polygamous.
RECODE V505  (1 thru HIGHEST=1).
VALUE LABELS v505 0' No other wives'  1 'Polygmaous'.
EXECUTE.

/* Recode nonumeric responde as 4 as per pertocol.
RECODE V613 (ELSE=Copy) (96=4)  INTO recode_ideal.
VARIABLE LABELS  recode_ideal 'ideal with non numeric recoded to 4'.
EXECUTE.


/* the married woman’s ability to have just the desired number of children.  numAbility_Desired computed by subtracting the ideal number of children from the number of living children.

COMPUTE numAbility_Desired = V218 - recode_ideal.
EXECUTE.


/*  Recode  numAbility_Desirde= If negative recode as less than ideal; If zero recode as achieves ideal;If negative recode as less than ideal; If positive recode as more than ideal.

RECODE numAbility_Desired (0=1) (Lowest thru -1=2) (1 thru Highest=3) INTO nonnumAbility.
VARIABLE LABELS   nonnumAbility 'ability to have just the desired number of children'.
VALUE LABELS  nonnumAbility 1 'achieves ideal' 2 'less than ideal'  3 'more than idea'.
EXECUTE.


/* Recode catergorical variable as per operational procotol.

RECODE V743B (1=1) (2 thru 3=0) (4 thru 6=-1) INTO reco_purchase.
RECODE  V743D (1=1) (2 thru 3=0) (4 thru 6=-1) INTO reco_visit.
RECODE  V743A (1=1) (2 thru 3=0) (4 thru 6=-1) INTO reco_care.
RECODE V632 (1=1)  (3=0) (2=-1) INTO reco_contra. 
RECODE V731 (0=0) (1=1) (2 thru 3=2) INTO reco_work. 
RECODE V741 (0=0) (1=3) (2=2) (3=1) INTO reco_pay. 
RECODE  V745A (0=-1) (1=1) (3=1) (2=0)  INTO reco_house.
RECODE  V745B (0=-1) (1=1) (3=1) (2=0)  INTO reco_land.
RECODE V158  (0=0) (1=1) (2 thru 3=2)   INTO reco_freradio.
RECODE V157  (0=0) (1=1) (2 thru 3=2)   INTO reco_frenews.
VARIABLE LABELS  reco_purchase 'reco_purchase' reco_contra 'reco_contra' reco_visit 'reco_visit' reco_care 'reco_care'.
VALUE LABELS reco_purchase reco_visit reco_care reco_contra -1 'Husband/other alone' 0 'joint' 1 'respondent alone'.
VALUE LABELS  reco_freradio reco_frenews 0 'Not at all'  1 'Less than once a week'  2 'At least once a week'.
VALUE LABELS  reco_house reco_land -1 'Does not own' 0 'Jointly only' 1 'Alone only or Both alone and jointly'.
value labels reco_pay 0 'Not paid' 1 'In-kind only' 2 'Cash and in-kind'  3 'Cash only'.
value labels reco_work 0 'No' 1 'In the past year' 2 'Have a job, but on leave or Currently working'.
EXECUTE.

RECODE V136  (1 thru 3=1) (4 thru 6=2) INTO size_reco.
VARIABLE LABELS  size_reco 'size_reco'.
VALUE LABELS size_reco 1 '1-3' 2 '4-6' 3 '7-9' 4 '10 and more'.
EXECUTE.
variable level    reco_purchase reco_visit reco_care reco_contra reco_work reco_pay reco_house reco_land reco_freradio reco_frenews V531 V212 V384A V133 V384B V384C   (scale).
EXECUTE.



# save new subset containing keeping only variable of interests.

SAVE OUTFILE= 'C:\Users\testi\Desktop\ADA FINAL\finalada4.19.sav'
  /KEEP CASEID V012 V212
V531
V613
nonnumAbility
V384A
V384B
V384C
V133
V701
V501
V136
size_reco 
V190
V190A
V130
V102
V505
reco_purchase
reco_visit
reco_care
reco_contra
reco_work
reco_pay
reco_house
reco_land
reco_freradio
reco_frenews.

# Opening subset data.

GET
  FILE='C:\Users\testi\Desktop\ADA FINAL\finalada4.19.sav'.
DATASET NAME DataSet14 WINDOW=FRONT.


*Table 1 Descriptive summaries.

FREQUENCIES VARIABLES=V012 cat_ideal nonnumAbility V102 V190A V701 size_reco V505 V130
  /STATISTICS=STDDEV VARIANCE RANGE MEAN MEDIAN MODE
  /ORDER=ANALYSIS.


 *factor analysis.

DATASET ACTIVATE DataSet14.
FACTOR
  /VARIABLES V212 V531 V384A V384B V384C V133 reco_purchase reco_visit reco_care reco_contra 
    reco_work reco_pay reco_house reco_land reco_freradio reco_frenews
  /MISSING PAIRWISE 
  /ANALYSIS V212 V531 V384A V384B V384C V133 reco_purchase reco_visit reco_care reco_contra 
    reco_work reco_pay reco_house reco_land reco_freradio reco_frenews
  /PRINT UNIVARIATE INITIAL CORRELATION KMO EXTRACTION ROTATION FSCORE
  /FORMAT SORT BLANK(.30)
  /PLOT EIGEN
  /CRITERIA FACTORS(5) ITERATE(25)
  /EXTRACTION PC
  /CRITERIA ITERATE(25) DELTA(0)
  /ROTATION OBLIMIN
  /SAVE REG(ALL)
  /METHOD=CORRELATION.


*/DHS aggregate women's empowerment index

FACTOR
  /VARIABLES reco_purchase reco_visit reco_care reco_contra reco_work reco_pay reco_house reco_land 
    reco_freradio reco_frenews V531 V212 V384A V133 V384B V384C
  /MISSING MEANSUB 
  /ANALYSIS reco_purchase reco_visit reco_care reco_contra reco_work reco_pay reco_house reco_land 
    reco_freradio reco_frenews V531 V212 V384A V133 V384B V384C
 /PRINT UNIVARIATE INITIAL EXTRACTION FSCORE 
 /PLOT EIGEN
 /CRITERIA FACTORS(1) ITERATE(25) 
 /EXTRACTION PC  
/ROTATION NOROTATE  
/SAVE REG(ALL) 
 /METHOD=CORRELATION .


*/DHS social empowerment index.

FACTOR
  /VARIABLES reco_freradio reco_frenews V133 
  /MISSING MEANSUB 
  /ANALYSIS  reco_freradio reco_frenews V133 
 /PRINT UNIVARIATE INITIAL EXTRACTION FSCORE 
 /PLOT EIGEN
 /CRITERIA FACTORS(1) ITERATE(25) 
 /EXTRACTION PC  
/ROTATION NOROTATE  
/SAVE REG(ALL) 
 /METHOD=CORRELATION .


*/DHS economic empowerment index.

FACTOR
  /VARIABLES reco_work reco_pay reco_house reco_land 
      /MISSING MEANSUB 
  /ANALYSIS reco_work reco_pay reco_house reco_land 
 /PRINT UNIVARIATE INITIAL EXTRACTION FSCORE 
  /PLOT EIGEN
/CRITERIA FACTORS(1) ITERATE(25) 
 /EXTRACTION PC  
/ROTATION NOROTATE  
/SAVE REG(ALL) 
 /METHOD=CORRELATION .


*/DHS family empowerment index.

FACTOR
  /VARIABLES reco_purchase reco_visit reco_care reco_contra  V531 V212 V384A V384B V384C
  /MISSING MEANSUB 
  /ANALYSIS reco_purchase reco_visit reco_care reco_contra  V531 V212 V384A V384B V384C
 /PRINT UNIVARIATE INITIAL EXTRACTION FSCORE 
  /PLOT EIGEN
/CRITERIA FACTORS(1) ITERATE(25) 
 /EXTRACTION PC  
/ROTATION NOROTATE  
/SAVE REG(ALL) 
 /METHOD=CORRELATION.


 /* Review new indecies.
 
EXAMINE VARIABLES=V613 FAC1_all FAC1_eco FAC1_social FAC1_fam
  /PLOT BOXPLOT HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES EXTREME
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.
 

/* review distribution.

NPAR TESTS
  /K-S(NORMAL)=V613 nonnumAbility
  /K-S(POISSON)=V613 nonnumAbility
  /STATISTICS DESCRIPTIVES
  /MISSING ANALYSIS.


/* review  z-scores.

DESCRIPTIVES VARIABLES=FAC1_all FAC1_eco FAC1_social FAC1_fam
  /SAVE
  /STATISTICS=MEAN STDDEV MIN MAX VARIANCE.


/* remove extremes and outliers.

DATASET ACTIVATE DataSet1.
FILTER OFF.
USE ALL.
SELECT IF (ZFAC1_all <= 2.68 OR ZFAC1_eco <= 2.68 OR ZFAC1_social <= 2.68 OR ZFAC1_fam <= 2.68 OR 
    ZFAC1_all >= -2.68 OR ZFAC1_eco >= -2.68  OR ZFAC1_social  >= -2.68  OR ZFAC1_fam  >= -2.68 ).
EXECUTE.



