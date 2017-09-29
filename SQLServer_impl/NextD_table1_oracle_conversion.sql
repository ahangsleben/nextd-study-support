/*---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
-----                          Part 1: Defining Denominator or Study Sample                               -----  
--------------------------------------------------------------------------------------------------------------- 
---------------------------------------------------------------------------------------------------------------
-----                People with at least two encounters recorded on different days                       -----
-----                                                                                                     -----            
-----                       Encounter should meet the following requerements:                             -----
-----    Patient must be 18 years old >= age <= 89 years old during the encounter day.                    -----
-----    Encounter should be encounter types: 'AMBULATORY VISIT', 'EMERGENCY DEPARTMENT',                 -----
-----    'INPATIENT HOSPITAL STAY', 'EMERGENCY DEPARTMENT TO INPATIENT HOSPITAL STAY'.                    -----
-----                                                                                                     -----
-----          The date of the first encounter and total number of encounters is collected.               -----
---------------------------------------------------------------------------------------------------------------
*/

/* Check that the curated lab, med data is loaded.
Create them using extraction_tmp_ddl.sql and
import from med_info.csv, lab_review.csv respectively.
*/
/*
select case when labs > 0 and meds > 0 then 1
       else 1 / 0 end curated_data_loaded from (
  select
    (select count(*) from nextd_med_info) meds,
    (select count(*) from nextd_lab_review) labs
  from dual
);
*/

with age_at_visit as (
  select d.BIRTH_DATE
       , -datediff(yy, e.ADMIT_DATE, d.BIRTH_DATE) age
       , e.*
  from [PCORNET_CDM_r006].[dbo].ENCOUNTER e
  join [PCORNET_CDM_r006].[dbo].DEMOGRAPHIC d
  on e.PATID=d.PATID
)
select e.ENCOUNTERID, e.patid, e.BIRTH_DATE, e.admit_date, e.enc_type INTO #encounter_of_interest
from age_at_visit e
  where e.ENC_TYPE in ('IP', 'EI', 'AV', 'ED') 
  and e.age between 18 and 89 
;


/* 
Collect data to put summary for the study sample. 
Further data collection will be performed for this population: 
*/
/*          Get all encounters for each patient sorted by date: */     
with Denominator_init as(
select e.PATID, e.BIRTH_DATE, e.ADMIT_DATE
     , row_number() over (partition by e.PATID order by e.ADMIT_DATE asc) rn 
  from #encounter_of_interest e
)
/* Collect visits reported on different days: */
, Denomtemp0v as (
select distinct uf.PATID, uf.BIRTH_DATE, uf.ADMIT_DATE
, row_number() over (partition by uf.PATID order by uf.ADMIT_DATE asc) rn 
  from Denominator_init uf
)
/* Collect number of visits (from ones recorded on different days) for each person: */
, Denomtemp1v as (
select x.PATID, x.BIRTH_DATE, count(distinct x.ADMIT_DATE) as NumberOfVisits 
  from Denomtemp0v x
  group by x.PATID, x.BIRTH_DATE
)
/* Collect date of the first visit: */
, Denomtemp2v as (
select x.PATID, x.BIRTH_DATE, x.ADMIT_DATE as FirstVisit 
  from Denomtemp0v x
  where x.rn=1
)

select x.PATID, b.BIRTH_DATE, b.FirstVisit, x.NumberOfVisits INTO #DenominatorSummary
  from Denomtemp1v x
  left join Denomtemp2v b
  on x.PATID=b.PATID;

/* Constrain encounters using just #DenominatorSummary, not all of DEMOGRAPHIC. */
with age_at_visit as (
  select d.BIRTH_DATE
       , -datediff(yy, e.ADMIT_DATE, d.BIRTH_DATE) age
       , e.*
  from [PCORNET_CDM_r006].[dbo].ENCOUNTER e
  join #DenominatorSummary d
  on e.PATID=d.PATID
)
select e.ENCOUNTERID, e.patid, e.BIRTH_DATE, e.admit_date, e.enc_type INTO #encounter_type_age_denominator
  from age_at_visit e
  where e.ENC_TYPE in ('IP', 'EI', 'AV', 'ED') 
  and e.age between 18 and 89
;
/*-------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
-----                                    Part 2: Defining Pregnancy                                       ----- 
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
-----                             People with pregnancy-related encounters                                -----
-----                                                                                                     -----            
-----                       Encounter should meet the following requirements:                             -----
-----           Patient must be 18 years old >= age <= 89 years old during the encounter day.             -----
-----                                                                                                     -----
-----                 The date of the first encounter for each pregnancy is collected.                    -----
---------------------------------------------------------------------------------------------------------------
 Cases with miscarriage or abortion diagnosis codes:*/
select ds.PATID, dia.ADMIT_DATE INTO #Miscarr_Abort
  from #DenominatorSummary ds
  join [PCORNET_CDM_r006].[dbo].DIAGNOSIS dia 
  on ds.PATID=dia.PATID
  join [PCORNET_CDM_r006].[dbo].ENCOUNTER e
  on dia.ENCOUNTERID=e.ENCOUNTERID 
  join [PCORNET_CDM_r006].[dbo].DEMOGRAPHIC d
  on e.PATID=d.PATID
    where ((dia.DX LIKE '63[0|1|2|3|4|5|6|7|8|9].%' and dia.DX_TYPE = '09') or ((dia.DX LIKE '^O' or dia.DX LIKE 'A34.%' or dia.DX LIKE 'Z3[3|4|6]%') and dia.DX_TYPE = '10'))
    and -datediff(yy, dia.ADMIT_DATE, d.BIRTH_DATE) <= 89 
    and -datediff(yy, dia.ADMIT_DATE, d.BIRTH_DATE) >=18;

/*-- Cases with pregnancy and birth diagnosis codes:*/
select ds.PATID,dia.ADMIT_DATE INTO #Pregn_Birth
  from #DenominatorSummary ds
  join [PCORNET_CDM_r006].[dbo].DIAGNOSIS dia 
  on ds.PATID=dia.PATID
  join [PCORNET_CDM_r006].[dbo].ENCOUNTER e
  on dia.ENCOUNTERID=e.ENCOUNTERID 
  join [PCORNET_CDM_r006].[dbo].DEMOGRAPHIC d
  on e.PATID=d.PATID  
  where ((dia.DX LIKE '6[4|5|6|7][0|1|2|3|4|5|6|7|8|9].%' or dia.DX LIKE 'V2[2|3|8]%') and dia.DX_TYPE = '09')
  and -datediff(yy, dia.ADMIT_DATE, d.BIRTH_DATE) <= 89 
  and -datediff(yy, dia.ADMIT_DATE, d.BIRTH_DATE) >=18;

/* Cases with delivery procedures in ICD-9 coding:*/
select ds.PATID, p.ADMIT_DATE INTO #DelivProc
  from #DenominatorSummary ds
  join [PCORNET_CDM_r006].[dbo].PROCEDURES p 
  on ds.PATID=p.PATID
  join [PCORNET_CDM_r006].[dbo].ENCOUNTER e
  on p.ENCOUNTERID=e.ENCOUNTERID 
  join [PCORNET_CDM_r006].[dbo].DEMOGRAPHIC d
  on e.PATID=d.PATID  
    where ((p.PX LIKE '7[2|3|4|5].%' and p.PX_TYPE = '09') or (p.PX like '^1' and p.PX_TYPE = '10'))
    and -datediff(yy, p.ADMIT_DATE, d.BIRTH_DATE) <= 89 
    and -datediff(yy, p.ADMIT_DATE, d.BIRTH_DATE)>=18;

/* Cases with delivery procedures in CPT coding:    */
select ds.PATID, p.ADMIT_DATE INTO #PregProc
  from #DenominatorSummary ds
  join [PCORNET_CDM_r006].[dbo].PROCEDURES p 
  on ds.PATID=p.PATID
  join [PCORNET_CDM_r006].[dbo].ENCOUNTER e
  on p.ENCOUNTERID=e.ENCOUNTERID 
  join [PCORNET_CDM_r006].[dbo].DEMOGRAPHIC d
  on e.PATID=d.PATID  
  where (p.PX LIKE '59[0|1|2|3|4|5|6|7|8|9][0|1|2|3|4|5|6|7|8|9][0|1|2|3|4|5|6|7|8|9]' and p.PX_TYPE in ('C3', 'C4', 'CH'))
    /* Changed to include C4 (and CH for later updates) since it refers to the same thing as C3 according to the CDM 3.1 Specification */
  and -datediff(yy, p.ADMIT_DATE, d.BIRTH_DATE) <= 89 
  and -datediff(yy, p.ADMIT_DATE, d.BIRTH_DATE) >=18;

/*---------------------------------------------------------------------------------------------------------------
 Collect all encounters related to pregnancy:  */
select x.PATID,x.ADMIT_DATE INTO #AllPregnancyWithAllDates
  from
  (select a.PATID, a.ADMIT_DATE 
  from #Miscarr_Abort a
  union
  select b.PATID, b.ADMIT_DATE
  from #Pregn_Birth b
  union
  select c.PATID, c.ADMIT_DATE
  from #DelivProc c
  union
  select d.PATID, d.ADMIT_DATE
  from #PregProc d)x
  group by x.PATID, x.ADMIT_DATE;

/*---------------------------------------------------------------------------------------------------------------
-- Find separate pregnancy events:                                   
-- Calculate time difference between each pregnancy encounter, select the first encounter of each pregnancy event:  */
select x2.PATID,x2.ADMIT_DATE,x2.dif INTO #DeltasPregnancy
  from
  (select x.PATID, x.ADMIT_DATE, round(-datediff(MM, x.ADMIT_DATE, Lag(x.ADMIT_DATE, 1,NULL) OVER(partition by x.PATID ORDER BY x.ADMIT_DATE)), 0) as dif
  from #AllPregnancyWithAllDates x)x2
  where x2.dif is NULL or x2.dif>=12;

/* Number pregnancies:  */
select x.PATID, x.ADMIT_DATE, row_number() over (partition by x.PATID order by x.ADMIT_DATE asc) rn INTO #NumberPregnancy
  from #DeltasPregnancy x;

/* Transponse pregnancy table into single row per patient. Currently allows 21 sepearate pregnacy events:  */
select * INTO #FinalPregnancy
  from
  (select PATID, ADMIT_DATE, rn
  from #NumberPregnancy) AS unpivoted
  pivot (max(ADMIT_DATE) for rn in ([1],[2],[3],[4],[5],[6],[7],[8],[9],[10],[11],[12],[13],[14],[15],[16],[17],[18],[19],[20],[21])
  ) pivoted order by PATID;     


--Exclude pregnancy encounters from the encounters of interest and create a new view from the remaining encounters
select * INTO #encounter_exclude_pregnancy from #encounter_of_interest
where encounterid not in
    (
    --Find encounters that fall within one year of pregnancy admit dates
    select /*distinct*/ eoi.encounterid 
    --eoi.birth_date, eoi.enc_type, eoi.patid, np.patid, 
    --eoi.admit_date, np.admit_date, (eoi.admit_date - np.admit_date) as date_diff
    from #encounter_of_interest eoi
    join #NumberPregnancy np
    on eoi.patid = np.patid
    where abs(-datediff(dd, eoi.admit_date, np.admit_date)) <= 365
    )
;


/*-------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
-----                 Part4: Combine results from all parts of the code into final table:                 -----
---------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------------------------------------------
-----                               Table FinalStatTable is Table 1                                      -----
-----                          will be used for post-processing analysis                                  ------
-----                             
-------------------------------------------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------------------------------------------*/
/* Data Request for 9/5: Without the DM onset column */
/*-------------------------------------------------------------------------------------------------------------*/
CREATE TABLE SubTable1_for_export
  (PATID VARCHAR(128) NOT NULL, 
  FirstVisit date NULL, 
  NumberOfVisits INT, 
  DEATH_DATE date NULL,
  PregnancyDate_1 date NULL, 
  PregnancyDate_2 date NULL, 
  PregnancyDate_3 date NULL, 
  PregnancyDate_4 date NULL, 
  PregnancyDate_5 date NULL,
  PregnancyDate_6 date NULL, 
  PregnancyDate_7 date NULL, 
  PregnancyDate_8 date NULL, 
  PregnancyDate_9 date NULL, 
  PregnancyDate_10 date NULL,
  PregnancyDate_11 date NULL, 
  PregnancyDate_12 date NULL, 
  PregnancyDate_13 date NULL, 
  PregnancyDate_14 date NULL, 
  PregnancyDate_15 date NULL,
  PregnancyDate_16 date NULL, 
  PregnancyDate_17 date NULL, 
  PregnancyDate_18 date NULL, 
  PregnancyDate_19 date NULL, 
  PregnancyDate_20 date NULL,
  PregnancyDate_21 date NULL);


insert into SubTable1_for_export 
select ds.PATID, ds.FirstVisit, ds.NumberOfVisits, d.DEATH_DATE, 
p.[1], p.[2], p.[3], p.[4], p.[5],
p.[6], p.[7], p.[8], p.[9], p.[10],
p.[11], p.[12], p.[13], p.[14], p.[15],
p.[16], p.[17], p.[18], p.[19], p.[20],
p.[21]
  from #DenominatorSummary ds
  left join [PCORNET_CDM_r006].[dbo].DEATH d
  on ds.PATID=d.PATID
  left join #FinalPregnancy p
  on ds.PATID=p.PATID;


/* Interactive investigational code commented out 

select count(distinct patid) from SubTable1_for_export;
select * from SubTable1_for_export;

-- For dates, only show YYYY-MM 
select patid, to_char(firstvisit, 'YYYY-MM') firstvisit, numberofvisits, to_char(death_date, 'YYYY-MM') death_date, 
to_char(PregnancyDate_1, 'YYYY-MM') PregnancyDate1, to_char(PregnancyDate_2, 'YYYY-MM') PregnancyDate2, to_char(PregnancyDate_3, 'YYYY-MM') PregnancyDate3, to_char(PregnancyDate_4, 'YYYY-MM') PregnancyDate4, to_char(PregnancyDate_5, 'YYYY-MM') PregnancyDate5,
to_char(PregnancyDate_6, 'YYYY-MM') PregnancyDate6, to_char(PregnancyDate_7, 'YYYY-MM') PregnancyDate7, to_char(PregnancyDate_8, 'YYYY-MM') PregnancyDate8, to_char(PregnancyDate_9, 'YYYY-MM') PregnancyDate9, to_char(PregnancyDate_10, 'YYYY-MM') PregnancyDate10, 
to_char(PregnancyDate_11, 'YYYY-MM') PregnancyDate11, to_char(PregnancyDate_12, 'YYYY-MM') PregnancyDate12, to_char(PregnancyDate_13, 'YYYY-MM') PregnancyDate13, to_char(PregnancyDate_14, 'YYYY-MM') PregnancyDate14, to_char(PregnancyDate_15, 'YYYY-MM') PregnancyDate15,
to_char(PregnancyDate_16, 'YYYY-MM') PregnancyDate16, to_char(PregnancyDate_17, 'YYYY-MM') PregnancyDate17, to_char(PregnancyDate_18, 'YYYY-MM') PregnancyDate18, to_char(PregnancyDate_19, 'YYYY-MM') PregnancyDate19, to_char(PregnancyDate_20, 'YYYY-MM') PregnancyDate20,
to_char(PregnancyDate_21, 'YYYY-MM') PregnancyDate21 from SubTable1_for_export;
*/