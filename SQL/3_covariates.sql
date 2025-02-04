
/* 1.1.)----------------------------------------------------
			Demographics (gender, race, ethnicity, etc.)
*/

SELECT * 
INTO #demoFY23
FROM PCS_HCOE.BLFY23.Demographics


/* 1.2.)----------------------------------------------------
			SMK 
*/

DROP TABLE IF EXISTS #smk;
with  smk1 as (
	SELECT a.ScrSSN, a.StartDate,
			hf.HealthFactorDateTime, hf.HealthFactorDateSID,
			dim.HealthFactorType, s.SmokingFactor
		FROM Dflt.Exposure as a inner join Src.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
								inner join CDWWork.HF.HealthFactor as hf on x.PatientSID = hf.PatientSID
								inner join CDWWork.Dim.HealthFactorType as dim on hf.HealthFactorTypeSID = dim.HealthFactorTypeSID										 
								inner join Dflt.dim_HF_SMOKING as s on dim.HealthFactorType = s.HEALTHFACTORTYPE
			WHERE HealthFactorDateTime <= StartDate
	),
	Smoking_ever as (
 --count each instance regardless of timing
SELECT count(*) as cnt, scrssn, StartDate, smokingFactor
  FROM smk1
  group by scrssn, StartDate, smokingFactor
  ), 
  smoking_wide as (
 --create one record per person file
	select pts.scrssn, pts.StartDate,
	   case when SMKcurrent > 0 then SMKcurrent else 0 end as SMKcurrent, 
	   case when SMKpast > 0 then SMKpast else 0 end as SMKpast, 
	   case when SMKnever > 0 then SMKnever else 0 end as SMKnever
	from
	  (select distinct scrssn, StartDate from smoking_ever) pts  --all patients in file
	  left join
	  (select scrssn, StartDate, cnt as SMKcurrent
	  FROM Smoking_ever
	  where smokingfactor = 'CURRENT SMOKER') a
	  on pts.scrssn = a.scrssn and pts.StartDate = a.StartDate
	  left join
	  (select scrssn, StartDate, cnt as SMKpast
	  from Smoking_ever
	  where smokingfactor = 'FORMER SMOKER') b
	  on pts.scrssn = b.scrssn and pts.StartDate = b.StartDate
	  left join
	  (select scrssn, StartDate, cnt as SMKnever
	  from Smoking_ever
	  where smokingfactor = 'NEVER SMOKER') C
	  ON pts.SCRSSN = C.SCRSSN and pts.StartDate = c.StartDate
  ),
  Smoking_smkhfcom as (
  --select most frequent response into smkhfcom variable
  --0=Never smoked; 1=current smoker; 2=past smoker
  select a.*, b.MostcommonNum
  ,case when b.mostcommonnum = 0 then null
		when a.SMKcurrent = b.MostcommonNum then 1
		when a.SMKpast = b.MostcommonNum then 2
		when a.smknever = b.mostcommonnum then 0 end as SMKHFCOM
  from smoking_wide a
  inner join
  (
  select scrssn, StartDate, max(counts) as MostcommonNum
  from
  (select scrssn, StartDate, smkcurrent as counts
  from Smoking_wide
  union
  select scrssn, StartDate, smkpast as counts
  from Smoking_wide
  union
  select scrssn, StartDate, smknever as counts
  from Smoking_wide) sub
  group by scrssn, StartDate) b
  on a.scrssn = b.scrssn and a.StartDate = b.StartDate
  ), 
  mostcomm as (SELECT ScrSSN, StartDate, SMKHFCOM from Smoking_smkhfcom
  	where mostcommonnum > 0                                                                      
	)
  --create permanent dataset
  select a.ScrSSN, a.StartDate, b.SMKHFCOM
  into #smk
  from Dflt.Exposure as a left join mostcomm as b on a.ScrSSN = b.ScrSSN and a.StartDate = b.StartDate
  GO


/* 1.3.)----------------------------------------------------
			GISURH, residency
*/

DROP TABLE IF EXISTS #GIS;
with g0 as (
SELECT DISTINCT a.ScrSSN, a.startdate, GISPatientAddressLongitude, GISPatientAddressLatitude, b.URH, b.zip, GISAddressUpdatedDate,
	ABS(DATEDIFF(DAY, GISAddressUpdatedDate, a.startdate)) as diff		-- most recent address
FROM Dflt.Exposure as a left join Src.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
						left join SRC.SPatient_SPatientGISAddress as b on x.PatientSID = b.PatientSID),
	g1 as (
	SELECT *, 
			row_number() over (partition by scrssn, startdate order by scrssn, startdate, diff) as RN
		FROM g0)
SELECT *
INTO #GIS
FROM g1
WHERE rn = 1
GO

/* 1.4.)----------------------------------------------------
			Area deprivation index (2015)
*/

DROP TABLE IF EXISTS #ADI_ALL;
SELECT PATIENTICN, Year, quarter, FIPS_GEOID, ADI_NATRANK, ADI_STATERNK
INTO #ADI_ALL
	FROM [VINCI_ADI].[ADI].[Pat_PSSG]
	where Year >= 2007
GO

-- match by baseline year
DROP TABLE IF EXISTS #ADI1;
with a0 as (
		SELECT ScrSSN, startdate, 
				YEAR(dateadd(mm,3,startdate)) as BLYear
		FROM Dflt.Exposure)
SELECT a.ScrSSN, a.startdate, a.BLYear, b.*
INTO #ADI1
from a0 as a left join Src.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
			 left join #ADI_ALL as b on x.PatientICN = b.PATIENTICN and a.BLYear = b.year

-- dup records, find most frequent one
DROP TABLE IF EXISTS #ADI2;
with ranked as (
	SELECT ScrSSN, startdate, FIPS_GEOID, ADI_NATRANK, count(*) as freq,
			DENSE_RANK() OVER(PARTITION BY ScrSSN, startdate ORDER BY ScrSSN, startdate, count(*) DESC) as ranking 
	from #ADI1
	group by ScrSSN, startdate, FIPS_GEOID, ADI_NATRANK
	)
SELECT *
into #ADI2 
from ranked 
where ranking = 1


SELECT ScrSSN, startdate, min(ADI_NATRANK) as ADI_NATRANK
into #ADI3
from #ADI2
group by ScrSSN, startdate


/* 2.1.)----------------------------------------------------
			Vital sign, all weight measurements
*/

DROP TABLE IF EXISTS #Weights;
SELECT a.ScrSSN,
	   b.VitalResultNumeric as WEIGHT, b.VitalSignTakenDateTime
INTO #Weights
FROM  (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a INNER JOIN Src.CohortCrossWalk as x on a.scrssn = x.ScrSSN
													   INNER JOIN Src.Vital_VitalSign as b on x.PatientSID = b.PatientSID
													   INNER JOIN CDWWork.Dim.VitalType as c on b.VitalTypeSID = c.VitalTypeSID
WHERE VitalSignTakenDateTime IS NOT NULL
		AND VitalResultNumeric IS NOT NULL
		AND VitalResultNumeric > 75
		AND VitalResultNumeric <700			-- biological implausible
		AND VitalType = 'WEIGHT'
		AND VitalSignTakenDateTime <= CAST('2024-09-30' as date)

-- create permanent table for time-updated cov
DROP TABLE IF EXISTS Dflt.Weights;
SELECT ScrSSN, WEIGHT, VitalSignTakenDateTime, cast(VitalSignTakenDateTime as date) as VitalSignDate
INTO Dflt.Weights
	FROM #Weights


-- baseline weight
DROP TABLE IF EXISTS #weightBL;
WITH w0 as (
	SELECT a.ScrSSN, a.startdate, 
			b.WEIGHT, b.VitalSignTakenDateTime,
			row_number() over (partition by a.scrssn, a.startdate order by a.scrssn, a.startdate, VitalSignTakenDateTime DESC) as RN
	FROM Dflt.Exposure as a left join #Weights as b on a.ScrSSN = b.ScrSSN
		WHERE VitalSignTakenDateTime <= startdate
	)
SELECT DISTINCT ScrSSN, startdate, WEIGHT as Weight_BL, VitalSignTakenDateTime as WeightDT
INTO #weightBL
from w0
where rn = 1

/* 2.2.)----------------------------------------------------
			Vital sign, baseline height
*/

DROP TABLE IF EXISTS #heightBL;
with h0 as (
		SELECT a.ScrSSN, a.startdate,
			b.VitalResultNumeric as HEIGHT, b.VitalSignTakenDateTime
	FROM Dflt.Exposure as a INNER JOIN SRC.CohortCrosswalk as x on a.scrssn = x.ScrSSN
							INNER JOIN Src.Vital_VitalSign as b on x.PatientSID = b.PatientSID
							INNER JOIN CDWWork.Dim.VitalType as c on b.VitalTypeSID = c.VitalTypeSID
		WHERE VitalSignTakenDateTime <= startdate
			AND VitalSignTakenDateTime IS NOT NULL
			AND VitalResultNumeric IS NOT NULL
			AND VitalResultNumeric > 48
			AND VitalResultNumeric < 84		-- biological implausible
			AND VitalType = 'HEIGHT'
)
SELECT distinct ScrSSN, startdate,
	   PERCENTILE_DISC(0.5) WITHIN GROUP (ORDER BY HEIGHT) OVER (PARTITION BY ScrSSN, startdate) as HEIGHT_BL	-- median value to avoid error measures
INTO #heightBL
from h0
GO


/* 2.3.)----------------------------------------------------
			Vital sign, blood pressure
*/

DROP TABLE IF EXISTS #BP_all;
SELECT a.ScrSSN, 
	   dim.VitalType,
	   b.VitalSignTakenDateTime, b.VitalResult, b.Systolic, b.Diastolic
INTO Dflt.BP_all
	FROM (SELECT DISTINCT ScrSSN from Dflt.Exposure) as a inner join Src.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
														  inner join Src.Vital_VitalSign as b on x.PatientSID = b.PatientSID
														  inner join CDWWork.Dim.VitalType as dim on b.VitalTypeSID = dim.VitalTypeSID
	WHERE dim.VitalType in ('BLOOD PRESSURE')
		AND VitalSignTakenDateTime IS NOT NULL
		AND Systolic is not null 
		AND Diastolic is not null
		AND Systolic != 0 
		AND Diastolic != 0
		AND Systolic > Diastolic
		AND VitalSignTakenDateTime <= CAST('2024-09-30' as date)
GO

-- baseline BP
DROP TABLE IF EXISTS #BP_BL;
with bp1 as (
	SELECT a.ScrSSN, startdate, b.VitalSignTakenDateTime, Systolic, Diastolic
		FROM Dflt.Exposure as a left join Dflt.BP_all as b on a.ScrSSN = b.ScrSSN
		 where VitalSignTakenDateTime <= startdate
	),
	bp2 as (
		SELECT *,
				ROW_NUMBER() OVER (PARTITION BY ScrSSN, startdate ORDER BY ScrSSN, startdate, VitalSignTakenDateTime DESC) AS RN
		FROM bp1
	)
SELECT  * 
	into #BP_BL
	from bp2
	where RN = 1
GO

/* 3.)----------------------------------------------------
			Care Assessment Need (CAN) score
*/


drop table if exists #can;
with can0 as(
SELECT a.ScrSSN, a.startdate,
	   b.cMort_1y as CAN_score, 
	   b.riskDate
	FROM Dflt.Exposure as a left join Src.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
							left join Src.can_weekly_report_V2_5_history as b on x.PatientICN = b.PatientICN
	where b.riskDate <= startdate 
	  AND b.riskDate >= dateadd(dd, -30, CAST(startdate as datetime2(0)))
	),
  can1 as (
select *,
		ROW_NUMBER() OVER (PARTITION BY scrssn, startdate ORDER BY scrssn, startdate, riskDate desc) AS RN
from can0),
  can2 as (
  select * 
	from can1 
	where rn = 1
  )
select a.ScrSSN, a.startdate, b.CAN_score
into #can
from Dflt.Exposure as a left join can2 as b on a.ScrSSN = b.ScrSSN and a.startdate = b.startdate

SELECT TOP(1000) * from #test order by scrssn, startdate



/* 4.)----------------------------------------------------
		Comorbidity
			2 outpatient dx or 1 inpatient discharge dx
*/

-- match ICDSID
SELECT DISTINCT ICD9SID, ICD9Code, condition
INTO #Dim_ICD9SID
	FROM CDWWork.Dim.ICD9 as x inner join Dflt.dim_ICD910 as a on x.ICD9Code = a.ICDCode
WHERE ICDType = 9 
GO

SELECT DISTINCT ICD10SID, ICD10Code, condition
INTO #Dim_ICD10SID
	FROM CDWWork.Dim.ICD10 as x inner join Dflt.dim_ICD910 as a on x.ICD10Code = a.ICDCode
WHERE ICDType = 10
GO

/* 4.1.)----------------------------------------------------
			Comorbidity from VHA
*/

DROP TABLE IF EXISTS Dflt.Comorbidites_long_VHA;
WITH in1 AS
	(SELECT a.scrssn, condition, dischargedatetime
		FROM Src.Inpat_InpatientDiagnosis id inner join #Dim_ICD9SID on id.ICD9SID = #Dim_ICD9SID.ICD9SID 
											 inner join Src.CohortCrosswalk x on id.PatientSID = x.PatientSID
											 inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.scrssn = x.scrssn
	UNION
	 SELECT a.scrssn, condition, dischargedatetime
		FROM Src.Inpat_InpatientDiagnosis id inner join #Dim_ICD10SID on id.ICD10SID = #Dim_ICD10SID.ICD10SID
											 inner join Src.CohortCrosswalk x on id.PatientSID = x.PatientSID
											 inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.scrssn = x.scrssn
		 ), 
  firstin AS
	 (SELECT scrssn, condition, MIN(dischargedatetime) as recDate
	  FROM in1
	  GROUP BY ScrSSN, condition
	  ), 
  ot1 AS
	 (SELECT a.scrssn, condition, vdiagnosisdatetime
		FROM Src.Outpat_VDiagnosis id inner join #Dim_ICD9SID on id.ICD9SID = #Dim_ICD9SID.ICD9SID
									  inner join Src.CohortCrosswalk x on id.PatientSID = x.PatientSID
									  inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.scrssn = x.scrssn
	UNION
	  SELECT a.scrssn, condition, vdiagnosisdatetime
		FROM Src.Outpat_VDiagnosis id inner join #Dim_ICD10SID on id.ICD10SID = #Dim_ICD10SID.ICD10SID
									  inner join Src.CohortCrosswalk x on id.PatientSID = x.PatientSID
									  inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.scrssn = x.scrssn
	 ), 
  ot2 AS
	 (SELECT scrssn, condition, vdiagnosisdatetime 
	 ,ROW_NUMBER() OVER (PARTITION BY scrssn, condition ORDER BY scrssn, condition, VDIAGNOSISDATETIME) AS RN
	 FROM ot1
	 ), 
  secot AS
	 (SELECT scrssn, condition, vdiagnosisdatetime as recdate
	 FROM ot2
	 where RN = 2
	 ), 
  comb AS
	 (SELECT scrssn, condition, recdate
	   FROM firstin
	UNION
	   SELECT scrssn, condition, recdate
	   FROM secot
	 ), 
  comb2 AS
	 (SELECT scrssn, condition, recdate
	  ,ROW_NUMBER() OVER (PARTITION BY scrssn, condition ORDER BY scrssn, condition, recdate) AS RN
	  FROM comb
	  ) 
SELECT scrssn, condition, recdate, 'VHA' as source
	INTO Dflt.Comorbidites_long_VHA
		FROM comb2
		WHERE rn = 1
GO


/* 4.2.)----------------------------------------------------
			Comorbidity from Medicare inpat/outpat/carrier
*/

DROP TABLE IF EXISTS Dflt.Comorbidites_long_medicare;
with medicare_out as (
	SELECT ScrSSn, 
		   THRU_DT as recdate, 
		   CASE WHEN THRU_DT < CAST('2015-10-01' as date) THEN 9 ELSE 10 END AS ICDType,
		   CASE WHEN THRU_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' AND LEN(ICD_nodecimal) = 4 THEN ICD_nodecimal 
				WHEN THRU_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' THEN STUFF(ICD_nodecimal, 5, 0, '.')
				WHEN LEN(ICD_nodecimal) = 3 THEN ICD_nodecimal
			ELSE STUFF(ICD_nodecimal, 4, 0, '.') END as ICDCode,
		   ICD_nodecimal
		FROM Dflt.MedicaREDx_outpat
			WHERE CLM_TYPE in (40,		-- hospital outpatient claim
							   4012,	-- Inpat but covered by Part B
							   4013,	-- hosp outpat
							   4014,	-- lab
							   4083,	-- Ambulatory surgery center
							   4085,	-- Critical access hospital
							   4089)	-- other special facility
			  AND FAC_TYPE in (1, 8)    -- 1 = hospital; 8 = ambulatory surgery center and other special facility
	UNION
	SELECT ScrSSN,
		   THRU_DT as recdate,
		   CASE WHEN THRU_DT < CAST('2015-10-01' as date) THEN 9 ELSE 10 END AS ICDType,
		   CASE WHEN THRU_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' AND LEN(ICD_nodecimal) = 4 THEN ICD_nodecimal 
				WHEN THRU_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' THEN STUFF(ICD_nodecimal, 5, 0, '.')
				WHEN LEN(ICD_nodecimal) = 3 THEN ICD_nodecimal
			ELSE STUFF(ICD_nodecimal, 4, 0, '.') END as ICDCode,
		   ICD_nodecimal
		FROM Dflt.MedicaREDx_carrier
			WHERE CLM_TYPE IN (4700,		-- professional, outpatient hospital services
							   71)			-- local carrier non-DME claim

			  AND (TYPSRVCB in ('1',		-- medical care
								'2',		-- surgery
								'3',		-- consultation
								'F',		-- ambulatory surgical center
								'T')		-- outpatient mental health
					OR TYPSRVCB IS NULL)
	 ),
	medicare_in as (
	SELECT ScrSSN, 
		   THRU_DT as recdate,
		   CASE WHEN THRU_DT < CAST('2015-10-01' as date) THEN 9 ELSE 10 END AS ICDType,
		   CASE WHEN THRU_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' AND LEN(ICD_nodecimal) = 4 THEN ICD_nodecimal 
				WHEN THRU_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' THEN STUFF(ICD_nodecimal, 5, 0, '.')
				WHEN LEN(ICD_nodecimal) = 3 THEN ICD_nodecimal
			ELSE STUFF(ICD_nodecimal, 4, 0, '.') END as ICDCode,
		   ICD_nodecimal
		FROM Dflt.MedicaREDx_inpat
	),
	in1 as (
	SELECT a.ScrSSN, condition, recdate
		FROM medicare_in as b inner join Dflt.dim_ICD910 as z on b.ICDCode = z.ICDCode AND b.ICDType = z.ICDType
							  inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.ScrSSN = b.ScrSSN
	),
  firstin AS
	 (SELECT scrssn, condition, MIN(recdate) as recdate
	  FROM in1
	  GROUP BY ScrSSN, condition
	  ), 
  ot1 AS
	 (SELECT a.scrssn, condition, recdate
		FROM medicare_out as b inner join Dflt.dim_ICD910 as z on b.ICDCode = z.ICDCode AND b.ICDType = z.ICDType
							   inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.scrssn = b.scrssn
	 ), 
  ot2 AS
	 (SELECT scrssn, condition, recdate 
	 ,ROW_NUMBER() OVER (PARTITION BY scrssn, condition ORDER BY scrssn, condition, recdate) AS RN
	 FROM ot1
	 ), 
  secot AS
	 (SELECT scrssn, condition, recdate
	 FROM ot2
	 where RN = 2
	 ), 
  comb AS
	 (SELECT scrssn, condition, recdate
	   FROM firstin
	UNION
	   SELECT scrssn, condition, recdate
	   FROM secot
	 ), 
  comb2 AS
	 (SELECT scrssn, condition, recdate
	  ,ROW_NUMBER() OVER (PARTITION BY scrssn, condition ORDER BY scrssn, condition, recdate) AS RN
	  FROM comb
	  ) 
SELECT scrssn, condition, recdate, 'MedicaRE' as source
	INTO Dflt.Comorbidites_long_medicare
		FROM comb2
		WHERE rn = 1
GO


/* 4.3.)----------------------------------------------------
			Comorbidity from Medicaid inpat/outpat
*/

DROP TABLE IF EXISTS Dflt.Comorbidites_long_medicaid;
with medicaid_in as (
	SELECT ScrSSN, 
		   SRVC_END_DT as recdate,
		   CASE WHEN SRVC_END_DT < CAST('2015-10-01' as date) THEN 9 ELSE 10 END AS ICDType,
		   CASE WHEN SRVC_END_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' AND LEN(ICD_nodecimal) = 4 THEN ICD_nodecimal 
				WHEN SRVC_END_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' THEN STUFF(ICD_nodecimal, 5, 0, '.')
				WHEN LEN(ICD_nodecimal) = 3 THEN ICD_nodecimal
			ELSE STUFF(ICD_nodecimal, 4, 0, '.') END as ICDCode,
		   ICD_nodecimal
		FROM Dflt.MedicaIDDx_inpat
	),
	medicaid_out as (
	SELECT ScrSSN, 
		   SRVC_END_DT as recdate,
		   CASE WHEN SRVC_END_DT < CAST('2015-10-01' as date) THEN 9 ELSE 10 END AS ICDType,
		   CASE WHEN SRVC_END_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' AND LEN(ICD_nodecimal) = 4 THEN ICD_nodecimal 
				WHEN SRVC_END_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' THEN STUFF(ICD_nodecimal, 5, 0, '.')
				WHEN LEN(ICD_nodecimal) = 3 THEN ICD_nodecimal
			ELSE STUFF(ICD_nodecimal, 4, 0, '.') END as ICDCode,
		   ICD_nodecimal
		FROM Dflt.MedicaIDDx_outpat
		-- WHERE MSIS_TOS in (8, 10, 11, 12, 20, 21, 22, 23, 53)
		--	  OR TOS_CD in ('002', '012', '015', '023', '026', '028', '060', '119', '120', '122')
	),
	in1 as (
	SELECT a.ScrSSN, condition, recdate
		FROM medicaid_in as b inner join Dflt.dim_ICD910 as z on b.ICDCode = z.ICDCode AND b.ICDType = z.ICDType
							  inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.ScrSSN = b.ScrSSN
	),
  firstin AS
	 (SELECT scrssn, condition, MIN(recdate) as recdate
	  FROM in1
	  GROUP BY ScrSSN, condition
	  ), 
  ot1 AS
	 (SELECT a.scrssn, condition, recdate
		FROM medicaid_out as b inner join Dflt.dim_ICD910 as z on b.ICDCode = z.ICDCode AND b.ICDType = z.ICDType
							   inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.scrssn = b.scrssn
	 ), 
  ot2 AS
	 (SELECT scrssn, condition, recdate 
	 ,ROW_NUMBER() OVER (PARTITION BY scrssn, condition ORDER BY scrssn, condition, recdate) AS RN
	 FROM ot1
	 ), 
  secot AS
	 (SELECT scrssn, condition, recdate
	 FROM ot2
	 where RN = 2
	 ), 
  comb AS
	 (SELECT scrssn, condition, recdate
	   FROM firstin
	UNION
	   SELECT scrssn, condition, recdate
	   FROM secot
	 ), 
  comb2 AS
	 (SELECT scrssn, condition, recdate
	  ,ROW_NUMBER() OVER (PARTITION BY scrssn, condition ORDER BY scrssn, condition, recdate) AS RN
	  FROM comb
	  ) 
SELECT scrssn, condition, recdate, 'MedicaID' as source
	INTO Dflt.Comorbidites_long_medicaid
		FROM comb2
		WHERE rn = 1
GO

/* 4.4.)----------------------------------------------------
			Comorbidity from VA Community Care Data
*/

-- inpat
drop table if exists Dflt.CommunityCare_inpat_clean;

-- PIT institutional claims
SELECT DISTINCT a.ScrSSN,
	   b.StatementFromDate as startdate, b.StatementToDate as enddate, 'PIT_Institutional' as ClaimSource,
	   dx.PITDiagnosisCode as ICDCode, 
	   -- create discharge dx position
	   CASE WHEN Qualifier = 'PRIN' THEN 1
			WHEN Qualifier like 'OTH%' THEN CAST(SUBSTRING(Qualifier, PATINDEX('%[0-9]%', Qualifier), LEN(Qualifier)) AS INT)+1
			ELSE NULL END AS ICDOrder,
	   -- ICD-9 or 10
	   CASE WHEN dx.PITDiagnosisCode like '[EV]%' and StatementToDate < CAST('2015-10-01' as date) THEN 9
			WHEN dx.PITDiagnosisCodeVersion = 'ICD9' THEN 9
			WHEN dx.PITDiagnosisCodeVersion = 'ICD10' THEN 10
			ELSE NULL END AS ICDType
INTO Dflt.CommunityCare_inpat_clean
	FROM Src.CohortCrosswalk as x inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.ScrSSN = x.ScrSSN
								  inner join Src.PIT_PITInstitutionalClaim_Archive as b on x.PatientSID = b.PITPatientSID
								  inner join CDWWork_Archive.NDim.PITBillType as y on b.PITBillTypeSID = y.PITBillTypeSID
								  inner join Src.PIT_PITClaimDiagnosis_Archive as c on b.PITInstitutionalClaimSID = c.PITInstitutionalClaimSID
								  inner join CDWWork_Archive.NDim.PITDiagnosisCode as dx on c.PITDiagnosisCodeSID = dx.PITDiagnosisCodeSID
	WHERE len(pitbilltypecode) = 3
	  AND PITBillTypeCode like '1[12]%'
UNION
-- PIT professional claims
SELECT DISTINCT a.ScrSSN,
	   d.ServiceFromDate as startdate, d.ServiceToDate as enddate, 'PIT_Professional' as ClaimSource,
	   dx.PITDiagnosisCode as ICDCode, cast(c.PointerOrder as int) as ICDOrder,
	   CASE WHEN dx.PITDiagnosisCode like '[EV]%' and ServiceToDate < CAST('2015-10-01' as date) THEN 9
			WHEN dx.PITDiagnosisCodeVersion = 'ICD9' THEN 9
			WHEN dx.PITDiagnosisCodeVersion = 'ICD10' THEN 10
			ELSE NULL END AS ICDType
	FROM Src.CohortCrossWalk as x inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.ScrSSN = x.ScrSSN
								  inner join Src.PIT_PITProfessionalClaim_Archive as b on x.PatientSID = b.PITPatientSID
								  inner join Src.PIT_PITProfessionalClaimDetails_Archive as d on b.PITClaimSID = d.PITClaimSID
								  inner join CDWWork_Archive.NDim.PITPlaceOfService as y on d.PITPlaceOfServiceSID = y.PITPlaceOfServiceSID
								  inner join Src.PIT_PITProfessionalClaimDiagnosis_Archive as c on c.PITProfessionalClaimDetailsSID = d.PITProfessionalClaimDetailsSID
								  inner join CDWWork_Archive.NDim.PITDiagnosisCode as dx on c.PITDiagnosisCodeSID = dx.PITDiagnosisCodeSID
	WHERE PITPlaceOfServiceCode in ('21')
UNION
-- FEE domain
SELECT DISTINCT pt.ScrSSN,
	   CAST(z.TreatmentFromDateTime as date) as startdate,
	   CAST(z.TreatmentToDateTime as date) as enddate, 
	   'Fee' as ClaimSource,
	   CASE WHEN a.ICD9SID <> -1 THEN icd9.ICD9Code
			WHEN a.ICD10SID <> 0 THEN icd10.ICD10Code
			END AS ICDCode,
	   a.OrdinalNumber as ICDOrder, 
	   CASE WHEN a.ICD9SID <> -1 THEN 9
			WHEN a.ICD10SID <> 0 THEN 10
			END AS ICDType
	  FROM Src.Fee_FeeInpatInvoiceICDDiagnosis as a inner join Src.CohortCrosswalk as x on a.PatientSID = x.PatientSID
													inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as pt on pt.ScrSSN = x.ScrSSN
													inner join CDWWork.DIM.ICD10 as icd10 on a.ICD10SID = icd10.ICD10SID
													inner join CDWWork.Dim.ICD9 as icd9 on a.ICD9SID = icd9.ICD9SID
													inner join Src.Fee_Feeinpatinvoice as z on a.FeeInpatInvoiceSID = z.FeeInpatInvoiceSID
UNION
-- IVC CDS claims
SELECT DISTINCT a.ScrSSN, 
				CAST(COALESCE(Admission_Date, Service_Start_Date) AS DATE) AS startdate,			
				CAST(COALESCE(Discharge_Date, Service_End_Date) AS DATE) AS enddate,
				'IVC CDS' as ClaimSource,
		   CASE WHEN CAST(COALESCE(Discharge_Date, Service_End_Date) AS DATE) < CAST('2015-10-01' as date) AND c.ICD like 'E%' AND LEN(c.ICD) = 4 THEN c.ICD 
				WHEN CAST(COALESCE(Discharge_Date, Service_End_Date) AS DATE) < CAST('2015-10-01' as date) AND c.ICD like 'E%' THEN STUFF(c.ICD, 5, 0, '.')
				WHEN LEN(c.ICD) = 3 THEN c.ICD
				ELSE STUFF(c.ICD, 4, 0, '.') END as ICDCode,
				c.Position as ICDOrder, 
				CASE WHEN CAST(COALESCE(Discharge_Date, Service_End_Date) AS DATE) < CAST('2015-10-01' as date) THEN 9
					 WHEN CAST(COALESCE(Discharge_Date, Service_End_Date) AS DATE) >= CAST('2015-10-01' as date) THEN 10 
					 ELSE NULL END as ICDType
		FROM (SELECT DISTINCT ScrSSN FROM Dflt.Exposure_20241028) as a left join Src.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
																	 left join Src.IVC_CDS_CDS_Claim_Header as b on b.Patient_ICN = x.PatientICN
																	 left join Src.IVC_CDS_CDS_Claim_Diagnosis as c on b.ClaimSID = c.ClaimSID
	WHERE c.ICD IS NOT NULL
	  AND (Bill_Type like '1[12]%'
		   or Place_of_Service_ID in ('21'))


-- Community Care outpat
DROP TABLE IF exists Dflt.CommunityCare_outpat_clean;
-- PIT institutional
SELECT DISTINCT a.ScrSSN,
	   /* tob.PITBillTypeCode, tob.PITBillTypeDesignation, tob.BillClassification, tob.PITBillTypeDescription, */
	   b.StatementFromDate as startdate, b.StatementToDate as enddate, 
	   'PIT_Institutional' as ClaimSource,
	   dx.PITDiagnosisCode as ICDCode, 
	   CASE WHEN dx.PITDiagnosisCode like '[EV]%' and StatementToDate < CAST('2015-10-01' as date) THEN 9
			WHEN dx.PITDiagnosisCodeVersion = 'ICD9' THEN 9
			WHEN dx.PITDiagnosisCodeVersion = 'ICD10' THEN 10
			ELSE NULL END AS ICDType
	into Dflt.CommunityCare_outpat_clean
	FROM Src.CohortCrosswalk as x inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.ScrSSN = x.ScrSSN
								  inner join Src.PIT_PITInstitutionalClaim_Archive as b on x.PatientSID = b.PITPatientSID
								  inner join Src.PIT_PITClaimDiagnosis_Archive as c on b.PITInstitutionalClaimSID = c.PITInstitutionalClaimSID
								  inner join CDWWork_Archive.NDim.PITDiagnosisCode as dx on c.PITDiagnosisCodeSID = dx.PITDiagnosisCodeSID
								  inner join CDWWork_Archive.NDim.PITBillType as tob on b.PITBillTypeSID = tob.PITBillTypeSID
	WHERE substring(PITBillTypeCode, 1, 2) in ('13', '71', '73', '76', '77', '78', '83')
UNION
-- PIT professional
SELECT DISTINCT a.ScrSSN, /* pos.PITPlaceOfServiceCode, */
	   d.ServiceFromDate as startdate, d.ServiceToDate as enddate, 
	   'PIT_Professional' as ClaimSource,
	   dx.PITDiagnosisCode as ICDCode, 
	   CASE WHEN dx.PITDiagnosisCode like '[EV]%' and ServiceToDate < CAST('2015-10-01' as date) THEN 9
			WHEN dx.PITDiagnosisCodeVersion = 'ICD9' THEN 9
			WHEN dx.PITDiagnosisCodeVersion = 'ICD10' THEN 10
			ELSE NULL END AS ICDType
	FROM Src.CohortCrossWalk as x inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.ScrSSN = x.ScrSSN
								  inner join Src.PIT_PITProfessionalClaim_Archive as b on x.PatientSID = b.PITPatientSID
								  inner join Src.PIT_PITProfessionalClaimDetails_Archive as d on b.PITClaimSID = d.PITClaimSID
								  inner join CDWWork_Archive.NDim.PITPlaceOfService as pos on d.PITPlaceOfServiceSID = pos.PITPlaceOfServiceSID
								  inner join Src.PIT_PITProfessionalClaimDiagnosis_Archive as c on c.PITProfessionalClaimDetailsSID = d.PITProfessionalClaimDetailsSID
								  inner join CDWWork_Archive.NDim.PITDiagnosisCode as dx on c.PITDiagnosisCodeSID = dx.PITDiagnosisCodeSID
	WHERE PITPlaceOfServiceCode in ('02', '05', '06', '07', '08', '11', '15', '17', '19', 
									'20', '22', '23', '24', '26', '49', '50', '52', '53', '71', '72')
UNION
-- FEE domain
SELECT DISTINCT a.ScrSSN, /* b.FeeServiceProvidedSID,
	   b.FeeAuthorizationSID, b.FeeUnauthorizedClaimSID, b.FeeAuthorizationRequestSID,
	   b.FeePurposeOfVisitSID, y.AustinCode, y.FeePurposeOfVisit, */
	   CASE WHEN b.FeeAuthorizationSID <> -1 THEN auth.FromDate
			WHEN b.FeeAuthorizationSID = -1 AND b.FeeUnauthorizedClaimSID <> -1 THEN unauth.TreatmentFromDate
			ELSE NULL END AS startdate,
	   CASE WHEN b.FeeAuthorizationSID <> -1 THEN auth.ToDate
			WHEN b.FeeAuthorizationSID = -1 AND b.FeeUnauthorizedClaimSID <> -1 THEN unauth.TreatmentToDate
			ELSE NULL END AS enddate,
	   'Fee' as ClaimSource,
	   CASE WHEN b.ICD9SID <> -1 THEN icd9.ICD9Code
			WHEN b.ICD10SID <> -1 THEN icd10.ICD10Code
			END AS ICDCode,
	   CASE WHEN b.ICD9SID <> -1 THEN 9
			WHEN b.ICD10SID <> -1 THEN 10
			END AS ICDType
	FROM Src.CohortCrosswalk as x INNER JOIN (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.ScrSSN = x.ScrSSN
								  INNER JOIN Src.Fee_FeeServiceProvided as b on x.PatientSID = b.PatientSID
								  INNER JOIN CDWWork.Dim.FeePurposeOfVisit as y on b.FeePurposeOfVisitSID = y.FeePurposeOfVisitSID
								  INNER JOIN Src.Fee_FeeAuthorization as auth on b.FeeAuthorizationSID = auth.FeeAuthorizationSID
								  INNER JOIN Src.Fee_FeeUnauthorizedClaim as unauth on b.FeeUnauthorizedClaimSID = unauth.FeeUnauthorizedClaimSID
								  INNER JOIN CDWWork.DIM.ICD10 as icd10 on b.ICD10SID = icd10.ICD10SID
								  INNER JOIN CDWWork.Dim.ICD9 as icd9 on b.ICD9SID = icd9.ICD9SID
	WHERE FeePurposeOfVisit in ('OPT - SC 50% OR MORE',
								'OPT - SC LESS THAN 50%',
								'OPT SERVICES/TREATMENT FOR NSC DISABILITIES',
								'OPT TO OBVIATE THE NEED FOR HOSP. ADMISSION',
								'OUTPATIENT 38 U.S.C. 1725',
								'UNAUTHORIZED CLAIM - OUTPATIENT')
-- IVC CDS claims
UNION
		SELECT DISTINCT a.ScrSSN, /* Bill_Type, Claim_Form_Type, b.Place_of_Service_ID, c.ClaimSID,  */
					CAST(COALESCE(Admission_Date, Service_Start_Date) AS DATE) AS startdate,			
					CAST(COALESCE(Discharge_Date, Service_End_Date) AS DATE) AS enddate,
					'IVC CDS' as ClaimSource,
		   CASE WHEN CAST(COALESCE(Discharge_Date, Service_End_Date) AS DATE) < CAST('2015-10-01' as date) AND c.ICD like 'E%' AND LEN(c.ICD) = 4 THEN c.ICD 
				WHEN CAST(COALESCE(Discharge_Date, Service_End_Date) AS DATE) < CAST('2015-10-01' as date) AND c.ICD like 'E%' THEN STUFF(c.ICD, 5, 0, '.')
				WHEN LEN(c.ICD) = 3 THEN c.ICD
				ELSE STUFF(c.ICD, 4, 0, '.') END as ICDCode,
					CASE WHEN CAST(COALESCE(Discharge_Date, Service_End_Date) AS DATE) < CAST('2015-10-01' as date) THEN 9
						 WHEN CAST(COALESCE(Discharge_Date, Service_End_Date) AS DATE) >= CAST('2015-10-01' as date) THEN 10 
						 ELSE NULL END as ICDType
			FROM (SELECT DISTINCT ScrSSN FROM Dflt.Exposure_20241028) as a left join Src.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
																		 left join Src.IVC_CDS_CDS_Claim_Header as b on b.Patient_ICN = x.PatientICN
																		 left join Src.IVC_CDS_CDS_Claim_Diagnosis as c on b.ClaimSID = c.ClaimSID
		WHERE c.ICD IS NOT NULL
		  AND (substring(Bill_Type, 1, 2) in ('13', '71', '73', '76', '77', '78', '83')
			   or Place_of_Service_ID in ('02', '05', '06', '07', '08', '11', '15', '17', '19', 
										  '20', '22', '23', '24', '26', '49', '50', '52', '53', '71', '72'))  


-- Comorbidity from all three domains of Community Care
drop table if exists Dflt.Comorbidites_long_CommunityCare;
with in1 as (
	SELECT a.ScrSSN, condition, enddate as recdate
		FROM Dflt.CommunityCare_inpat_clean as b inner join Dflt.dim_ICD910 as z on b.ICDCode = z.ICDCode AND b.ICDType = z.ICDType
												 inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.ScrSSN = b.ScrSSN
	),
  firstin AS
	 (SELECT scrssn, condition, MIN(recdate) as recdate
	  FROM in1
	  GROUP BY ScrSSN, condition
	  ), 
  ot1 AS
	 (SELECT a.scrssn, condition, startdate as recdate
		FROM Dflt.CommunityCare_outpat_clean as b inner join Dflt.dim_ICD910 as z on b.ICDCode = z.ICDCode AND b.ICDType = z.ICDType
												  inner join (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a on a.scrssn = b.scrssn
	 ), 
  ot2 AS
	 (SELECT scrssn, condition, recdate 
	 ,ROW_NUMBER() OVER (PARTITION BY scrssn, condition ORDER BY scrssn, condition, recdate) AS RN
	 FROM ot1
	 ), 
  secot AS
	 (SELECT scrssn, condition, recdate
	 FROM ot2
	 where RN = 2
	 ), 
  comb AS
	 (SELECT scrssn, condition, recdate
	   FROM firstin
	UNION
	   SELECT scrssn, condition, recdate
	   FROM secot
	 ), 
  comb2 AS
	 (SELECT scrssn, condition, recdate
	  ,ROW_NUMBER() OVER (PARTITION BY scrssn, condition ORDER BY scrssn, condition, recdate) AS RN
	  FROM comb
	  ) 
SELECT scrssn, condition, recdate, 'Community Care' as source
	INTO Dflt.Comorbidites_long_CommunityCare
		FROM comb2
		WHERE rn = 1
GO


/***********************************************************************************************************
++++ Place of service: https://www.cms.gov/medicare/coding-billing/place-of-service-codes/code-sets
-- inpat
	21 Inpatient Hospital
-- Outpat
	02	Telehealth Provided Other than in Patient’s Home
	10	Telehealth Provided in Patient’s Home
	11	Office
	19	Off Campus-Outpatient Hospital
	20	Urgent Care Facility
	22	On Campus-Outpatient Hospital
	23	Emergency Room – Hospital
	24	Ambulatory Surgical Center 

++++ bill type: https://resdac.org/cms-data/variables/bill-type-code
-- inpat
	1[12]x	hospital + inpat/inpat
-- outpat
	[18]3x	hospital/special facility/ASC + outpatient
**********************************************************************************************************/


/* 4.5.)----------------------------------------------------
		Merge from CDW, CMS and Community Care Data
*/

DROP TABLE IF EXISTS Dflt.Comorbidites_long_AllSource;
SELECT ScrSSN, condition, cast(recdate as date) as recdate, source
into Dflt.Comorbidites_long_AllSource
FROM Dflt.Comorbidites_long_VHA
UNION
SELECT * FROM Dflt.Comorbidites_long_medicare
UNION
SELECT * FROM Dflt.Comorbidites_long_medicaid
UNION 
SELECT * FROM Dflt.Comorbidites_long_CommunityCare


-- long to wide
DROP TABLE IF EXISTS #long1;
SELECT  b.ScrSSN, b.startdate,
		a.condition, a.recdate,
		concat(a.condition,'_DT') as condition_dt
INTO #long1
  FROM Dflt.Comorbidites_long_AllSource as a left join Dflt.Exposure as b on a.ScrSSN = b.ScrSSN
  where recdate <= startdate


DROP TABLE IF EXISTS #Comorbidities_dates;
SELECT b.*
INTO #Comorbidities_dates
FROM Dflt.Exposure as a left join (SELECT scrssn, startdate, recdate, condition_dt FROM #long1) as t
PIVOT
	(
	MAX(recdate)
	FOR condition_dt in 
	(CMB_AFibFlutter_DT,
	CMB_ARD_DT,
	CMB_AS_DT,
	CMB_Cardiomyopathy_DT,
	CMB_ChronicMigraine_DT,
	CMB_CKD_DT,
	CMB_COPD_DT,
	CMB_DM_DT,
	CMB_DVT_PE_DT,
	CMB_DysLipid_DT,
	CMB_HF_DT,
	CMB_HIV_DT,
	CMB_HTN_DT,
	CMB_IBD_DT,
	CMB_MDD_DT,
	CMB_OSA_DT,
	CMB_PAD_DT,
	CMB_PsA_DT,
	CMB_RA_DT,
	CMB_SLE_DT,
	CMB_SRD_DT,
	CMB_SSc_DT,
	CMB_TBI_DT,
	CMB_VHD_DT,
	-- History of MACE
	HemoStroke_DT,
	IschemicStroke_DT,
	MI_DT,
	SAH_DT,
	TIA_DT
	 )) as b on a.ScrSSN = b.ScrSSN and a.startdate = b.startdate
where b.scrssn is not null
GO



/* 4.6.)----------------------------------------------------
			TBI screening from health factor
*/
select a.*, healthfactortypesid, healthfactortypeien
into #hf
from CDWWork.dim.HealthFactorType t inner join Dflt.dim_HF_TBI a on t.HealthFactorType = a.FACTOR
GO

DROP TABLE IF EXISTS #TBI_HF;
with t0 as (
	SELECT distinct a.ScrSSN, a.startdate, 1 as TBIScreen
		FROM Src.HF_HealthFactor as h inner join #hf on h.HealthFactorTypeSID = #hf.HealthFactorTypeSID
									  inner join src.CohortCrosswalk as x on h.PatientSID = x.PatientSID
									  inner join Dflt.Exposure as a on a.ScrSSN = x.ScrSSN
		where HealthFactorDateTime <= startdate
		  AND HealthFactorDateTime <= CAST('2024-09-30' AS DATE)),
	t1 as (
	SELECT a.scrssn, a.startdate, min(healthfactordatetime) as TBIhfposdatetime
		   FROM Src.HF_HealthFactor as h inner join #hf on h.HealthFactorTypeSID = #hf.HealthFactorTypeSID
										 inner join src.CohortCrosswalk as x on h.PatientSID = x.PatientSID
										 inner join Dflt.Exposure as a on a.ScrSSN = x.ScrSSN
		where HealthFactorDateTime <= startdate
		  AND HealthFactorDateTime <= CAST('2024-09-30' AS DATE)
		  AND factor = 'TBI-SECTION IV - YES'
		group by a.ScrSSN, a.startdate)
SELECT a.ScrSSN, a.startdate, b.TBIScreen, 
	   CASE WHEN tbihfposdatetime is not null then 1 else 0 end as TBIhfPos,
	   CAST(TBIhfPosDateTime as date) as TBIhfPosDate
INTO #TBI_HF
	FROM Dflt.Exposure as a LEFT JOIN t0 as b on a.ScrSSN = b.ScrSSN AND a.startdate = b.startdate
							LEFT JOIN t1 as c on a.ScrSSN = c.ScrSSN AND a.startdate = c.startdate



/* 5.1.)----------------------------------------------------
		 VA Outpat Prescriptions
*/

-- dim table 
SELECT * 
INTO #dim_rx
  FROM Dflt.dim_rx
  WHERE RX_cat in ('RX_alpha_blocker', 'RX_betaBlocker', 'RX_CCB', 'RX_Central_sympathetic', 'RX_Diuretic_potassium',
				   'RX_Diuretic_TZD', 'RX_Diuretics_loop', 'RX_Nitrates', 'RX_other_ACEIs_ARBs', 'RX_Renin_blocker', 'RX_Vasodiator',
				   'RX_Anticoagulant_DOAC', 'RX_Anticoagulant_VKA', 'RX_aPLT_Aspirin', 'RX_aPLT_other', 'RX_SGLT2', 'RX_GLP1',
				   'RX_HRT', 'RX_lipid_bileacid', 'RX_lipid_fibrate', 'RX_lipid_other', 'RX_lipid_statin', 'RX_Mgr_aCGRP_acute',
				   'RX_Mgr_clonidine', 'RX_Mgr_ergotamine', 'RX_Mgr_neurotoxin', 'RX_Mgr_NMDA', 'RX_Mgr_otherAnticonvulsant',
				   'RX_Mgr_SNRI', 'RX_Mgr_TCA', 'RX_Mgr_triptan', 'RX_NSAID', 'RX_aPsyc_atpc', 'RX_aPsyc_tpc')

DROP TABLE IF EXISTS #rx0;
select a.ScrSSN, y.RX_Cat,
		y.DrugNameWithoutDose, y.DrugNameWithDose, z.StrengthNumeric, b.QtyNumeric, b.DaysSupply, b.ReleaseDateTime,
		cast(b.ReleaseDateTime as date) as startdate,
		cast(DATEADD(day, DaysSupply, ReleaseDateTime) as date) as enddate
into #rx0
	FROM (SELECT DISTINCT ScrSSN from Dflt.Exposure) as a inner join Src.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
														  inner join Src.RxOut_RxOutpatFill as b on x.PatientSID = b.PatientSID
														  inner join #dim_rx as y on y.NationalDrugSID = b.NationalDrugSID
														  inner join CDWWork.Dim.NationalDrug as z on y.NationalDrugSID = z.NationalDrugSID
WHERE ReleaseDateTime IS NOT NULL
  AND ReleaseDateTime <= CAST('2024-09-30' AS DATE)
  AND DaysSupply IS NOT NULL
  AND b.ReleaseDateTime >= CAST('2005-1-1' as date)	
GO


DROP TABLE IF EXISTS #RX_BL;
SELECT a.ScrSSN, a.StartDate,
		b.Rx_cat, b.startdate as rxstartdate, b.enddate as rxenddate
INTO #RX_BL
	FROM Dflt.Exposure as a left join #rx0 as b on a.ScrSSN = b.ScrSSN
WHERE a.StartDate > b.startdate 
  AND a.StartDate < b.enddate


DROP TABLE IF EXISTS #RX_dates;
with bl0 as (
SELECT  distinct *,
		concat(RX_Cat,'_DT') as RX_Cat_dt
  FROM #RX_BL
)
SELECT DISTINCT b.*
INTO #RX_dates
FROM Dflt.Exposure as a left join (SELECT scrssn, startdate, rxstartdate, RX_Cat_dt FROM bl0) as t
PIVOT
(
	MIN(rxstartdate)
	FOR RX_Cat_dt in 
	(RX_alpha_blocker_DT, 
	RX_betaBlocker_DT, 
	RX_CCB_DT, 
	RX_Central_sympathetic_DT, 
	RX_Diuretic_potassium_DT,
	RX_Diuretic_TZD_DT, 
	RX_Diuretics_loop_DT,
	RX_Nitrates_DT,
	RX_other_ACEIs_ARBs_DT,
	RX_Renin_blocker_DT,
	RX_Vasodiator_DT,
	RX_Anticoagulant_DOAC_DT, 
	RX_Anticoagulant_VKA_DT, 
	RX_aPLT_Aspirin_DT, 
	RX_aPLT_other_DT, 
	RX_SGLT2_DT, 
	RX_GLP1_DT,
	RX_HRT_DT,
	RX_lipid_bileacid_DT,
	RX_lipid_fibrate_DT,
	RX_lipid_other_DT,
	RX_lipid_statin_DT,
	RX_Mgr_aCGRP_acute_DT,
	RX_Mgr_clonidine_DT, 
	RX_Mgr_ergotamine_DT,
	RX_Mgr_neurotoxin_DT, 
	RX_Mgr_NMDA_DT, 
	RX_Mgr_otherAnticonvulsant_DT,
	RX_Mgr_SNRI_DT, 
	RX_Mgr_TCA_DT, 
	RX_Mgr_triptan_DT, 
	RX_NSAID_DT, 
	RX_aPsyc_atpc_DT,
	RX_aPsyc_tpc_DT)) as b on a.ScrSSN = b.ScrSSN and a.startdate = b.startdate
where b.scrssn is not null
GO



/* 5.2.)----------------------------------------------------
			roll-over rx for time-updated cov
*/

DROP TABLE IF EXISTS #Prescriptions;
  CREATE TABLE #Prescriptions
(
  prescriptionid INT  NOT NULL IDENTITY,
  patientid      varchar(9)  NOT NULL,
  drugid     varchar(100)  NOT NULL,
  startdate      DATE NOT NULL,
  numdays    INT  NOT NULL,
  enddate AS DATEADD(day, numdays, startdate),
  CONSTRAINT CHK_Prescriptions_ed_sd CHECK(numdays > 0)
);

CREATE UNIQUE CLUSTERED INDEX idx_start
  ON #Prescriptions
   (patientid, drugid, startdate, prescriptionid);

ALTER TABLE #Prescriptions
  ADD CONSTRAINT PK_Prescriptions PRIMARY KEY
    NONCLUSTERED(prescriptionid);

INSERT INTO #Prescriptions
  (patientid, drugid, startdate, numdays) 
select scrssn, RX_Cat, startdate, DaysSupply
from #rx0
where  DaysSupply > 0		-- days supply must > 0
go

DROP TABLE IF EXISTS Dflt.RX_all;
SET NOCOUNT ON;
DECLARE @Result AS TABLE
(
  patientid varchar(9)  NOT NULL,
  drugid    varchar(100)  NOT NULL,
  startdate DATE NOT NULL,
  enddate   DATE NOT NULL,
  daystotal INT NOT NULL
);

DECLARE
  @patientid      AS varchar(9),
  @drugid     AS varchar(100),
  @startdate      AS DATE,
  @numdays    AS INT,
  @sumnumdays     AS INT,
  @prevpatientid  AS varchar(9),
  @prevdrugid     AS varchar(100),
  @prevnumdays AS INT,
  @daystot as INT,
  @firststartdate AS DATE;

DECLARE C CURSOR FAST_FORWARD FOR
  SELECT patientid, drugid, startdate, numdays
  FROM #Prescriptions
  ORDER BY patientid, drugid, startdate, prescriptionid;

OPEN C;

FETCH NEXT FROM C INTO
  @patientid, @drugid, @startdate, @numdays;

SELECT
  @prevpatientid  = @patientid,
  @prevdrugid     = @drugid,
  @prevnumdays    = @numdays,
  @firststartdate = @startdate,
  @daystot        = 0,
  @sumnumdays     = 0;

WHILE @@fetch_status = 0
BEGIN

  IF    @prevpatientid <> @patientid
     OR @prevdrugid    <> @drugid
     OR DATEADD(day, @sumnumdays+1, @firststartdate)	-- no gap (use 1 day)
      <   @startdate
 BEGIN --begin loop to end this run
   INSERT INTO @Result(patientid, drugid, startdate, enddate, daystotal)
   VALUES(@prevpatientid, @prevdrugid, @firststartdate,
      DATEADD(day, @sumnumdays, @firststartdate), @daystot);

  SELECT  --create new startdate
    @firststartdate = @startdate,
    @sumnumdays     = 0,
	@daystot        = 0;
 END --end loop to end this run


 --
 IF @prevpatientid = @prevpatientid
   AND @prevdrugid = @drugid
   AND DATEADD(day, @sumnumdays, @firststartdate)
      < @startdate
	AND DATEADD(day, @sumnumdays + 1, @firststartdate)		-- no gap (use 1 day)
	  >= @startdate

   BEGIN
      SELECT 
	    @sumnumdays += DATEDIFF(day, DATEADD(day, @sumnumdays, @firststartdate), @startdate)
   END

  SELECT
    @sumnumdays    += @numdays,
	@daystot       += @numdays,
    @prevpatientid  = @patientid,
    @prevdrugid     = @drugid,
	@prevnumdays    = @numdays;

  FETCH NEXT FROM C INTO
    @patientid, @drugid, @startdate, @numdays;
END

IF @sumnumdays > 0
  INSERT INTO @Result(patientid, drugid, startdate, enddate, daystotal)
    VALUES(@prevpatientid, @prevdrugid, @firststartdate,
  DATEADD(day, @sumnumdays, @firststartdate), @daystot);

CLOSE C;

DEALLOCATE C;
SET NOCOUNT OFF;

SELECT patientid as ScrSSN,
	   drugid as RX_cat,
	   startdate as rx_startdate, enddate as rx_enddate, daystotal 
into Dflt.RX_all
FROM @Result;

/* 5.3.)----------------------------------------------------
			neurotoxin from outpat CPT codes
*/

DROP TABLE IF EXISTS #neurotoxin_CPT_all;
SELECT a.ScrSSN, b.visitdatetime as CPT_startdate, 
	   DATEADD(DAY, 90, visitdatetime) as CPT_enddate	-- every 3 months
INTO #neurotoxin_CPT_all
	FROM (SELECT DISTINCT ScrSSN from Dflt.Exposure) as a inner join src.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
														  inner join Src.Outpat_VProcedure as b on x.PatientSID = b.PatientSID
														  inner join CDWWork.Dim.CPT as c on b.CPTSID = c.CPTSID
	WHERE VisitDateTime is not null
	  AND VisitDateTime <= CAST('2024-09-30' AS DATE)
	  AND CPTCode in ('64615', 'J0585', 'J0586', 'J0587', 'J0588')

drop table if exists #neurotoxin_CPT_BL;
with bl0 as (
	select a.ScrSSN, a.startdate, cast(CPT_startdate as date) as CPT_startdate, cast(CPT_enddate as date) as CPT_enddate
		from Dflt.Exposure as a inner join #neurotoxin_CPT_all as b on a.ScrSSN = b.ScrSSN
		 WHERE StartDate > CPT_startdate AND StartDate < CPT_enddate)
SELECT DISTINCT *
INTO #neurotoxin_CPT_BL
FROM bl0

-- add to the big rx table
INSERT INTO Dflt.RX_all
SELECT ScrSSN, 'CPT_Neurotoxin' as RX_cat, 
		cast(CPT_startdate as date) as rx_startdate,
		cast(CPT_enddate as date) as rx_enddate,
		90 as daystotal
FROM #neurotoxin_CPT_all


/* 6.1.)----------------------------------------------------
			headache related healthcare utilization
*/

-- headache ICDs
SELECT *
INTO #dim_headache_icd
from PCS_HCOE.codes.ICD_HCOE_CODES
where comnum in (1106, 1114, 1115, 1116, 1117, 1118, 1119, 1120)	-- Sico el al., 2022


SELECT DISTINCT ICD9SID, ICD9Code, codename as condition
INTO #Dim_ICD9_HA
	FROM CDWWork.Dim.ICD9 as x inner join #dim_headache_icd as a on x.ICD9Code = a.ICDCode
WHERE dx_codetype = 9 
GO

SELECT DISTINCT ICD10SID, ICD10Code, codename as condition
INTO #Dim_ICD10_HA
	FROM CDWWork.Dim.ICD10 as x inner join #dim_headache_icd as a on x.ICD10Code = a.ICDCode
WHERE dx_codetype = 10
GO

-- get all outpat encounters with headache dx
DROP TABLE IF EXISTS #HAVisits;
WITH a0 as (
SELECT a.ScrSSN, b.VisitDateTime, b.visitSID, ICD9Code as ICDcode,
	   y.StopCode as PrimaryStopCode, y.StopCodeName as PrimaryStopCodeName,
	   ServiceCategory, b.WorkloadLogicFlag, NoncountClinicFlag
	FROM (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a inner join Src.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
														  inner join Src.Outpat_Visit as b on x.PatientSID = b.PatientSID
														  inner join CDWWork.Dim.StopCode as y on b.PrimaryStopCodeSID = y.StopCodeSID
														  inner join Src.Outpat_VDiagnosis as dx on b.VisitSID = dx.VisitSID
														  inner join #Dim_ICD9_HA as xx on dx.ICD9SID = xx.ICD9SID
	WHERE b.VisitDateTime >= CAST('2005-01-01' AS date)
	  AND b.VisitDateTime <= CAST ('2024-09-30' as date)
	  AND b.WorkloadLogicFlag = 'Y'
	  AND NoncountClinicFlag = 'N'
	  AND b.VisitDateTime IS NOT NULL
UNION
SELECT a.ScrSSN, b.VisitDateTime, b.visitSID, ICD10Code as ICDcode,
	   y.StopCode as PrimaryStopCode, y.StopCodeName as PrimaryStopCodeName,
	   ServiceCategory, b.WorkloadLogicFlag, NoncountClinicFlag
	FROM (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a inner join Src.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
														  inner join Src.Outpat_Visit as b on x.PatientSID = b.PatientSID
														  inner join CDWWork.Dim.StopCode as y on b.PrimaryStopCodeSID = y.StopCodeSID
														  inner join Src.Outpat_VDiagnosis as dx on b.VisitSID = dx.VisitSID
														  inner join #Dim_ICD10_HA as xx on dx.ICD10SID = xx.ICD10SID
	WHERE b.VisitDateTime >= CAST('2005-01-01' AS date)
	  AND b.VisitDateTime <= CAST ('2024-09-30' as date)
	  AND b.WorkloadLogicFlag = 'Y'
	  AND NoncountClinicFlag = 'N'
	  AND b.VisitDateTime IS NOT NULL
	  ),
	a1 as (
	SELECT	a.*,
			b.ProviderTypeSID, b.PrimarySecondary,
			c.ProviderType, c.Classification ,
		CASE WHEN ProviderType IN ('Allopathic & Osteopathic Physicians') AND Classification NOT IN ('Resident, Allopathic (includes Interns, Residents, Fellows)', 'Resident, Osteopathic (includes Interns, Residents, Fellows)') THEN 1		--Physician 
			 WHEN ProviderType IN ('Physicians (M.D. and D.O.)') AND Classification IN ('Physician/Osteopath', 'Surgery') THEN 1		-- excluded resident and fellow
			 WHEN ProviderType IN ('Nursing Service') AND Classification IN ('Nurse Practitioner') THEN 2	-- NP
			 WHEN ProviderType IN ('Physician Assistants & Advanced Practice Nursing Providers') AND Classification IN ('Nurse Practitioner') THEN 2
			 WHEN ProviderType IN ('Physician Assistants & Advanced Practice Nursing Providers') AND Classification IN ('Physician Assistant') THEN 3	-- PA
			 ELSE 9		-- OTHER
		END AS ProviderCategory
	  FROM a0 as a LEFT JOIN Src.Outpat_VProvider as b on a.VisitSID = b.VisitSID
						 LEFT JOIN CDWWork.Dim.ProviderType as c on b.ProviderTypeSID = c.ProviderTypeSID
		WHERE b.PrimarySecondary = 'P'
			AND b.WorkloadLogicFlag = 'Y')
SELECT DISTINCT * 
INTO #HAVisits
FROM a1
WHERE ProviderCategory in (1, 2, 3)


-- define specialty using stopcodes
DROP TABLE IF EXISTS Dflt.HAEncounters;
with a0 as (
SELECT DISTINCT ScrSSN, cast(VisitDateTime as date) as VisitDate, PrimaryStopCode, PrimaryStopCodeName,
		CASE WHEN PrimaryStopCode in (130, 131) THEN 1 ELSE 0 END AS HA_ED_flag,
		CASE WHEN PrimaryStopCode in (315, 325) THEN 1 ELSE 0 END AS HA_Neurology_flag,
		CASE WHEN PrimaryStopCode in (170, 172, 178,
									  301, 318, 319, 322, 323, 324, 326, 348, 338, 350) THEN 1 ELSE 0 END AS HA_PC_flag
FROM #HAVisits
WHERE ProviderCategory in (1, 2, 3) 
  )
SELECT ScrSSN, VisitDate,
	   CASE WHEN sum(HA_ED_flag) > 0 THEN 1 ELSE 0 END AS HA_ED_flag,
	   CASE WHEN sum(HA_Neurology_flag) > 0 THEN 1 ELSE 0 END AS HA_Neurology_flag,
	   CASE WHEN sum(HA_PC_flag) > 0 THEN 1 ELSE 0 END AS HA_PC_flag
INTO Dflt.HAEncounters
FROM a0
GROUP BY ScrSSN, VisitDate
GO

/* 6.2.)----------------------------------------------------
			Any primary care visits
*/

WITH a0 as (
SELECT a.ScrSSN, b.VisitDateTime, b.visitSID,
	   y.StopCode as PrimaryStopCode, y.StopCodeName as PrimaryStopCodeName,
	   ServiceCategory, WorkloadLogicFlag, NoncountClinicFlag
	FROM (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a left join Src.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
														  left join Src.Outpat_Visit as b on x.PatientSID = b.PatientSID
														  left join CDWWork.Dim.StopCode as y on b.PrimaryStopCodeSID = y.StopCodeSID
	WHERE VisitDateTime >= CAST('2005-01-01' AS date)
	  AND VisitDateTime <= CAST ('2024-09-30' as date)
	  AND WorkloadLogicFlag = 'Y'
	  AND NoncountClinicFlag = 'N'
	  AND VisitDateTime IS NOT NULL),
	a1 as (
	SELECT	a.*,
			b.ProviderTypeSID, b.PrimarySecondary,
			c.ProviderType, c.Classification ,
		CASE WHEN ProviderType IN ('Allopathic & Osteopathic Physicians') AND Classification NOT IN ('Resident, Allopathic (includes Interns, Residents, Fellows)', 'Resident, Osteopathic (includes Interns, Residents, Fellows)') THEN 1		--Physician 
			 WHEN ProviderType IN ('Physicians (M.D. and D.O.)') AND Classification IN ('Physician/Osteopath', 'Surgery') THEN 1		-- excluded resident and fellow
			 WHEN ProviderType IN ('Nursing Service') AND Classification IN ('Nurse Practitioner') THEN 2	-- NP
			 WHEN ProviderType IN ('Physician Assistants & Advanced Practice Nursing Providers') AND Classification IN ('Nurse Practitioner') THEN 2
			 WHEN ProviderType IN ('Physician Assistants & Advanced Practice Nursing Providers') AND Classification IN ('Physician Assistant') THEN 3	-- PA
			 ELSE 9		-- OTHER
		END AS ProviderCategory
	  FROM a0 as a LEFT JOIN Src.Outpat_VProvider as b on a.VisitSID = b.VisitSID
						 LEFT JOIN CDWWork.Dim.ProviderType as c on b.ProviderTypeSID = c.ProviderTypeSID
		WHERE b.PrimarySecondary = 'P'
			AND b.WorkloadLogicFlag = 'Y'
			AND PrimaryStopCode in (170, 172, 178, 301, 318, 319, 322, 323, 324, 326, 348, 338, 350)	-- stopcode for primary care
	),
	a2 as (
	SELECT distinct a.scrssn, a.VisitSID, a.VisitDateTime, 
				PrimaryStopCode, PrimaryStopCodeName, 
				ServiceCategory,
					ProviderCategory, CPTCode,
					CASE WHEN CPTCode in ('99201', '99202', '99203', '99204', '99205', 
					  '99211', '99212', '99213', '99214', '99215',										/* office visit */
					  '99241', '99242', '99243', '99244', '99245',										/* office consultation */
					  '99385', '99386', '99387', '99395', '99396', '99397',								/* Prev visit */
					  '99401', '99402', '99403', '99404',												/* Prev consult */
					  '99371', '99372', '99373', 													    /* phone consult before 1/1/2008 */
					  '99441', '99442', '99443', '99444',												/* phone/online E/M */
					  '99421', '99422', '99423') THEN 1 ELSE 0 END AS EM_flag
					FROM a1 AS a LEFT JOIN CDWWork.Outpat.VProcedure as d on a.VisitSID = d.VisitSID
										   LEFT JOIN CDWWork.Dim.CPT as x on d.CPTSID = x.CPTSID
						WHERE ProviderCategory in (1, 2, 3))
SELECT * 
INTO #allPCEncounters
FROM a2
WHERE EM_flag = 1			-- use CPT code to find evaluation and management appt


SELECT DISTINCT ScrSSN, CAST(Visitdatetime as date) as VisitDate,
				1 as PCEncounter_flag
INTO Dflt.PCEncounters
FROM #allPCEncounters

/* 6.3.)----------------------------------------------------
			Any primary care visits
*/

DROP TABLE IF EXISTS Dflt.FluShot;
with a0 as (
	SELECT a.ScrSSN, CAST(b.VisitDateTime as date) as FluVaccinationDate, c.CVXCode, c.ImmunizationName
		FROM (SELECT DISTINCT ScrSSN FROM Dflt.Exposure) as a LEFT JOIN Src.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
															  LEFT JOIN CDWWork.Immun.Immunization as b on x.PatientSID = b.PatientSID
															  LEFT JOIN CDWWork.Dim.ImmunizationName as c on b.ImmunizationNameSID = c.ImmunizationNameSID
	WHERE b.VisitDateTime >= CAST('2005-01-01' as date)
	  AND b.VisitDateTime <=  CAST('2024-09-30' as date)
	  AND CVXCode in ('15', '16', '88', '111', '125', '126', '127', '128', 
					  '135', '140', '141', '144', '149', '150', '151', '153', '155', '158',
					  '160', '161', '166', '168', '171', '185', '186', '194', '197',
					  '200', '201', '202', '205', '231', '320'))		-- double check for Haemophilus 'influenzae' type B (Hib) vaccine!
SELECT *, 1 as flushot_flag
INTO Dflt.FluShot
FROM a0
GO

/* 6.4.)----------------------------------------------------
			combine all utilization data
*/

DROP TABLE IF EXISTS #utilization;
with ED0 as (
	SELECT a.ScrSSN, a.startdate, b.HA_ED_flag
		FROM Dflt.Exposure as a inner join Dflt.HAEncounters as b on a.ScrSSN = b.ScrSSN
		WHERE HA_ED_flag = 1 
		  AND VisitDate >= DATEADD(YEAR, -1, startdate)		-- within 1 year
		  AND VisitDate < startdate),
	ED1 as (
	SELECT scrssn, startdate, sum(HA_ED_flag) as HAEDVisit_1y
		FROM ED0
		GROUP BY ScrSSN, startdate),
	neuro0 as (
	SELECT a.ScrSSN, a.startdate, b.HA_Neurology_flag
		FROM Dflt.Exposure as a inner join Dflt.HAEncounters as b on a.ScrSSN = b.ScrSSN
		WHERE HA_Neurology_flag = 1 
		  AND VisitDate >= DATEADD(YEAR, -1, startdate)	  -- within 1 year
		  AND VisitDate < startdate),
	neuro1 as (
	SELECT ScrSSN, startdate, sum(HA_Neurology_flag) as HANeuroVisit_1y
		FROM neuro0
		GROUP BY ScrSSN, startdate),
	PC0 as (
	SELECT a.ScrSSN, a.startdate, b.PCEncounter_flag
		FROM Dflt.Exposure as a inner join Dflt.PCEncounters as b on a.ScrSSN = b.ScrSSN
		WHERE VisitDate >= DATEADD(YEAR, -1, startdate)		-- within 1 year
		  AND VisitDate < startdate),
	PC1 as (
	SELECT ScrSSN, startdate, sum(PCEncounter_flag) as AnyPCVisit_1y
		FROM PC0
		GROUP BY ScrSSN, startdate),
	flu0 as (
	SELECT a.ScrSSN, a.startdate, b.flushot_flag
		FROM Dflt.Exposure as a inner join dflt.FluShot as b on a.ScrSSN = b.ScrSSN
		WHERE FluVaccinationDate >= DATEADD(YEAR, -1, startdate)	-- within 1 year
		 AND FluVaccinationDate < startdate),
	flu1 as (
	SELECT ScrSSN, startdate, sum(flushot_flag) as flushot_1y
		FROM flu0
		GROUP BY ScrSSN, startdate)
SELECT a.ScrSSN, a.startdate,
		CASE WHEN b.HAEDVisit_1y is not NULL THEN b.HAEDVisit_1y ELSE 0 END AS HAEDVisit_1y,
		CASE WHEN c.HANeuroVisit_1y is not NULL THEN c.HANeuroVisit_1y ELSE 0 END AS HANeuroVisit_1y,
		CASE WHEN d.AnyPCVisit_1y is not NULL THEN d.AnyPCVisit_1y ELSE 0 END AS AnyPCVisit_1y,
		CASE WHEN e.flushot_1y is not NULL THEN 'Yes' ELSE 'No' END AS flushot_1y
INTO #utilization
	FROM Dflt.Exposure as a left join ED1 as b on a.ScrSSN = b.ScrSSN AND a.startdate = b.startdate
							left join neuro1 as c on a.ScrSSN = c.ScrSSN AND a.startdate = c.startdate
							left join PC1 as d on a.ScrSSN = d.ScrSSN AND a.startdate = d.startdate
							left join flu1 as e on a.ScrSSN = e.ScrSSN AND a.startdate = e.startdate


/* 7.)----------------------------------------------------
			LSP and aCGRP as covariables
			co-rx or rx within 90 days
*/


DROP TABLE IF EXISTS #aCGRP_BL;
SELECT distinct a.ScrSSN, a.startdate, a.enddate, a.arm, b.startdate as Rx_startdate, b.enddate as Rx_enddate,
		case when b.ScrSSN is not null THEN 1 ELSE 0 END AS RX_aCGRP_BL
INTO #aCGRP_BL
FROM Dflt.Exposure as a left join dflt.aCGRP_PersonTrials as b on a.ScrSSN = b.ScrSSn
WHERE a.StartDate >= b.startdate 
  AND a.StartDate <= dateadd(month, 3, b.enddate)
  and a.Arm <> 'aCGRP'


DROP TABLE IF EXISTS #LSP_BL;
SELECT DISTINCT a.ScrSSN, a.startdate, a.enddate, a.arm, b.startdate as Rx_startdate, b.enddate as Rx_enddate,
	case when b.ScrSSN is not null THEN 1 ELSE 0 END AS RX_LSP_BL
INTO #LSP_BL
FROM Dflt.Exposure as a left join dflt.LSP_PersonTrials as b on a.ScrSSN = b.ScrSSn
WHERE a.StartDate >= b.startdate 
  AND a.StartDate <= dateadd(month, 3, b.enddate)
  AND a.Arm <> 'LSP/CAN'

DROP TABLE IF EXISTS #TPM_BL;
SELECT DISTINCT a.ScrSSN, a.Arm, a.startdate, a.enddate, b.startdate as Rx_startdate, b.enddate as Rx_enddate,
	case when b.ScrSSN is not null THEN 1 ELSE 0 END AS RX_TPM_BL
INTO #TPM_BL
FROM Dflt.Exposure as a left join dflt.TPM_PersonTrials as b on a.ScrSSN = b.ScrSSn
WHERE a.StartDate >= b.startdate 
  AND a.StartDate <= dateadd(month, 3, b.enddate)
  AND a.Arm <> 'TPM'


/* 8.)----------------------------------------------------
		create BL table
*/

DROP TABLE IF EXISTS Dflt.BLData;
SELECT DISTINCT a.*, Gender, SelfIdentifiedGender, Race, Ethnicity, MaritalStatus, Eligibility, ServiceConnectedFlag,
				URH as GISURH, ADI_NATRANK, m.VISN, SMKHFCOM,
				Weight_BL, WeightDT as Weight_BLDT, HEIGHT_BL, 
				Systolic as SBP_BL, diastolic as DBP_BL,
				CAN_score as CAN_score_BL,
				AnyPCVisit_1y, flushot_1y,
				TBIScreen, TBIhfPos, TBIhfPosDate,
				CMB_AFibFlutter_DT, CMB_ARD_DT, CMB_AS_DT, CMB_Cardiomyopathy_DT, CMB_ChronicMigraine_DT, CMB_CKD_DT, CMB_COPD_DT,
				CMB_DM_DT, CMB_DVT_PE_DT, CMB_DysLipid_DT, CMB_HF_DT, CMB_HIV_DT, CMB_HTN_DT, CMB_IBD_DT, CMB_MDD_DT, CMB_OSA_DT,
				CMB_PAD_DT, CMB_PsA_DT, CMB_RA_DT, CMB_SLE_DT, CMB_SRD_DT, CMB_SSc_DT, CMB_TBI_DT, CMB_VHD_DT, 
				HemoStroke_DT as Hx_HemoStroke_DT, IschemicStroke_DT as Hx_IschemicStroke_DT,
				MI_DT as Hx_MI_DT, SAH_DT as Hx_SAH_DT, TIA_DT as Hx_TIA_DT,
				RX_alpha_blocker_DT, RX_betaBlocker_DT,  RX_CCB_DT,  RX_Central_sympathetic_DT,  RX_Diuretic_potassium_DT,
				RX_Diuretic_TZD_DT, RX_Diuretics_loop_DT, RX_Nitrates_DT, RX_other_ACEIs_ARBs_DT, RX_Renin_blocker_DT,
				RX_Vasodiator_DT, RX_Anticoagulant_DOAC_DT,  RX_Anticoagulant_VKA_DT, RX_aPLT_Aspirin_DT, RX_aPLT_other_DT, 
				RX_SGLT2_DT, RX_GLP1_DT, RX_HRT_DT, RX_lipid_bileacid_DT, RX_lipid_fibrate_DT, RX_lipid_other_DT, 
				RX_lipid_statin_DT, RX_aPsyc_atpc_DT, RX_aPsyc_tpc_DT,
				HAEDVisit_1y, HANeuroVisit_1y,
				RX_Mgr_aCGRP_acute_DT, RX_Mgr_clonidine_DT, RX_Mgr_ergotamine_DT, RX_Mgr_neurotoxin_DT, CPT_Mgr_neurotoxin_DT,
				RX_Mgr_NMDA_DT, RX_Mgr_otherAnticonvulsant_DT, RX_Mgr_SNRI_DT, RX_Mgr_TCA_DT, RX_Mgr_triptan_DT, RX_NSAID_DT,
				RX_aCGRP_BL, RX_LSP_BL, RX_TPM_BL -- as cov to check co-rx
INTO Dflt.BLData
FROM Dflt.Exposure as a left join #demoFY23 as b on a.ScrSSN = b.ScrSSN
						left join #GIS as c on a.ScrSSN = c.ScrSSN AND a.startdate = c.startdate
						left join #ADI3 as d on a.ScrSSN = d.ScrSSN AND a.startdate = d.startdate
						left join #smk as e on a.ScrSSN = e.ScrSSN AND a.startdate = e.startdate
						left join #weightBL as f on a.ScrSSN = f.ScrSSN AND a.startdate = f.startdate
						left join #heightBL as g on a.ScrSSN = g.ScrSSN AND a.startdate = g.startdate
						left join (select distinct * from #Comorbidities_dates) as i on a.ScrSSN = i.ScrSSN AND a.startdate = i.startdate
						left join (select distinct * from #RX_dates) as j on a.ScrSSN = j.ScrSSN AND a.startdate = j.startdate
						left join #BP_BL as k on a.ScrSSN = k.ScrSSN AND a.startdate = k.startdate
						left join #can as l on a.ScrSSN = l.ScrSSN AND a.startdate = l.startdate
						left join (SELECT DISTINCT sta3n, VISNFY17 as VISN from CDWWork.Dim.Sta3n) as m on a.sta3n = m.sta3n
						left join #TBI_HF as n on a.ScrSSN = n.ScrSSN and a.startdate = n.startdate
						left join #neurotoxin_CPT_BL as o on a.ScrSSN = o.ScrSSN and a.startdate = o.startdate
						left join #utilization as p on a.ScrSSN = p.ScrSSN and a.startdate = p.startdate
						left join #aCGRP_BL as xx on a.ScrSSN = xx.ScrSSN AND a.startdate = xx.startdate
						left join #LSP_BL as yy on a.ScrSSN = yy.ScrSSN AND a.startdate = yy.startdate
						left join #TPM_BL as zz on a.ScrSSN = zz.ScrSSN AND a.startdate = zz.startdate
