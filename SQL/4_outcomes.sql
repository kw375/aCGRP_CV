
/* 1.)-------------------------------------------------------------------------
		Death
*/

DROP TABLE IF EXISTS #DeathDates_all;

DECLARE @lastDAFdate AS datetime2(0);
SET @lastDAFdate = (SELECT MAX(MPI_DOD) FROM Src.SDAF_DAFMaster);

WITH DEATH AS (
	SELECT COH.ScrSSN,
		   CAST(
				COALESCE(
						DAF.MPI_DOD,
						DAF.Missing_DOD,
						MVI.DeathDateTime,
						PAT.DeathDateTime) AS DATE) AS DeathDate,
			CASE WHEN DAF.MPI_DOD IS NOT NULL THEN 'DAF.MPI_DOD'
				 WHEN DAF.Missing_DOD IS NOT NULL THEN 'DAF.Missing_DOD'
				 WHEN MVI.DeathDateTime IS NOT NULL THEN 'MVI.DeathDateTime'
				 WHEN PAT.DeathDateTime IS NOT NULL THEN 'PAT.DeathDateTime'
			END AS DeathDateSource
		FROM Src.CohortCrosswalk as COH LEFT JOIN Src.SDAF_DAFMaster AS DAF ON COH.PatientICN = DAF.MPI_ICN
										LEFT JOIN CDWWork.SVeteran.SMVIPerson as MVI on COH.PatientICN = MVI.MVIPersonICN AND MVI.DeathDateTime > @lastDAFdate
										LEFT JOIN Src.Patient_Patient AS PAT on COH.PatientICN = PAT.PatientICN AND PAT.DeathDateTime > @lastDAFdate
										)
SELECT DISTINCT ScrSSN, DeathDate
INTO #DeathDates_all
FROM DEATH
WHERE DeathDate IS NOT NULL



/* 2.1.)-------------------------------------------------------------------------
		  MACE from VHA CDW
			inpatient discharge dx at 1st, 2nd or 3rd position
*/

-- get all SIDs
DROP TABLE IF EXISTS #Dim_ICD9_event;
SELECT DISTINCT ICD9SID, ICD9Code, condition
INTO #Dim_ICD9_MACE
	FROM CDWWork.Dim.ICD9 as x inner join Dflt.dim_ICD910 as a on x.ICD9Code = a.ICDCode
WHERE ICDtype = 9 
  AND condition in ('MI', 'IschemicStroke', 'HemoStroke', 'SAH')
GO

DROP TABLE IF EXISTS #Dim_ICD10_event;
SELECT DISTINCT ICD10SID, ICD10Code, condition
INTO #Dim_ICD10_MACE
	FROM CDWWork.Dim.ICD10 as x inner join Dflt.dim_ICD910 as a on x.ICD10Code = a.ICDCode
WHERE ICDtype = 10
  AND condition in ('MI', 'IschemicStroke', 'HemoStroke', 'SAH')
GO


DROP TABLE IF EXISTS #outcome_VHA;
select a.scrssn, b.InpatientSID, 
		b.AdmitDateTime, b.dischargedatetime,
		y.ICD9Code as ICDCode, c.OrdinalNumber, y.condition
into #outcome_VHA
 from (SELECT DISTINCT ScrSSN from Dflt.BLData) as a 
							inner join Src.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
							inner join Src.Inpat_Inpatient as b on x.PatientSID = b.PatientSID
							inner join Src.Inpat_InpatDischargeDiagnosis as c on b.InpatientSID = c.InpatientSID
							inner join #Dim_ICD9_MACE as y on c.ICD9SID = y.ICD9SID
    where b.AdmitDateTime IS NOT NULL
	  AND b.dischargedatetime IS NOT NULL
	  AND OrdinalNumber in (1, 2, 3)
 UNION
select a.scrssn, b.InpatientSID,
		b.AdmitDateTime, b.dischargedatetime,
		y.ICD10Code as ICDCode, c.OrdinalNumber, y.condition
 from (SELECT DISTINCT ScrSSN from Dflt.BLData) as a 
							inner join Src.CohortCrossWalk as x on a.ScrSSN = x.ScrSSN
							inner join Src.Inpat_Inpatient as b on x.PatientSID = b.PatientSID
							inner join Src.Inpat_InpatDischargeDiagnosis as c on b.InpatientSID = c.InpatientSID
							inner join #Dim_ICD10_MACE as y on c.ICD10SID = y.ICD10SID
    where b.AdmitDateTime IS NOT NULL
	  AND b.dischargedatetime IS NOT NULL
	  AND OrdinalNumber in (1, 2, 3)

/* 2.2.)-------------------------------------------------------------------------
		  MACE from CMS
			inpatient discharge dx at 1st, 2nd or 3rd position
*/

DROP TABLE IF EXISTS #outcome_CMS;
with comb as (
	SELECT a.ScrSSN, From_DT as AdmitDate,
		   THRU_DT as DischargeDate, 
		   line, 
		   CASE WHEN THRU_DT < CAST('2015-10-01' as date) THEN 9 ELSE 10 END AS ICDType,
		   CASE WHEN THRU_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' AND LEN(ICD_nodecimal) = 4 THEN ICD_nodecimal 
				WHEN THRU_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' THEN STUFF(ICD_nodecimal, 5, 0, '.')
				WHEN LEN(ICD_nodecimal) = 3 THEN ICD_nodecimal
			ELSE STUFF(ICD_nodecimal, 4, 0, '.') END as ICDCode,
		   'MedicaRE' as source
	FROM (SELECT DISTINCT ScrSSN FROM Dflt.BLData) as a inner join Dflt.MedicaREDx_inpat as b on a.ScrSSN = b.ScrSSN
	UNION
	SELECT a.ScrSSN,
		   SRVC_BGN_DT as AdmitDate,
		   SRVC_END_DT as DischargeDate,
		   line,
		   CASE WHEN SRVC_END_DT < CAST('2015-10-01' as date) THEN 9 ELSE 10 END AS ICDType,
		   CASE WHEN SRVC_END_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' AND LEN(ICD_nodecimal) = 4 THEN ICD_nodecimal 
				WHEN SRVC_END_DT < CAST('2015-10-01' as date) AND ICD_nodecimal like 'E%' THEN STUFF(ICD_nodecimal, 5, 0, '.')
				WHEN LEN(ICD_nodecimal) = 3 THEN ICD_nodecimal
			ELSE STUFF(ICD_nodecimal, 4, 0, '.') END as ICDCode,
		   ICD_nodecimal,
		   'MedicaID' as source
	FROM (SELECT DISTINCT ScrSSN FROM Dflt.BLData) as a inner join Dflt.MedicaIDDx_inpat as b on a.ScrSSN = b.ScrSSN
	)
SELECT DISTINCT ScrSSN, AdmitDate, DischargeDate, condition
INTO #outcome_CMS
	FROM comb as a inner join Dflt.dim_ICD910 as x on a.ICDCode = x.ICDCode AND a.ICDType = x.ICDType
	where condition in ('MI', 'IschemicStroke', 'HemoStroke', 'SAH')
	  AND line in (1, 2, 3)

/* 2.3.)-------------------------------------------------------------------------
		  MACE from Community care data
			inpatient discharge dx at 1st, 2nd or 3rd position
*/

DROP TABLE IF EXISTS #outcome_CommunityCare;
SELECT DISTINCT ScrSSN, startdate, enddate, condition
INTO #outcome_CommunityCare
	FROM Dflt.CommunityCare_inpat_clean as a inner join Dflt.dim_ICD910 as x on a.ICDCode = x.ICDCode AND a.ICDType = x.ICDType
		where condition in ('MI', 'IschemicStroke', 'HemoStroke', 'SAH', 'Flu', 'Melanoma')
		  AND ICDOrder in (1, 2, 3)

/* 2.4.)-------------------------------------------------------------------------
		MACE, merge all sources
*/

DROP TABLE IF EXISTS #MACE_allSource;
SELECT ScrSSN, 
	   CAST(AdmitDateTime as date) as AdmitDate,
	   CAST(DischargeDateTime as date) as DischargeDate, condition, 'VHA' as source 
INTO #MACE_allSource
FROM #outcome_VHA
UNION
SELECT ScrSSN, AdmitDate, DischargeDate, condition, 'CMS' as source FROM #outcome_CMS
UNION
SELECT ScrSSN, startdate as AdmitDate, enddate as DischargeDate, condition, 'Community Care' as source FROM #outcome_CommunityCare


/* 2.5.)-------------------------------------------------------------------------
		  MACE, roll-over events
		
			a 30-day gap to reconcile an event from overlapping encounters/claims
*/
DROP TABLE IF EXISTS Dflt.MACE_ALL;
with a0 as (
	select distinct ScrSSN, condition, AdmitDate, DischargeDate
	from #MACE_allSource 
	)
select early.scrssn, early.condition, early.AdmitDate, min(latest.DischargeDate) as DischargeDate			
into Dflt.MACE_ALL		
 from (select distinct scrssn, condition, AdmitDate			
       from a0 t1			
	   where not exists		
		(select distinct scrssn, condition, AdmitDate from a0 t2 	
		 where t1.Condition = t2.condition
		 and t1.scrssn = t2.scrssn	
		 and t2.AdmitDate < t1.AdmitDate 	
		 and dateadd(dd, 30, t2.DischargeDate) >= t1.AdmitDate)			-- 30 days
		 ) as early (scrssn, condition, AdmitDate )	
		 inner join	
		 (select distinct scrssn, condition, DischargeDate	
		  from a0 t3	
	      where  not exists		
		 (select distinct scrssn, condition, DischargeDate from  a0 t4	
	          where t3.Condition = t4.condition
			  and t3.scrssn = t4.scrssn		
			  and t4.DischargeDate > t3.DischargeDate
			  and t4.AdmitDate <= dateadd(dd, 30, t3.DischargeDate))	-- 30 days
		) as latest(scrssn, condition, DischargeDate)	
		on early.condition = latest.condition
		and early.scrssn = latest.scrssn 	
		and early.AdmitDate <= latest.DischargeDate	
group by early.scrssn, early.condition, early.AdmitDate			
go


/* 2.6.)-------------------------------------------------------------------------
		  Hx of MACE
		
			Update BL table for consistancy; discharge dx vs. 2 outpat/1 inpat dx
*/


with hx0 as (
	SELECT a.ScrSSN, a.startdate, b.DischargeDate, b.condition
		FROM Dflt.BLData as a LEFT JOIN Dflt.MACE_ALL AS b on a.ScrSSN = b.ScrSSN
			WHERE b.DischargeDate <= a.startdate
	)
SELECT b.*
INTO #MACE_BL_wide
FROM (SELECT ScrSSN, startdate, DischargeDate, condition from hx0) as t
PIVOT
(
MAX(DischargeDate)
FOR condition in
	(MI, 
	IschemicStroke, 
	HemoStroke, 
	SAH
	)
) b
GO

/* 2.6.)-------------------------------------------------------------------------
		  MACE, post-BL events
*/

DROP TABLE IF EXISTS #MACE_postBL_wide;
with event0 as (
	SELECT a.ScrSSN, a.startdate, b.DischargeDate, b.condition
		FROM Dflt.BLData as a LEFT JOIN Dflt.MACE_ALL AS b on a.ScrSSN = b.ScrSSN
			WHERE b.DischargeDate > a.startdate
	)
SELECT b.*
INTO #MACE_postBL_wide
FROM (SELECT ScrSSN, startdate, DischargeDate, condition from event0) as t
PIVOT
(
MIN(DischargeDate)
FOR condition in
	(MI, 
	IschemicStroke, 
	HemoStroke, 
	SAH,
	TIA
	)
) b
GO

/* 2.7.)-------------------------------------------------------------------------
		  MACE, update BL table
*/

ALTER TABLE Dflt.BLData
	ADD MI_dt date,
	    IschemicStroke_dt date,
	    HemoStroke_dt date,
		SAH_dt date,
		DeathDate date
GO

UPDATE Dflt.BLData
SET Dflt.BLData.Hx_MI_DT = z.MI,
	Dflt.BLData.Hx_IschemicStroke_DT = z.IschemicStroke,
	Dflt.BLData.Hx_HemoStroke_DT = z.HemoStroke,
	Dflt.BLData.Hx_SAH_DT = z.SAH,
	Dflt.BLData.MI_dt = y.MI,
	Dflt.BLData.IschemicStroke_dt = y.IschemicStroke,
	Dflt.BLData.HemoStroke_dt = y.HemoStroke,
	Dflt.BLData.SAH_dt = y.SAH,
	Dflt.BLData.DeathDate = zz.DeathDate
FROM Dflt.BLData as a left join #MACE_postBL_wide as y on a.ScrSSN = y.ScrSSN and a.startdate = y.startdate
					  left join #MACE_BL_wide as z on a.ScrSSN = z.ScrSSN and a.startdate = z.startdate
					  left join #DeathDates_all as zz on a.ScrSSN = zz.ScrSSN
GO


/* 3.1.)-------------------------------------------------------------------------
		  Outcome controls, flu test (antigen, PCR)
*/


DROP TABLE IF EXISTS #dim_flu;
SELECT distinct Phenotype, LabChemTestName, LabChemTestSID
INTO #dim_flu
  FROM CDWWork.ORDCovid.DimLabs
-- double check for flu antibody test, Hib or para-flu
  where phenotype in ('InfluAAntigen', 'InfluAPCR', 'InfluBAntigen', 'InfluBPCR', 'OtherInfluenza')
    AND LabChemTestName not like '%HAEMOPHILUS%'
	AND LabChemTestName not like '%H. INFLU%'
	AND LabChemTestName not like '%H.INFLU%'
	AND LabChemTestName not like '%PARAINFLUENZA%'


SELECT a.ScrSSN, b.LabChemSID, b.LabChemTestSID, y.LabChemTestName, b.LabChemSpecimenDateTime, b.LabChemResultValue, b.LabChemResultNumericValue,
       b.Units, b.Abnormal, b.RefHigh, b.RefLow
INTO #flu0
	FROM (SELECT DISTINCT ScrSSN from dflt.BLData) as a INNER JOIN Src.cohortcrosswalk as x on a.ScrSSN = x.ScrSSN
														INNER JOIN Src.Chem_LabChem as b on x.PatientSID = b.PatientSID
														INNER JOIN #dim_flu as y on b.LabChemTestSID = y.LabChemTestSID
	WHERE LabChemSpecimenDateTime >= CAST('2005-01-01' as date)
	  AND LabChemSpecimenDateTime <= CAST('2024-09-30' as date)


SELECT * 
INTO #flu_pos
FROM #flu0
WHERE (((LabChemResultValue LIKE '%POS%' or LabChemResultValue LIKE '%DET%') AND LabChemResultValue NOT LIKE '%NO%')
		OR LabChemResultValue = 'D'
		OR LabChemResultValue = 'P'
		OR LabChemResultValue = 'A'
		OR LabChemResultValue = 'B'
		OR LabChemResultValue = 'INFAP'
		OR LabChemResultValue = 'INFBP'
		OR LabChemResultValue = 'INFLUENZA A SUBTYPE H3'
		OR LabChemResultValue = 'PA'
		OR LabChemResultValue = 'PB'
		OR LabChemResultValue = 'PCR positive for H1N1'
		OR LabChemResultValue = 'PIA'
		OR LabChemResultValue = 'PIB'
		OR LabChemResultValue = 'INFAP')


/* 3.2.)-------------------------------------------------------------------------
		ICD codes for flu, malignancy and fracture
*/

DROP TABLE IF EXISTS #Dim_ICD9_OC;
SELECT DISTINCT ICD9SID, ICD9Code, condition
INTO #Dim_ICD9_OC
	FROM CDWWork.Dim.ICD9 as x inner join Dflt.dim_ICD910 as a on x.ICD9Code = a.ICDCode
WHERE ICDtype = 9 
  AND condition in ('Flu', 'Any_malignancy', 'Fracture')
GO

DROP TABLE IF EXISTS #Dim_ICD10_OC;
SELECT DISTINCT ICD10SID, ICD10Code, condition
INTO #Dim_ICD10_OC
	FROM CDWWork.Dim.ICD10 as x inner join Dflt.dim_ICD910 as a on x.ICD10Code = a.ICDCode
WHERE ICDtype = 10
  AND condition in ('Flu', 'Any_malignancy', 'Fracture')
GO


/* 3.3.)-------------------------------------------------------------------------
		  Outcome controls from VHA CDW

		    any outpat or inpat dx
*/

DROP TABLE IF EXISTS #OutcomeControls_VHA;
WITH in1 AS
	(SELECT a.scrssn, condition, dischargedatetime
		FROM Src.Inpat_InpatientDiagnosis id inner join #Dim_ICD9_OC as xx on id.ICD9SID = xx.ICD9SID 
											 inner join Src.CohortCrosswalk x on id.PatientSID = x.PatientSID
											 inner join (SELECT DISTINCT ScrSSN FROM Dflt.BLData) as a on a.scrssn = x.scrssn
	   WHERE  dischargeDateTime >= cast('2005-01-01' as date) 
	UNION
	 SELECT a.scrssn, condition, dischargedatetime
		FROM Src.Inpat_InpatientDiagnosis id inner join #Dim_ICD10_OC as xx on id.ICD10SID = xx.ICD10SID
											 inner join Src.CohortCrosswalk x on id.PatientSID = x.PatientSID
											 inner join (SELECT DISTINCT ScrSSN FROM Dflt.BLData) as a on a.scrssn = x.scrssn
	   WHERE  dischargeDateTime >= cast('2005-01-01' as date) 
		 ), 
  ot1 AS
	 (SELECT a.scrssn, condition, vdiagnosisdatetime
		FROM Src.Outpat_VDiagnosis id inner join #Dim_ICD9_OC as xx on id.ICD9SID = xx.ICD9SID
									  inner join Src.CohortCrosswalk x on id.PatientSID = x.PatientSID
									  inner join (SELECT DISTINCT ScrSSN FROM Dflt.BLData) as a on a.scrssn = x.scrssn
	   WHERE  VisitDateTime >= cast('2005-01-01' as date)
	UNION
	  SELECT a.scrssn, condition, vdiagnosisdatetime
		FROM Src.Outpat_VDiagnosis id inner join #Dim_ICD10_OC as xx on id.ICD10SID = xx.ICD10SID
									  inner join Src.CohortCrosswalk x on id.PatientSID = x.PatientSID
									  inner join (SELECT DISTINCT ScrSSN FROM Dflt.BLData) as a on a.scrssn = x.scrssn
	  WHERE  VisitDateTime >= cast('2005-01-01' as date)
	 ),
	 comb as (
	 SELECT scrssn, condition, dischargedatetime as recdate, 'VHA_inpat' as Source
	   FROM in1
	UNION
		SELECT scrssn, condition, vdiagnosisdatetime as recdate, 'VHA_Outpat' as Source
		   FROM ot1
	UNION 
		SELECT ScrSSN, 'Flu' as condition, LabChemSpecimenDateTime as redcate, 'VHA_Lab' as Source
		   FROM #flu_pos
		   )
SELECT DISTINCT ScrSSN, condition, CAST(recdate as date) as recdate, Source 
INTO #OutcomeControls_VHA
FROM comb

/* 3.4.)-------------------------------------------------------------------------
		  Outcome controls from Medicare
*/

DROP TABLE IF EXISTS #OutcomeControls_MedicaRE;
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
			WHERE CLM_TYPE in (40, 4012, 4013, 4014, 4083, 4085, 4089) 
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
			WHERE CLM_TYPE IN (4700, 71)
			  AND (TYPSRVCB in ('1', '2', '3', 'F', 'T') OR TYPSRVCB IS NULL)
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
	SELECT a.ScrSSN, condition, recdate, b.ICDCode, b.ICDType
		FROM medicare_in as b inner join Dflt.dim_ICD910 as z on b.ICDCode = z.ICDCode AND b.ICDType = z.ICDType
							  inner join (SELECT DISTINCT ScrSSN FROM Dflt.BLData) as a on a.ScrSSN = b.ScrSSN
		WHERE z.condition in ('Flu', 'Any_malignancy', 'Fracture')
	),
  ot1 AS
	 (SELECT a.scrssn, condition, recdate, b.ICDCode, b.ICDType
		FROM medicare_out as b inner join Dflt.dim_ICD910 as z on b.ICDCode = z.ICDCode AND b.ICDType = z.ICDType
							   inner join (SELECT DISTINCT ScrSSN FROM Dflt.BLData) as a on a.scrssn = b.scrssn
		WHERE z.condition in ('Flu', 'Any_malignancy', 'Fracture')
	 ), 
  comb AS
	 (SELECT scrssn, condition, recdate, ICDCode, ICDType
	   FROM in1
	UNION
	   SELECT scrssn, condition, recdate, ICDCode, ICDType
	   FROM ot1
	 )
SELECT scrssn, condition, recdate, 'MedicaRE' as source
	INTO #OutcomeControls_MedicaRE
		FROM comb
GO

/* 3.5.)-------------------------------------------------------------------------
		  Outcome controls from Medicaid
*/

DROP TABLE IF EXISTS #OutcomeControls_MedicaID;
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
	SELECT a.ScrSSN, condition, recdate, b.ICDCode, b.ICDType
		FROM medicaid_in as b inner join Dflt.dim_ICD910 as z on b.ICDCode = z.ICDCode AND b.ICDType = z.ICDType
							  inner join (SELECT DISTINCT ScrSSN FROM Dflt.BLData) as a on a.ScrSSN = b.ScrSSN
		WHERE z.condition in ('Flu', 'Any_malignancy', 'Fracture')
	),
  ot1 AS
	 (SELECT a.scrssn, condition, recdate, b.ICDCode, b.ICDType
		FROM medicaid_out as b inner join Dflt.dim_ICD910 as z on b.ICDCode = z.ICDCode AND b.ICDType = z.ICDType
							   inner join (SELECT DISTINCT ScrSSN FROM Dflt.BLData) as a on a.scrssn = b.scrssn
		WHERE z.condition in ('Flu', 'Any_malignancy', 'Fracture')
	 ), 
  comb AS
	 (SELECT scrssn, condition, recdate, ICDCode, ICDType
	   FROM in1
	UNION
	   SELECT scrssn, condition, recdate, ICDCode, ICDType
	   FROM ot1
	 )
SELECT scrssn, condition, recdate, 'MedicaID' as source
	INTO #OutcomeControls_MedicaID
		FROM comb
GO

/* 3.6.)-------------------------------------------------------------------------
		  Outcome controls from Community Care
*/

drop table if exists #OutcomeControl_CommunityCare;
 with in1 as (
	SELECT a.ScrSSN, condition, enddate as recdate, b.ICDCode, b.ICDType, b.ClaimSource
		FROM Dflt.CommunityCare_inpat_clean as b inner join Dflt.dim_ICD910 as z on b.ICDCode = z.ICDCode AND b.ICDType = z.ICDType
												 inner join (SELECT DISTINCT ScrSSN FROM Dflt.BLData) as a on a.ScrSSN = b.ScrSSN
		WHERE z.condition in ('Flu', 'Any_malignancy', 'Fracture')
	),
  ot1 AS
	 (SELECT a.scrssn, condition, startdate as recdate, b.ICDCode, b.ICDType, b.ClaimSource
		FROM Dflt.CommunityCare_outpat_clean as b inner join Dflt.dim_ICD910 as z on b.ICDCode = z.ICDCode AND b.ICDType = z.ICDType
										inner join (SELECT DISTINCT ScrSSN FROM Dflt.BLData) as a on a.scrssn = b.scrssn
		WHERE z.condition in ('Flu', 'Any_malignancy', 'Fracture')
	 ), 
  comb AS
	 (SELECT scrssn, condition, recdate, ICDCode, ICDType, ClaimSource
	   FROM in1
	UNION
	   SELECT scrssn, condition, recdate, ICDCode, ICDType, ClaimSource
	   FROM ot1
	 )
SELECT scrssn, condition, recdate, ICDCode, ICDType, ClaimSource, 'Community Care' as source
	INTO #OutcomeControl_CommunityCare
		FROM comb
GO

/* 3.7.)-------------------------------------------------------------------------
		  Merge outcome controls from all sources
*/

drop table if exists Dflt.OutcomeControls_allSource;
SELECT scrssn, condition, recdate, 'VHA' as source
INTO Dflt.OutcomeControls_allSource
	FROM #OutcomeControls_VHA
UNION
SELECT * FROM #OutcomeControls_MedicaRE
UNION
SELECT * FROM #OutcomeControls_MedicaID
UNION
SELECT  scrssn, condition, recdate, source FROM #OutcomeControl_CommunityCare
GO;

/* 3.8.)-------------------------------------------------------------------------
		  Reconcile outcome controls

			Flu: 30 days
			Malignancy: first primary malignancy
			Fracture: 90 days
*/


-- flu
DROP TABLE IF EXISTS #flu_rollup;
with a0 as (
	SELECT DISTINCT ScrSSN, condition, recdate, 
			recdate as startdate, DATEADD(day, 1, recdate) as enddate
		FROM Dflt.OutcomeControls_allSource

		where condition = 'Flu'
	)
select early.scrssn,  early.startdate, min(latest.enddate) as enddate			
into #flu_rollup			
 from (select distinct scrssn,  startdate			
       from a0 t1			
	   where not exists		
		(select distinct scrssn,    startdate from a0 t2 	
		 where t1.scrssn = t2.scrssn	
		 and t2.startdate < t1.startdate 	
		 and dateadd(dd, 30, t2.enddate) >= t1.startdate)		-- 30 days
		 ) as early (scrssn,  startdate )	
		 inner join	
		 (select distinct scrssn, enddate	
		  from a0 t3	
	      where  not exists		
		 (select distinct scrssn, enddate from  a0 t4	
	          where t3.scrssn = t4.scrssn		
			  and t4.enddate > t3.enddate
			  and t4.startdate <= dateadd(dd, 30, t3.enddate))	-- 30 days
		) as latest(scrssn,  enddate)	
		on early.scrssn = latest.scrssn 	
		and early.startdate <= latest.enddate	
group by early.scrssn, early.startdate			
go		

DROP TABLE IF EXISTS Dflt.flu;
with flu0 as (SELECT a.ScrSSN, a.startdate, b.startdate as FluDate
	FROM Dflt.BLData as a inner join #flu_rollup as b on a.ScrSSN = b.ScrSSN
	where b.startdate > a.startdate),
	flu1 as (
	SELECT *, 
			row_number() over (partition by scrssn, startdate order by scrssn, startdate, FluDate) as RN
		FROM flu0
	)
SELECT * 
INTO Dflt.flu
from flu1
WHERE rn = 1


-- any malignancy
with m0 as (
	SELECT DISTINCT ScrSSN, condition, recdate
		FROM Dflt.OutcomeControls_allSource
			WHERE condition = 'Any_malignancy'
	),
	firstM as (
	SELECT ScrSSN, min(recdate) as FirstMalignancyDate
	FROM m0
	GROUP BY ScrSSN
	),
	merge0 as (
	SELECT a.ScrSSN, a.startdate, b.recdate as MalignancyDate
		FROM Dflt.BLData as a inner join m0 as b on a.ScrSSN = b.ScrSSN
		where recdate > startdate
	),
	merge1 as (
	SELECT *,
		ROW_NUMBER() OVER(PARTITION BY scrssn, startdate order by scrssn, startdate, MalignancyDate) as rn
		from merge0
	)
SELECT a.ScrSSN, a.startdate, b.FirstMalignancyDate, a.MalignancyDate
INTO Dflt.Malignancy
	FROM merge1 as a left join firstM as b on a.ScrSSN = b.ScrSSN
	WHERE rn = 1

-- fracture
DROP TABLE IF EXISTS #fracture_rollup;
with a0 as (
	SELECT DISTINCT ScrSSN, condition, recdate, 
			recdate as startdate, DATEADD(day, 1, recdate) as enddate
		FROM Dflt.OutcomeControls_allSource
		where condition = 'Fracture'
	)
select early.scrssn,  early.startdate, min(latest.enddate) as enddate			
into #fracture_rollup			
 from (select distinct scrssn,  startdate			
       from a0 t1			
	   where not exists		
		(select distinct scrssn,    startdate from a0 t2 	
		 where t1.scrssn = t2.scrssn	
		 and t2.startdate < t1.startdate 	
		 and dateadd(dd, 90, t2.enddate) >= t1.startdate)		-- 90 days
		 ) as early (scrssn,  startdate )	
		 inner join	
		 (select distinct scrssn, enddate	
		  from a0 t3	
	      where  not exists		
		 (select distinct scrssn, enddate from  a0 t4	
	          where t3.scrssn = t4.scrssn		
			  and t4.enddate > t3.enddate
			  and t4.startdate <= dateadd(dd, 90, t3.enddate))	-- 90 days
		) as latest(scrssn,  enddate)	
		on early.scrssn = latest.scrssn 	
		and early.startdate <= latest.enddate	
group by early.scrssn, early.startdate			
go		


DROP TABLE IF EXISTS Dflt.fracture;
with f0 as (
			SELECT a.ScrSSN, a.startdate, b.startdate as FractureDate
				FROM Dflt.BLData as a inner join #fracture_rollup as b on a.ScrSSN = b.ScrSSN
					WHERE b.startdate > a.startdate),
	f1 as (
	SELECT *, 
			row_number() over (partition by scrssn, startdate order by scrssn, startdate, FractureDate) as RN
		FROM f0
	)
SELECT * 
INTO Dflt.fracture
from f1
WHERE rn = 1

/* 3.9.)-------------------------------------------------------------------------
		  Update BL table with outcome controls
*/

ALTER TABLE Dflt.BLData
	ADD FluDate date,
		AnyMalignancyDate date,
		FractureDate date
GO

UPDATE Dflt.BLData
SET Dflt.BLData.FluDate = x.FluDate,
	Dflt.BLData.AnyMalignancyDate = y.FirstMalignancyDate,
	Dflt.BLData.FractureDate = z.FractureDate
FROM Dflt.BLData as a left join Dflt.flu as x on a.ScrSSN = x.ScrSSN and a.startdate = x.startdate
					  left join Dflt.Malignancy as y on a.ScrSSN = y.ScrSSN and a.startdate = y.startdate
					  left join Dflt.fracture as z on a.ScrSSN = z.ScrSSN and a.startdate = z.startdate
GO

/* 4.)-------------------------------------------------------------------------
		Outcome control: weight change
			get all weight measurements from BL until deviation
*/

DROP TABLE IF EXISTS Dflt.WeightLoss;
select DISTINCT  a.ScrSSN, a.Arm, a.startdate, a.enddate, a.Weight_BL, a.Weight_BLDT, b.Weight, cast(b.VitalSignDate as date) as WeightLossDate,
	Weight - Weight_BL as diff
	into Dflt.WeightLoss
	FROM Dflt.BLData as a left join dflt.PostBL_weights as b on a.ScrSSN = b.ScrSSN
	where VitalSignDate > startdate
	 AND VitalSignDate <= enddate
