
/****************************************************************/
/*********************** Migraine sample ************************/
/****************************************************************/
					
/* 1.1.)----------------------------------------------------
			Pts and encounters from HCoE cohort 
*/
DROP TABLE IF EXISTS #encounters;
select * 
into #encounters
	from PCS_HCOE.Projects.Headache_icd_clinics_fy23
	where comdescrip = 'HCOE_Migraine'
GO

DROP TABLE IF EXISTS #demo_fy23;
SELECT ScrSSN, Sta3n, stapa, Visn, TestPatientFlag, CDWPossibleTestPatientFlag, VeteranFlag, 
		BirthDateTime, Gender, SelfIdentifiedGender, Race, Ethnicity, MaritalStatus, MeansTestStatus, ServiceConnectedFlag, 
		Eligibility, InsuranceCoverageFlag, gisurh
INTO #demo_fy23
  FROM [PCS_HCoE].[BLFY23].[Demographics]
GO

/* 1.2.)------------------------------------------------------------
		migraine define as 1 outpat (by provider & E/M app) 
			or 1 inpat discharge dx at any positions
*/

-- Outpat
DROP TABLE IF EXISTS #dx_ot;
with ot1 AS
	 (SELECT a.scrssn, id.VisitSID, id.sta3n, VisitDateTime
		FROM Src.Outpat_VDiagnosis id inner join CDWWork.Dim.ICD9 as zz on id.ICD9SID = zz.ICD9SID
									  inner join Src.CohortCrosswalk x on id.PatientSID = x.PatientSID
									  inner join (SELECT DISTINCT ScrSSN from #encounters) as a on a.ScrSSN = x.ScrSSN
			where zz.ICD9Code like '346%'
			  AND VisitDateTime is not null
			  AND WorkloadLogicFlag = 'Y'
			  AND VisitDateTime <= CAST('2023-09-30' as date)
	UNION
	  SELECT a.scrssn, id.VisitSID, id.sta3n, VisitDateTime
		FROM Src.Outpat_VDiagnosis id inner join CDWWork.Dim.ICD10 as zz on id.ICD10SID = zz.ICD10SID
									  inner join Src.CohortCrosswalk x on id.PatientSID = x.PatientSID
									  inner join (SELECT DISTINCT ScrSSN from #encounters) as a on a.ScrSSN = x.ScrSSN
			where zz.ICD10Code like 'G43%' 
			  AND VisitDateTime is not null
			  AND WorkloadLogicFlag = 'Y'
			  AND VisitDateTime <= CAST('2023-09-30' as date)
	 ), 
	PtScProvider as (
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
	  FROM OT1 as a LEFT JOIN CDWWork.Outpat.VProvider as b on a.VisitSID = b.VisitSID
						 LEFT JOIN CDWWork.Dim.ProviderType as c on b.ProviderTypeSID = c.ProviderTypeSID
		WHERE b.PrimarySecondary = 'P'
			AND b.WorkloadLogicFlag = 'Y'
			),
	pt_MDs as (SELECT distinct *
				from PtScProvider
					WHERE ProviderCategory in (1, 2, 3)),
	EM_cpt as (SELECT distinct a.*
					FROM pt_MDs AS a LEFT JOIN CDWWork.Outpat.VProcedure as d on a.VisitSID = d.VisitSID
									 LEFT JOIN CDWWork.Dim.CPT as x on d.CPTSID = x.CPTSID
					WHERE CPTCode in ('99201', '99202', '99203', '99204', '99205', 
					  '99211', '99212', '99213', '99214', '99215',										/* office visit */
					  '99241', '99242', '99243', '99244', '99245',										/* office consultation */
					  '99281', '99282', '99283', '99284', '99285',										/* ED */
					  '99385', '99386', '99387', '99395', '99396', '99397',								/* Prev visit */
					  '99401', '99402', '99403', '99404',												/* Prev consult */
					  '99371', '99372', '99373', 													    /* phone consult before 1/1/2008 */
					  '99441', '99442', '99443', '99444',												/* phone/online E/M */
					  '99421', '99422', '99423')),
	EM_cpt2 as (SELECT *,
					   ROW_NUMBER() OVER(PARTITION BY ScrSSN ORDER BY ScrSSN, VisitDateTime, VisitSID) as rn
				FROM EM_cpt)
SELECT *	
INTO #dx_ot
	FROM EM_cpt2
	WHERE rn = 1
GO

-- inpat discharge dx
DROP TABLE IF EXISTS #dx_in;
with in1 as (
	SELECT a.scrssn, id.sta3n, id.InpatientSID, dischargedatetime, ICD9Code as ICDCode, OrdinalNumber
			FROM Src.Inpat_InpatientDiagnosis id inner join CDWWork.Dim.ICD9 as zz on id.ICD9SID = zz.ICD9SID 
												 inner join Src.CohortCrosswalk x on id.PatientSID = x.PatientSID
												 inner join (SELECT DISTINCT ScrSSN from #encounters) as a on a.scrssn = x.scrssn
		   WHERE dischargeDateTime <= cast('2023-09-30' as date) 
			 AND dischargeDateTime is not null
			 AND zz.ICD9Code like '346%'
		UNION
		 SELECT a.scrssn, id.sta3n, id.InpatientSID, dischargedatetime, ICD10Code as ICDCode, OrdinalNumber
			FROM Src.Inpat_InpatientDiagnosis id inner join CDWWork.Dim.ICD10 as zz on id.ICD10SID = zz.ICD10SID
												 inner join Src.CohortCrosswalk x on id.PatientSID = x.PatientSID
												 inner join (SELECT DISTINCT ScrSSN from #encounters) as a on a.scrssn = x.scrssn
		   WHERE dischargeDateTime <= cast('2023-09-30' as date) 
			 AND dischargeDateTime is not null
			 AND zz.ICD10Code like 'G43%'),
	in2 as (
	SELECT *,
		ROW_NUMBER() OVER(PARTITION BY ScrSSN ORDER BY ScrSSN, dischargedatetime)  as rn
		from in1
	)
SELECT *
INTO #dx_in
from in2
where rn = 1

-- combine outpat and inpat 
DROP TABLE IF EXISTS Dflt.Pt0;
with comb1 as (
		SELECT ScrSSN, sta3n, VisitDateTime as DxDate
			FROM #dx_ot
		UNION
		SELECT ScrSSN, sta3n, DischargeDatetime as DxDate
			FROM #dx_in),
	comb2 as (
		SELECT *, ROW_NUMBER() OVER(PARTITION BY ScrSSN ORDER BY ScrSSN, DxDate)  as rn
		FROM comb1
	)
SELECT a.ScrSSN, a.sta3n, 
		CAST(x.BirthDateTime as date) as BirthDateTime, 
		CAST(DxDate as date) as FirstMigDate
into Dflt.Pt0
from comb2 as a inner join #demo_fy23 as x on a.ScrSSN = x.ScrSSN
where rn = 1



/****************************************************************/
/************************* Exposure *****************************/
/****************************************************************/

/* 2.1)------------------------------------------------------------
			Erenumab, Fremanezumab, galcanezumab
note:
	1. Through 9/30/2024
    2. Only use VA outpatRX as the source for pt who regularly use the VHA (one PC visit in past 12 months).
	3. Will not use Rx from CMS, because the cutoff dates were through 12/31/2022, and even brought in, 
		treatment length only increased days.
*/

DROP TABLE IF EXISTS #mab0;
select a.ScrSSN, y.DrugNameWithoutDose, y.DrugNameWithDose, z.StrengthNumeric, b.QtyNumeric, b.DaysSupply, b.ReleaseDateTime,
		cast(b.ReleaseDateTime as date) as startdate
	into #mab0
	FROM Dflt.Pt0  as a inner join Src.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
						inner join CDWWork.RxOut.RxOutpatFill as b on x.PatientSID = b.PatientSID
						inner join dflt.dim_rx as y on y.NationalDrugSID = b.NationalDrugSID
						inner join CDWWork.Dim.NationalDrug as z on y.NationalDrugSID = z.NationalDrugSID
		WHERE b.ReleaseDateTime >= a.FirstMigDate
		AND	y.DrugNameWithoutDose in ('ERENUMAB-AOOE', 'FREMANEZUMAB-VFRM', 'GALCANEZUMAB-GNLM')
		AND y.DrugNameWithDose not in ('GALCANEZUMAB-GNLM 100MG/ML INJ,SYRINGE,1ML')  -- label for cluster headache
		AND ReleaseDateTime IS NOT NULL
		AND ReleaseDateTime <= CAST('2024-09-30' AS DATE)
GO


-- calculate days' supplied. 
-- 240mg loading dose for galcanezumab!
SELECT *,
		row_number() over (partition by ScrSSN, DrugNameWithoutDose, grp order by ReleaseDateTime) as RN
	into #mab1
		FROM (SELECT *,
			 ROW_NUMBER() OVER (PARTITION BY ScrSSN ORDER BY ReleaseDateTime)
			 - ROW_NUMBER() OVER (PARTITION BY ScrSSN, DrugNameWithoutDose ORDER BY ReleaseDateTime) AS grp
			FROM #mab0) x

SELECT *,
		CASE WHEN DrugNameWithoutDose = 'GALCANEZUMAB-GNLM' and RN = 1 AND QtyNumeric = 1 THEN 30
			 WHEN DrugNameWithoutDose = 'GALCANEZUMAB-GNLM' and RN = 1 AND QtyNumeric > 1 THEN (QtyNumeric-1)*30
			 ELSE QtyNumeric*30
		END AS DaySupply_calc
	into #mab2
	FROM #mab1

SELECT distinct ScrSSn, DrugNameWithoutDose, QtyNumeric, DaysSupply, cast(ReleaseDateTime as date) as ReleaseDateTime, DaySupply_calc, startdate,
	  CAST(DATEADD(DAY, DaySupply_calc, startdate) as DATE) as enddate
into #mab3
from #mab2
GO

/* 2.2)------------------------------------------------------------
			Eptinezumab
*/

DROP TABLE IF EXISTS #JJMR;
select distinct a.ScrSSN, 
		'EPTINEZUMAB-JJMR' AS DrugNameWithoutDose, 
		1 AS QtyNumeric, 
		90 AS DaysSupply, 
		cast(b.StartDateTime as date) AS ReleaseDateTime,
		90.00 AS DaySupply_calc,
		cast(b.StartDateTime as date) as startdate,
		CAST(DATEADD(DAY, 90, b.StartDateTime) as DATE) as enddate
into #JJMR
	FROM Dflt.Pt0  as a inner join Src.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
						inner join CDWWork.RxIV.IVAdditive as b on x.PatientSID = b.PatientSID
						inner join CDWWork.Dim.IVAdditiveIngredient as y on b.IVAdditiveIngredientSID = y.IVAdditiveIngredientSID
						inner join CDWWork.RxIV.IV as c on b.IVSID = c.IVSID
		WHERE b.StartDateTime >= a.FirstMigDate
		AND	y.IVAdditiveIngredientPrintName like '%Eptinezumab%'
		AND c.AdministeredDoseTotal IS NOT NULL
		AND b.StartDateTime IS NOT NULL
		AND b.StartDateTime <= CAST('2024-09-30' AS DATE)
GO

SELECT * 
INTO #allmabs 
	FROM #mab3 
UNION 
SELECT * from #JJMR
GO

/* 2.3)------------------------------------------------------------
			gepants for prevention
*/
DROP TABLE IF EXISTS #gepant;
SELECT a.ScrSSN, y.DrugNameWithoutDose, y.DrugNameWithDose, b.QtyNumeric, b.DaysSupply, b.ReleaseDateTime,
		30*QtyNumeric/DaysSupply as QTY_30days, b.Sta3n
	into #gepant
	FROM Dflt.Pt0 as a inner join Src.CohortCrosswalk as x on a.ScrSSN = x.ScrSSN
					   inner join CDWWork.RxOut.RxOutpatFill as b on x.PatientSID = b.PatientSID
					   inner join dflt.dim_rx as y on b.NationalDrugSID = y.NationalDrugSID
		WHERE b.ReleaseDateTime > a.firstMigDate
		AND	y.RX_Cat = 'RX_Mgr_aCGRP' 
		AND y.DrugNameWithoutDose in ('ATOGEPANT', 'RIMEGEPANT')
		AND ReleaseDateTime IS NOT NULL
		AND ReleaseDateTime <= CAST('2024-09-30' as date)
GO

-- rimegepant for prevention defined as receiving 12+ tablets per 30 days
DROP TABLE IF EXISTS #g1_prev;
with g0 as (
select ScrSSN, DrugNameWithoutDose, QtyNumeric, DaysSupply, ReleaseDateTime,
	CASE WHEN DrugNameWithoutDose = 'ATOGEPANT' THEN DaysSupply
		 WHEN DrugNameWithoutDose = 'RIMEGEPANT' and QTY_30days >= 12 THEN QtyNumeric*2
		 ELSE NULL END AS DaySupply_calc,
		 cast(ReleaseDateTime as date) as startdate
from #gepant
where (DrugNameWithoutDose = 'ATOGEPANT')
	OR (DrugNameWithoutDose = 'RIMEGEPANT' and QTY_30days >= 12))
SELECT distinct ScrSSN, DrugNameWithoutDose, QtyNumeric, DaysSupply, ReleaseDateTime, 
		DaySupply_calc, startdate, 
		CAST(DATEADD(DAY, DaySupply_calc, startdate) as DATE) as enddate
	INTO #g1_prev
	FROM g0

/* 2.4)------------------------------------------------------------
			merge all mAbs and gepants
*/

DROP TABLE IF EXISTS Dflt.aCGRPPrev;
SELECT * 
INTO Dflt.aCGRPPrev
	FROM #allmabs 
UNION 
SELECT * from #g1_prev
GO

/* 2.5)------------------------------------------------------------
			Create aCGRP person-trials
*/

drop table IF EXISTS #Prescriptions;
--ALTER table #Prescriptions
--drop CONSTRAINT CHK_Prescriptions_ed_sd

  CREATE TABLE #Prescriptions
(
  prescriptionid INT  NOT NULL IDENTITY,
  patientid      varchar(9)  NOT NULL,
  startdate      DATE NOT NULL,
  numdays    INT  NOT NULL,
  enddate AS DATEADD(day, numdays, startdate),
  CONSTRAINT CHK_Prescriptions_ed_sd CHECK(numdays > 0)
);

CREATE UNIQUE CLUSTERED INDEX idx_start
  ON #Prescriptions
   (patientid, startdate, prescriptionid);

ALTER TABLE #Prescriptions
  ADD CONSTRAINT PK_Prescriptions PRIMARY KEY
    NONCLUSTERED(prescriptionid);

INSERT INTO #Prescriptions
  (patientid, startdate, numdays) 
select scrssn, startdate, DaySupply_calc
from Dflt.aCGRPPrev
go

-----------------------
-- editted cursor
-----------------------
SET NOCOUNT ON;

DECLARE @Result AS TABLE
(
  patientid varchar(9)  NOT NULL,
  startdate DATE NOT NULL,
  enddate   DATE NOT NULL,
  daystotal INT NOT NULL
);

DECLARE
  @patientid      AS varchar(9),
  @startdate      AS DATE,
  @numdays    AS INT,
  @sumnumdays     AS INT,
  @prevpatientid  AS varchar(9),
  @prevnumdays AS INT,
  @daystot as INT,
  @firststartdate AS DATE;

DECLARE C CURSOR FAST_FORWARD FOR
  SELECT patientid, startdate, numdays
  FROM #Prescriptions
  ORDER BY patientid, startdate, prescriptionid;

OPEN C;

FETCH NEXT FROM C INTO
  @patientid, @startdate, @numdays;

SELECT
  @prevpatientid  = @patientid,
  @prevnumdays    = @numdays,
  @firststartdate = @startdate,
  @daystot        = 0,
  @sumnumdays     = 0;

WHILE @@fetch_status = 0
BEGIN

  IF    @prevpatientid <> @patientid
     OR DATEADD(day, @sumnumdays+90, @firststartdate)           -- 90-day gap
      <   @startdate
 BEGIN --begin loop to end this run
   INSERT INTO @Result(patientid, startdate, enddate, daystotal)
   VALUES(@prevpatientid, @firststartdate,
      DATEADD(day, @sumnumdays, @firststartdate), @daystot);

  SELECT  --create new startdate
    @firststartdate = @startdate,
    @sumnumdays     = 0,
	@daystot        = 0;
 END --end loop to end this run


 --
 IF @prevpatientid = @prevpatientid
   AND DATEADD(day, @sumnumdays, @firststartdate)
      < @startdate
	AND DATEADD(day, @sumnumdays + 90, @firststartdate)			-- 90-day gap
	  >= @startdate

   BEGIN
      SELECT 
	    @sumnumdays += DATEDIFF(day, DATEADD(day, @sumnumdays, @firststartdate), @startdate)
   END


  SELECT
    @sumnumdays    += @numdays,
	@daystot       += @numdays,
    @prevpatientid  = @patientid,
	@prevnumdays    = @numdays;

  FETCH NEXT FROM C INTO
    @patientid, @startdate, @numdays;
END

IF @sumnumdays > 0
  INSERT INTO @Result(patientid, startdate, enddate, daystotal)
    VALUES(@prevpatientid, @firststartdate,
  DATEADD(day, @sumnumdays, @firststartdate), @daystot);

CLOSE C;

DEALLOCATE C;
SET NOCOUNT OFF;

SELECT * 
into #aCGRP_person_trial
FROM @Result;


drop table if exists Dflt.aCGRP_PersonTrials;
SELECT DISTINCT CAST(patientid as VARCHAR(9)) as ScrSSN,
	   'aCGRP' as Arm,
	   startdate,
	   enddate,
	   daystotal as DaysSupplyTotal		/* sum total Days' Supplied */
into Dflt.aCGRP_PersonTrials
FROM #aCGRP_person_trial



/* 3.1)------------------------------------------------------------
			Topirimate as the comparison drug
*/


drop table IF EXISTS #tpm_VHA0;
with t0 as (
select a.ScrSSN, firstMigDate, b.RxOutpatSID, b.RxOutpatFillSID,
		c.DrugNameWithoutDose, c.DrugNameWithDose, y.StrengthNumeric, b.QtyNumeric, 
		b.DaysSupply, CAST(FillDateTime as date) as FillDate, DispensedDate,
		CAST(b.ReleaseDateTime as date) as ReleaseDate, DoseOrdered, d.Schedule, MailWindow, FillRemarks
	FROM Dflt.Pt0 as a inner join Src.cohortcrosswalk as x on a.ScrSSN = x.ScrSSN
					   inner join CDWWork.RxOut.RxOutpatFill as b on x.PatientSID = b.PatientSID
					   inner join dflt.dim_rx as c on c.NationalDrugSID = b.NationalDrugSID
					   inner join CDWWork.Dim.NationalDrug as y on c.NationalDrugSID = y.NationalDrugSID
					   left join (SELECT DISTINCT RxOutpatSID, DoseOrdered, Schedule
									FROM Src.RxOut_RxOutpatMedInstructions
										WHERE DoseOrdered IS NOT NULL
										  AND Schedule IS NOT NULL
										  AND InstructionNumber = 1) as d on b.RxOutpatSID = d.RxOutpatSID
		WHERE	c.RX_Cat in ('RX_Mgr_TPM')
		AND ReleaseDateTime IS NOT NULL
		AND ReleaseDateTime <= CAST('2024-09-30' as date)
		AND QtyNumeric IS NOT NULL
		AND DaysSupply IS NOT NULL),
 t1 as (
	SELECT *, 
		  ROW_NUMBER() OVER (PARTITION BY ScrSSN, ReleaseDate  ORDER BY ScrSSN, ReleaseDate, RxOutpatFillSID desc) as rn
		  from t0),
 t2 as (
	SELECT ScrSSN, firstMigDate, min(ReleaseDate) as TPM_1st_Date
		FROM t0
		GROUP BY ScrSSN, firstMigDate
	)
SELECT a.ScrSSN, a.firstMigDate, TPM_1st_Date, RxOutpatSID, RxOutpatFillSID, DrugNameWithoutDose, DrugNameWithDose, StrengthNumeric, QtyNumeric, 
		DaysSupply, 
		DaysSupply as DaySupply_calc,		-- check extreme values, manual correction if needed.
		ReleaseDate, DoseOrdered, Schedule, rn
INTO #TPM_VHA0
FROM t1 as a left join t2 as b on a.ScrSSN = b.ScrSSN
GO

DROP TABLE IF EXISTS #TPM_VHA1;
SELECT ScrSSN, firstMigDate, DrugNameWithoutDose, TPM_1st_Date, ReleaseDate, 
		MAX(QtyNumeric) as QtyNumeric,
		MAX(DaysSupply) as DaysSupply,
		MAX(DaySupply_calc) as DaySupply_calc
INTO #TPM_VHA1
FROM #TPM_VHA0
GROUP BY ScrSSN, firstMigDate, DrugNameWithoutDose, TPM_1st_Date, ReleaseDate 


DROP TABLE IF EXISTS Dflt.TPM;
SELECT ScrSSN, DrugNameWithoutDose , QtyNumeric, DaysSupply,
		DaySupply_calc,
		CAST(ReleaseDate AS DATE) as startdate,
		CAST(DATEADD(DAY, DaySupply_calc, ReleaseDate) as DATE) as enddate
INTO Dflt.TPM
	FROM #TPM_VHA1
GO

/* 3.2)------------------------------------------------------------
			create TPM person-trials
*/

-- ALTER table #Prescriptions
-- drop CONSTRAINT CHK_Prescriptions_ed_sd

DROP TABLE IF EXISTS #Prescriptions;
  CREATE TABLE #Prescriptions
(
  prescriptionid INT  NOT NULL IDENTITY,
  patientid      varchar(9)  NOT NULL,
  startdate      DATE NOT NULL,
  numdays    INT  NOT NULL,
  enddate AS DATEADD(day, numdays, startdate),
  CONSTRAINT CHK_Prescriptions_ed_sd CHECK(numdays > 0)
);

CREATE UNIQUE CLUSTERED INDEX idx_start
  ON #Prescriptions
   (patientid, startdate, prescriptionid);

ALTER TABLE #Prescriptions
  ADD CONSTRAINT PK_Prescriptions PRIMARY KEY
    NONCLUSTERED(prescriptionid);

INSERT INTO #Prescriptions
  (patientid, startdate, numdays) 
select scrssn, startdate, DaySupply_calc
from Dflt.TPM
go

-----------------------
-- editted cursor
-----------------------
DROP TABLE IF EXISTS #TPM_person_trial;
SET NOCOUNT ON;

DECLARE @Result AS TABLE
(
  patientid varchar(9)  NOT NULL,
  startdate DATE NOT NULL,
  enddate   DATE NOT NULL,
  daystotal INT NOT NULL
);

DECLARE
  @patientid      AS varchar(9),
  @startdate      AS DATE,
  @numdays    AS INT,
  @sumnumdays     AS INT,
  @prevpatientid  AS varchar(9),
  @prevnumdays AS INT,
  @daystot as INT,
  @firststartdate AS DATE;

DECLARE C CURSOR FAST_FORWARD FOR
  SELECT patientid, startdate, numdays
  FROM #Prescriptions
  ORDER BY patientid, startdate, prescriptionid;

OPEN C;

FETCH NEXT FROM C INTO
  @patientid, @startdate, @numdays;

SELECT
  @prevpatientid  = @patientid,
  @prevnumdays    = @numdays,
  @firststartdate = @startdate,
  @daystot        = 0,
  @sumnumdays     = 0;

WHILE @@fetch_status = 0
BEGIN

  IF    @prevpatientid <> @patientid
     OR DATEADD(day, @sumnumdays+90, @firststartdate)			-- 90-day gap
      <   @startdate
 BEGIN --begin loop to end this run
   INSERT INTO @Result(patientid, startdate, enddate, daystotal)
   VALUES(@prevpatientid, @firststartdate,
      DATEADD(day, @sumnumdays, @firststartdate), @daystot);

  SELECT  --create new startdate
    @firststartdate = @startdate,
    @sumnumdays     = 0,
	@daystot        = 0;
 END --end loop to end this run


 --
 IF @prevpatientid = @prevpatientid
   AND DATEADD(day, @sumnumdays, @firststartdate)
      < @startdate
	AND DATEADD(day, @sumnumdays + 90, @firststartdate)			-- 90-day gap
	  >= @startdate

   BEGIN
      SELECT 
	    @sumnumdays += DATEDIFF(day, DATEADD(day, @sumnumdays, @firststartdate), @startdate)
   END

  SELECT
    @sumnumdays    += @numdays,
	@daystot       += @numdays,
    @prevpatientid  = @patientid,
	@prevnumdays    = @numdays;

  FETCH NEXT FROM C INTO
    @patientid, @startdate, @numdays;
END

IF @sumnumdays > 0
  INSERT INTO @Result(patientid, startdate, enddate, daystotal)
    VALUES(@prevpatientid, @firststartdate,
  DATEADD(day, @sumnumdays, @firststartdate), @daystot);

CLOSE C;

DEALLOCATE C;

SET NOCOUNT OFF;

SELECT * 
into #TPM_person_trial
FROM @Result;


drop table IF EXISTS Dflt.TPM_PersonTrials;
SELECT DISTINCT CAST(patientid as VARCHAR(9)) as ScrSSN,
	   'TPM' as Arm,
	   startdate,
	   enddate,
	   daystotal as DaysSupplyTotal		/* sum total Days' Supplied */
into Dflt.TPM_PersonTrials
FROM #TPM_person_trial


/* 4.1)------------------------------------------------------------
			Lisinopril and Candesartan
*/

select a.ScrSSN, b.RxOutpatSID, b.RxOutpatFillSID,
		c.DrugNameWithoutDose, c.DrugNameWithDose, y.StrengthNumeric, b.QtyNumeric, 
		b.DaysSupply, 
		b.ReleaseDateTime,		
		d.DoseOrdered, d.Schedule
INTO #Lisinopril_VHA0
	FROM Dflt.Pt0 as a inner join Src.cohortcrosswalk as x on a.ScrSSN = x.ScrSSN
					   inner join CDWWork.RxOut.RxOutpatFill as b on x.PatientSID = b.PatientSID
					   inner join dflt.dim_rx as c on c.NationalDrugSID = b.NationalDrugSID
					   inner join CDWWork.Dim.NationalDrug as y on c.NationalDrugSID = y.NationalDrugSID
					   left join (SELECT DISTINCT RxOutpatSID, DoseOrdered, Schedule
										FROM Src.RxOut_RxOutpatMedInstructions
											WHERE DoseOrdered IS NOT NULL
											  AND Schedule IS NOT NULL
											  AND InstructionNumber = 1) as d on b.RxOutpatSID = d.RxOutpatSID
		WHERE c.Rx_Cat = 'RX_Mgr_ACEI_ARB'
		AND ReleaseDateTime IS NOT NULL
		AND ReleaseDateTime <= CAST('2024-09-30' as date)
		AND QtyNumeric IS NOT NULL
		AND DaysSupply IS NOT NULL


drop table IF EXISTS #Lisinopril_VHA1;
with a0 as (SELECT *,
				   DaysSupply AS DaySupply_calc		-- check extreme values, manual correction if needed.
	FROM #Lisinopril_VHA0)
SELECT *,
		CAST(ReleaseDateTime AS DATE) as startdate,
		CAST(DATEADD(DAY, DaySupply_calc, ReleaseDateTime) as DATE) as enddate
INTO #Lisinopril_VHA1
FROM a0

DROP TABLE IF EXISTS Dflt.LSP;
SELECT ScrSSN, DrugNameWithoutDose, QtyNumeric, DaysSupply, ReleaseDateTime, DaySupply_calc,
		CAST(ReleaseDateTime AS DATE) as startdate,
		CAST(DATEADD(DAY, DaySupply_calc, ReleaseDateTime) as DATE) as enddate
INTO Dflt.LSP
FROM #Lisinopril_VHA1


/* 4.2)------------------------------------------------------------
				Create LSP/CAN person-trials
*/

drop table IF EXISTS #Prescriptions;
  CREATE TABLE #Prescriptions
(
  prescriptionid INT  NOT NULL IDENTITY,
  patientid      varchar(9)  NOT NULL,
  startdate      DATE NOT NULL,
  numdays    INT  NOT NULL,
  enddate AS DATEADD(day, numdays, startdate),
  CONSTRAINT CHK_Prescriptions_ed_sd CHECK(numdays > 0)
);

CREATE UNIQUE CLUSTERED INDEX idx_start
  ON #Prescriptions
   (patientid, startdate, prescriptionid);

ALTER TABLE #Prescriptions
  ADD CONSTRAINT PK_Prescriptions PRIMARY KEY
    NONCLUSTERED(prescriptionid);

INSERT INTO #Prescriptions
  (patientid, startdate, numdays) 
select scrssn, startdate, DaySupply_calc
from Dflt.LSP
go

-----------------------
-- editted cursor
-----------------------
SET NOCOUNT ON;

DECLARE @Result AS TABLE
(
  patientid varchar(9)  NOT NULL,
  startdate DATE NOT NULL,
  enddate   DATE NOT NULL,
  daystotal INT NOT NULL
);

DECLARE
  @patientid      AS varchar(9),
  @startdate      AS DATE,
  @numdays    AS INT,
  @sumnumdays     AS INT,
  @prevpatientid  AS varchar(9),
  @prevnumdays AS INT,
  @daystot as INT,
  @firststartdate AS DATE;

DECLARE C CURSOR FAST_FORWARD FOR
  SELECT patientid, startdate, numdays
  FROM #Prescriptions
  ORDER BY patientid, startdate, prescriptionid;

OPEN C;

FETCH NEXT FROM C INTO
  @patientid, @startdate, @numdays;

SELECT
  @prevpatientid  = @patientid,
  @prevnumdays    = @numdays,
  @firststartdate = @startdate,
  @daystot        = 0,
  @sumnumdays     = 0;

WHILE @@fetch_status = 0
BEGIN

  IF    @prevpatientid <> @patientid
     OR DATEADD(day, @sumnumdays+90, @firststartdate)		-- 90-day gap
      <   @startdate
 BEGIN --begin loop to end this run
   INSERT INTO @Result(patientid, startdate, enddate, daystotal)
   VALUES(@prevpatientid, @firststartdate,
      DATEADD(day, @sumnumdays, @firststartdate), @daystot);

  SELECT  --create new startdate
    @firststartdate = @startdate,
    @sumnumdays     = 0,
	@daystot        = 0;
 END --end loop to end this run


 --
 IF @prevpatientid = @prevpatientid
   AND DATEADD(day, @sumnumdays, @firststartdate)
      < @startdate
	AND DATEADD(day, @sumnumdays + 90, @firststartdate)		-- 90-day gap
	  >= @startdate

   BEGIN
      SELECT 
	    @sumnumdays += DATEDIFF(day, DATEADD(day, @sumnumdays, @firststartdate), @startdate)
   END


  SELECT
    @sumnumdays    += @numdays,
	@daystot       += @numdays,
    @prevpatientid  = @patientid,
	@prevnumdays    = @numdays;

  FETCH NEXT FROM C INTO
    @patientid, @startdate, @numdays;
END

IF @sumnumdays > 0
  INSERT INTO @Result(patientid, startdate, enddate, daystotal)
    VALUES(@prevpatientid, @firststartdate,
  DATEADD(day, @sumnumdays, @firststartdate), @daystot);

CLOSE C;

DEALLOCATE C;

SET NOCOUNT OFF;

SELECT * 
into #LSPL_person_trial
FROM @Result;


SELECT patientid as ScrSSN, 'LSP/CAN' as Arm, startdate, enddate, daystotal as DaysSupplyTotal
INTO Dflt.LSP_PersonTrials
FROM #LSPL_person_trial
-- (414822 rows affected)


/* 4.2)------------------------------------------------------------
			Combine all treatment episodes from study meds (person-trials)
*/

drop table IF EXISTS Dflt.Exposure;
with a0 AS (
		SELECT * FROM Dflt.aCGRP_PersonTrials
		UNION
		SELECT * FROM Dflt.LSP_PersonTrials
		UNION
		SELECT * FROM Dflt.TPM_PersonTrials),
	aCGRP AS (
	SELECT ScrSSN, min(startdate) as First_aCGRPDate FROM Dflt.aCGRPPrev GROUP BY ScrSSN),
	Lisinopril AS (
	SELECT ScrSSN, min(startdate) as First_LSPDate FROM Dflt.LSP GROUP BY ScrSSN),
	TPM AS (
	SELECT ScrSSN, min(startdate) as First_TPMDate FROM Dflt.TPM GROUP BY ScrSSN)
SELECT a.*, b.sta3n, b.BirthDateTime, b.FirstMigDate, c.First_aCGRPDate, d.First_LSPDate, e.First_TPMDate
INTO Dflt.Exposure
FROM a0 as a left join Dflt.Pt0 as b on a.ScrSSN = b.ScrSSN
			 left join aCGRP as c on a.ScrSSN = c.ScrSSN
			 left join Lisinopril as d on a.ScrSSN = d.ScrSSN
			 left join TPM as e on a.ScrSSN = e.ScrSSN
GO
