
/*************************************************************************/
/*************************** Comorbidity *********************************/
/*************************************************************************/

-- ICD9 codes
DROP TABLE IF EXISTS #dim9;
with dim9_ICDs as (
	SELECT CASE WHEN ICD9Code like '346.7%' THEN 101
				END AS comnum,  
			'CMB_ChronicMigraine' as condition,
			ICD9Code
			FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '410%' THEN 102
				END AS comnum,  
			'MI' as condition,
			ICD9Code
			FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '433._1%' 
				  OR ICD9Code like '434._1%' 
				  OR ICD9Code like '436%' THEN 103
				END AS comnum,  
			'IschemicStroke' as condition,
			ICD9Code
			FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '435%' THEN 104
				END AS comnum,  
			'TIA' as condition,
			ICD9Code
			FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '431%' THEN 105
				END AS comnum,  
			'HemoStroke' as condition,
			ICD9Code
			FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '430%' THEN 106
				END AS comnum,  
			'SAH' as condition,
			ICD9Code
			FROM CDWWork.Dim.ICD9
			UNION
  	SELECT CASE WHEN ICD9Code like '487%'
				  OR ICD9Code like '488%' THEN 107
				END AS comnum, 
			'Flu' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
  	SELECT CASE WHEN (ICD9Code like '1[4-9]%' OR ICD9Code like '20[0-8]%')
					  AND ICD9Code not like '173%'
					  AND ICD9Code not like '19[6-8]%' THEN 108
				END AS comnum, 
			'Any_malignancy' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
  	SELECT CASE WHEN ICD9Code like '8[0-2]%' THEN 109
				END AS comnum, 
			'Fracture' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '427.3%' THEN 201
				END AS comnum, 
			'CMB_AFibFlutter' as condition,
			ICD9Code
			FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '425%'
				OR ICD9Code like '429.83%' THEN 202
				END AS comnum, 
			'CMB_Cardiomyopathy' as condition,
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '398.91%' 
				OR ICD9Code like '402._1%'
				OR ICD9Code like '404._1%'
				OR ICD9Code like '404._3%'
				OR ICD9Code like '428%' THEN 203
				END AS comnum, 
			'CMB_HF' as condition,
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '40[1-5]%' 
				  OR ICD9Code like '437.2%' THEN 204
				END AS comnum, 
			'CMB_HTN' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '437.3%'
				  OR ICD9Code like '44[01]%'
				  OR ICD9Code like '443.[1289]%'
				  OR ICD9Code like '447.1%'
				  OR ICD9Code like '557%'
				  OR ICD9Code like 'V43.4%' THEN 205
				END AS comnum, 
			'CMB_PAD' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '39[4-7]%'
				  OR ICD9Code like '424%'
				  OR ICD9Code like 'V42.2%'
				  OR ICD9Code like 'V43.3%' THEN 206
				END AS comnum, 
			'CMB_VHD' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '415%'
				  OR ICD9Code like '451.[12]%' THEN 207
				END AS comnum, 
			'CMB_DVT_PE' as condition,
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '55[56]%' THEN 208
				END AS comnum, 
			'CMB_IBD' as condition,
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '696.[01]%' THEN 209
				END AS comnum, 
			'CMB_PsA' as condition,
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '720.0%' THEN 210
				END AS comnum, 
			'CMB_AS' as condition,
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '714.[012489]%' THEN 211
				END AS comnum, 
			'CMB_RA' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '710.0%' THEN 212
				END AS comnum, 
			'CMB_SLE' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
	SELECT CASE WHEN ICD9Code like '710.1%' THEN 213
				END AS comnum, 
			'CMB_SSc' as condition,
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
  	SELECT CASE WHEN ICD9Code like '042%'
				  OR ICD9Code like 'V08%' THEN 214
				END AS comnum, 
			'CMB_HIV' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
  	SELECT CASE WHEN ICD9Code like '40[34]%'
				  OR ICD9Code like '58[23568]%'
				  OR ICD9Code like 'V42.0%'
				  OR ICD9Code like 'V45.1%'
				  OR ICD9Code like 'V56%' THEN 215
				END AS comnum, 
			'CMB_CKD' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION		
  	SELECT CASE WHEN ICD9Code like '49[12]%'
				  OR ICD9Code like '493.2%'
				  OR ICD9Code like '496%' THEN 216
				END AS comnum, 
			'CMB_COPD' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
  	SELECT CASE WHEN ICD9Code like '250%' THEN 217
				END AS comnum, 
			'CMB_DM' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
  	SELECT CASE WHEN ICD9Code like '272%' THEN 218
				END AS comnum, 
			'CMB_DysLipid' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
  	SELECT CASE WHEN ICD9Code like '327.23%' THEN 219
				END AS comnum, 
			'CMB_OSA' as condition,
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
  	SELECT CASE WHEN ICD9Code like '310.2%'
				  OR ICD9Code like '80[0-4]%'
				  OR ICD9Code like '85[0-4]%'
				  OR ICD9Code like '907.0%'
				  OR ICD9Code like 'V15.52%' THEN 220
				END AS comnum, 
			'CMB_TBI' as condition,
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
  	SELECT CASE WHEN ICD9Code like '296.[23]%'
				  OR ICD9Code like '311%' THEN 221
				END AS comnum, 
			'CMB_MDD' as condition,
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
  	SELECT CASE WHEN ICD9Code like '291%'
				  OR ICD9Code like '303%'
				  OR ICD9Code like '305.0%' THEN 222
				END AS comnum, 
			'CMB_ARD' as condition,	
			ICD9Code
				FROM CDWWork.Dim.ICD9
			UNION
  	SELECT CASE WHEN ICD9Code like '292%'
				  OR ICD9Code like '304%'
				  OR ICD9Code like '305.[2-9]%' THEN 223
				END AS comnum, 
			'CMB_SRD' as condition,
			ICD9Code
				FROM CDWWork.Dim.ICD9
				),
	dim9_ICDs2 as (
	SELECT comnum, condition,
	       REPLACE(ICD9Code, '.', '') as ICDCodeNoDecimal,
		   ICD9Code as ICDCode,
		   9 as ICDType
	FROM dim9_ICDs
	)
SELECT DISTINCT * 
INTO #dim9
FROM dim9_ICDs2
	WHERE comnum is not NULL
	ORDER BY comnum, ICDCode
GO

-- ICD10 codes
DROP TABLE IF EXISTS #dim10;
with dim10_ICDs as (
	SELECT CASE WHEN ICD10Code like'G43.7%' THEN 101
				END AS comnum,
			'CMB_ChronicMigraine' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
	SELECT CASE WHEN ICD10Code like'I2[12]%' THEN 102
				END AS comnum,
			'MI' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
	SELECT CASE WHEN ICD10Code like'I63%' THEN 103
				END AS comnum,
			'IschemicStroke' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
	SELECT CASE WHEN ICD10Code like'G45%' THEN 104
				END AS comnum,
			'TIA' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
	SELECT CASE WHEN ICD10Code like'I61%' THEN 105
				END AS comnum,
			'HemoStroke' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
	SELECT CASE WHEN ICD10Code like'I60%' THEN 106
				END AS comnum,
			'SAH' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'J09%' 
			      OR ICD10Code like 'J1[01]%' THEN 107
				END AS comnum,
			'Flu' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'C%'
					 AND ICD10Code not like 'C44%'
					 AND ICD10Code not like 'C4A%'
					 AND ICD10Code not like 'C7[789AB]%' THEN 108
				END AS comnum,
			'Any_malignancy' as condition,	
			ICD10Code
				FROM CDWWork.Dim.ICD10
		 UNION
  	SELECT CASE WHEN ICD10Code like 'S[0-9]2%' THEN 109
				END AS comnum,
			'Fracture' as condition,	
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
	SELECT CASE WHEN ICD10Code like'I48%' THEN 201
				END AS comnum,
			'CMB_AFibFlutter' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
	SELECT CASE WHEN ICD10Code like 'I4[23]%'
				  OR ICD10Code like 'I43%'
				  OR ICD10Code like 'I51.81%' THEN 202
				END AS comnum,
			'CMB_Cardiomyopathy' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'I09.81%'
				  OR ICD10Code like 'I11.0%'
				  OR ICD10Code like 'I13.0%'
				  OR ICD10Code like 'I13.2%'
				  OR ICD10Code like 'I50%' THEN 203
				END AS comnum,
			'CMB_HF' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'I1[0-6]%'
				  OR ICD10Code like 'I67.4%'
				  OR ICD10Code like 'N26.2%' THEN 204
				END AS comnum,
			'CMB_HTN' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'I70%'
				  OR ICD10Code like 'I71%'
				  OR ICD10Code like 'I73.[189]%'
				  OR ICD10Code like 'I77.1%'
				  OR ICD10Code like 'I79.[18]%'
				  OR ICD10Code like 'K55.[189]%'
				  OR ICD10Code like 'Z95.82%'
				  OR ICD10Code like 'Z95.9%' THEN 205
				END AS comnum,
			'CMB_PAD' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'I0[5-8]%'
				  OR ICD10Code like 'I09.1%'
				  OR ICD10Code like 'I3[4-9]%'
				  OR ICD10Code like 'Z95.[2-4]%' THEN 206
				END AS comnum,
			'CMB_VHD' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'I26%'
				  OR ICD10Code like 'I80.[1-3]%' THEN 207
				END AS comnum,
			'CMB_DVT_PE' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'K5[01]%' THEN 208
				END AS comnum,
			'CMB_IBD' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'L40.0%'
				  OR ICD10Code like 'L40.5%' THEN 209
				END AS comnum,
			'CMB_PsA' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'M45%' THEN 210
				END AS comnum,
			'CMB_AS' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'M05%'
				  OR ICD10Code like 'M06.[02389]%'
				  OR ICD10Code like 'M12.0%' THEN 211
				END AS comnum,
			'CMB_RA' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'M32%' THEN 212
				END AS comnum,
			'CMB_SLE' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'M34%' THEN 213
				END AS comnum,
			'CMB_SSc' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'B20%'
				OR ICD10Code like 'Z21%' THEN 214
				END AS comnum,
			'CMB_HIV' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'I1[23]%'
				  OR ICD10Code like 'N0[35]%'
				  OR ICD10Code like 'N1[89]%'
				  OR ICD10Code like 'N25%'
				  OR ICD10Code like 'Z49%'
				  OR ICD10Code like 'Z94.0%'
				  OR ICD10Code like 'Z99.2%' THEN 215
				END AS comnum,
			'CMB_CKD' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'J4[1-4]%' THEN 216
				END AS comnum,
			'CMB_COPD' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'E1[013]%' THEN 217
				END AS comnum,
			'CMB_DM' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'E78%' THEN 218
				END AS comnum,
			'CMB_DysLipid' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like '47.33%' THEN 219
				END AS comnum,
			'CMB_OSA' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'F07.81%'
				  OR ICD10Code like 'S04.04%'
				  OR ICD10Code like 'S06.[0-9]%'
				  OR ICD10Code like 'Z87.820%' THEN 220
				END AS comnum,
			'CMB_TBI' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'F32%'
				  OR ICD10Code like 'F33%'
				  OR ICD10Code like 'F34.1%' THEN 221
				END AS comnum,
			'CMB_MDD' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'F10%' THEN 222
				END AS comnum,
			'CMB_ARD' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
			UNION
  	SELECT CASE WHEN ICD10Code like 'F1[1-6]%'
				  OR ICD10Code like 'F18%'
				  OR ICD10Code like 'F19%' THEN 223
				END AS comnum,
			'CMB_SRD' as condition,			
			ICD10Code
				FROM CDWWork.Dim.ICD10
				),
	dim10_ICDs2 as (
	SELECT comnum, condition,
	       REPLACE(ICD10Code, '.', '') as ICDCodeNoDecimal,
		   ICD10Code as ICDCode,
		   10 as ICDType
	FROM dim10_ICDs
	)
SELECT DISTINCT * 
INTO #dim10
FROM dim10_ICDs2
	WHERE comnum is not NULL
	ORDER BY comnum, ICDCode
GO
-- (21044 rows affected)


-- merge ICD-9/10
DROP TABLE IF EXISTS dflt.dim_ICD910;
SELECT * 
INTO dflt.dim_ICD910
	FROM #dim9
UNION
SELECT * FROM #dim10
GO



/*************************************************************************/
/******************************** RX *************************************/
/*************************************************************************/

DROP TABLE IF EXISTS #Rx;
with RX0 as(
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%DABIGATRAN%'
				or DrugNameWithDose like '%APIXABAN%'
				or DrugNameWithDose like '%BETRIXABAN%'
				or DrugNameWithDose like '%EDOXABAN%'
				or DrugNameWithDose like '%RIVAROXABAN%' THEN 'RX_Anticoagulant_DOAC'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%WARFARIN%' THEN 'RX_Anticoagulant_VKA'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%BENAZEPRIL%'
				or DrugNameWithDose like '%CAPTOPRIL%'
				or DrugNameWithDose like '%ENALAPRIL%'	
				or DrugNameWithDose like '%FOSINOPRIL%'				
				or DrugNameWithDose like '%MOEXIPRIL%'
				or DrugNameWithDose like '%PERINDOPRIL%'
				or DrugNameWithDose like '%QUINAPRIL%'
				or DrugNameWithDose like '%RAMIPRIL%'
				or DrugNameWithDose like '%TRANDOLAPRIL%'

				or DrugNameWithDose like '%AZILSARTAN%'
				or DrugNameWithDose like '%EPROSARTAN%'
				or DrugNameWithDose like '%IRBESARTAN%'
				or DrugNameWithDose like '%LOSARTAN%'
				or DrugNameWithDose like '%OLMESARTAN%'
				or DrugNameWithDose like '%TELMISARTAN%'
				or DrugNameWithDose like '%VALSARTAN%' THEN 'RX_other_ACEIs_ARBs' 
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ALFUZOSIN%'
				or DrugNameWithDose like '%DOXAZOSIN%'
				or DrugNameWithDose like '%PRAZOSIN%' 
				or DrugNameWithDose like '%SILODOSIN%' 
				or DrugNameWithDose like '%TAMSULOSIN%' 
				or DrugNameWithDose like '%TERAZOSIN%' THEN 'RX_alpha_blocker'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ACEBUTOLOL%'
				or DrugNameWithDose like '%ATENOLOL%'
				or DrugNameWithDose like '%BISOPROLOL%'
				or DrugNameWithDose like '%CARVEDILOL%'
				or DrugNameWithDose like '%LABETALOL%'
				or DrugNameWithDose like '%METOPROLOL%'
				or DrugNameWithDose like '%NADOLOL%'
				or DrugNameWithDose like '%NEBIVOLOL%'
				or DrugNameWithDose like '%PINDOLOL%'
				or DrugNameWithDose like '%PROPRANOLOL%'
				or DrugNameWithDose like '%SOTALOL%' 
				or DrugNameWithDose like '%TIMOLOL%' THEN 'RX_betaBlocker'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%AMLODIPINE%' 
				or DrugNameWithDose like '%DILTIAZEM%'
				or DrugNameWithDose like '%FELODIPINE%'
				or DrugNameWithDose like '%ISRADIPINE%'
				or DrugNameWithDose like '%LEVAMLODIPINE%'
				or DrugNameWithDose like '%NICARDIPINE%'
				or DrugNameWithDose like '%NIFEDIPINE%'
				or DrugNameWithDose like '%NISOLDIPINE%'
				or DrugNameWithDose like '%VERAPAMIL%' THEN 'RX_CCB'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like 'CLONIDINE%'
				or DrugNameWithDose like '%/CLONIDINE%' 
				or DrugNameWithDose like '%GUANFACINE%' 
				or DrugNameWithDose like '%METHYLDOPA%'
				or DrugNameWithDose like '%RESERPINE%'  THEN 'RX_Central_sympathetic'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ALISKIREN%' THEN 'RX_Renin_blocker'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%BUMETANIDE%'
				or DrugNameWithDose like '%FUROSEMIDE%'
				or DrugNameWithDose like '%TORSEMIDE%' THEN 'RX_Diuretics_loop'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%AMILORIDE%'
				or DrugNameWithDose like '%EPLERENONE%' 
				or DrugNameWithDose like '%SPIRONOLACTONE%'
				or DrugNameWithDose like '%TRIAMTERENE%'  THEN 'RX_Diuretic_potassium'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%BENDROFLUMETHIAZIDE%'
				or DrugNameWithDose like '%CHLORTHALIDONE%'
				or DrugNameWithDose like '%CHLOROTHIAZIDE%'
				or DrugNameWithDose like '%HYDROCHLOROTHIAZIDE%'
				or DrugNameWithDose like '%HYDROFLUMETHIAZIDE%'
				or DrugNameWithDose like '%INDAPAMIDE%'
				or DrugNameWithDose like '%METOLAZONE%'
				or DrugNameWithDose like '%POLYTHIAZIDE%' THEN 'RX_Diuretic_TZD'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ISOSORBIDE DINITRATE%'
				or DrugNameWithDose like '%ISOSORBIDE MONONITRATE%' THEN 'RX_Nitrates'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%HYDRALAZINE%'
				or DrugNameWithDose like '%MINOXIDIL%' THEN 'RX_Vasodiator'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ASPIRIN 75MG%'
				or DrugNameWithDose like '%ASPIRIN 81MG%'
				or DrugNameWithDose like '%ASPIRIN 65MG%'
				or DrugNameWithDose like '%ASA 81%' THEN 'RX_aPLT_Aspirin'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%CLOPIDOGREL%'
				or DrugNameWithDose like '%DIPYRIDAMOLE%'
				or DrugNameWithDose like '%PRASUGREL%'
				or DrugNameWithDose like '%TICAGRELOR%'
				or DrugNameWithDose like '%TICLOPIDINE%' THEN 'RX_aPLT_other'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%AMISULPRIDE%'
				or DrugNameWithDose like '%ARIPIPRAZOLE%'
				or DrugNameWithDose like '%ASENAPINE%'
				or DrugNameWithDose like '%CLOZAPINE%'
				or DrugNameWithDose like '%ILOPERIDONE%'
				or DrugNameWithDose like '%LURASIDONE%'
				or DrugNameWithDose like '%OLANZAPINE%'
				or DrugNameWithDose like '%PALIPERIDONE%'
				or DrugNameWithDose like '%QUETIAPINE%'
				or DrugNameWithDose like '%RISPERIDONE%'
				or DrugNameWithDose like '%ZIPRASIDONE%' THEN 'RX_aPsyc_atpc'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%CHLORPROMAZINE%'
				or DrugNameWithDose like '%FLUPHENAZINE%'
				or DrugNameWithDose like '%HALOPERIDOL%'
				or DrugNameWithDose like '%LOXAPINE%'
				or DrugNameWithDose like '%MESORIDAZINE%'
				or DrugNameWithDose like '%MOLINDONE%'
				or DrugNameWithDose like '%PERPHENAZINE%'
				or DrugNameWithDose like '%PIMOZIDE%'
				or DrugNameWithDose like '%PROCHLORPERAZINE%'
				or DrugNameWithDose like '%THIORIDAZINE%'
				or DrugNameWithDose like '%THIOTHIXENE%' THEN 'RX_aPsyc_tpc'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ALBIGLUTIDE%'
				or DrugNameWithDose like '%DULAGLUTIDE%'
				or DrugNameWithDose like '%EXENATIDE%'
				or DrugNameWithDose like '%LIRAGLUTIDE%'
				or DrugNameWithDose like '%LIXISENATIDE%'
				or DrugNameWithDose like '%SEMAGLUTIDE%'
				or DrugNameWithDose like '%TIRZEPATIDE%' THEN 'RX_GLP1'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ATORVASTATIN%'
				or DrugNameWithDose like '%FLUVASTATIN%' 
				or DrugNameWithDose like '%LOVASTATIN%'
				or DrugNameWithDose like '%PITAVASTATIN%'
				or DrugNameWithDose like '%PRAVASTATIN%'
				or DrugNameWithDose like '%ROSUVASTATIN%'
				or DrugNameWithDose like '%SIMVASTATIN%' THEN 'RX_lipid_statin'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%FENOFIBRATE%'
				or DrugNameWithDose like '%GEMFIBROZIL%' THEN 'RX_lipid_fibrate'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%CHOLESTYRAMINE%' 
				or DrugNameWithDose like '%COLESEVELAM%'
				or DrugNameWithDose like '%COLESTIPOL%' THEN 'RX_lipid_bileacid'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ALIROCUMAB%' 
				or DrugNameWithDose like '%BEMPEDOIC ACID%'
				or DrugNameWithDose like '%EVINACUMAB%'
				or DrugNameWithDose like '%EVOLOCUMAB%'
				or DrugNameWithDose like '%EZETIMIBE%'
				or DrugNameWithDose like '%INCLISIRAN%'
				or DrugNameWithDose like '%OMEGA-3%' THEN 'RX_lipid_other'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ASPIRIN%' 
				or DrugNameWithDose like '%CELECOXIB%'
				or DrugNameWithDose like '%DICLOFENAC%'
				or DrugNameWithDose like '%DIFLUNISAL%'
				or DrugNameWithDose like '%ETODOLAC%'
				or DrugNameWithDose like '%FENOPROFEN%'
				or DrugNameWithDose like '%FLURBIPROFEN%'
				or DrugNameWithDose like '%IBUPROFEN%'
				or DrugNameWithDose like '%INDOMETHACIN%'
				or DrugNameWithDose like '%KETOPROFEN%'
				or DrugNameWithDose like '%KETOROLAC%'
				or DrugNameWithDose like '%MECLOFENAMATE%'
				or DrugNameWithDose like '%MEFENAMIC ACID%'
				or DrugNameWithDose like '%MELOXICAM%'
				or DrugNameWithDose like '%NABUMETONE%'
				or DrugNameWithDose like '%NAPROXEN%'
				or DrugNameWithDose like '%OXAPROZIN%'
				or DrugNameWithDose like '%PHENYLBUTAZONE%'
				or DrugNameWithDose like '%PIROXICAM%'
				or DrugNameWithDose like '%ROFECOXIB%'
				or DrugNameWithDose like '%SULINDAC%'
				or DrugNameWithDose like '%SUPROFEN%'
				or DrugNameWithDose like '%TOLMETIN%'
				or DrugNameWithDose like '%VALDECOXIB%' THEN 'RX_NSAID'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%DESOGESTREL%'
				or DrugNameWithDose like '%DROSPIRENONE%'
				or DrugNameWithDose like '%ESTRADIOL%'
				or DrugNameWithDose like '%ESTROGENS%'			
				or DrugNameWithDose like '%ESTRONE%'
				or DrugNameWithDose like '%ETHINYL ESTRADIOL%'
				or DrugNameWithDose like '%LEVONORGESTREL%'
				or DrugNameWithDose like '%MEDROXYPROGESTERONE%'
				or DrugNameWithDose like '%MEGESTROL%'
				or DrugNameWithDose like '%NORELGESTROMIN%'
				or DrugNameWithDose like '%NORETHINDRONE%'
				or DrugNameWithDose like '%NORGESTIMATE%'
				or DrugNameWithDose like '%NORGESTREL%'
				or DrugNameWithDose like '%TAMOXIFEN%' THEN 'RX_HRT'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%BEXAGLIFLOZIN%'
				or DrugNameWithDose like '%CANAGLIFLOZIN%'
				or DrugNameWithDose like '%DAPAGLIFLOZIN%'
				or DrugNameWithDose like '%EMPAGLIFLOZIN%'
				or DrugNameWithDose like '%ERTUGLIFLOZIN%'
				or DrugNameWithDose like '%SOTAGLIFLOZIN%' THEN 'RX_SGLT2'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like 'RIMEGEPANT%'
				or DrugNameWithDose like '%UBROGEPANT%'
				or DrugNameWithDose like '%ZAVEGEPANT%' THEN 'RX_Mgr_aCGRP_acute'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ALMOTRIPTAN%'
				or DrugNameWithDose like '%ELETRIPTAN%'
				or DrugNameWithDose like '%FROVATRIPTAN%'
				or DrugNameWithDose like '%NARATRIPTAN%'
				or DrugNameWithDose like '%RIZATRIPTAN%'
				or DrugNameWithDose like '%SUMATRIPTAN%'
				or DrugNameWithDose like '%ZOLMITRIPTAN%' THEN 'RX_Mgr_triptan'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ERGOTAMINE%'
				or DrugNameWithDose like '%DIHYDROERGOTAMINE%' THEN 'RX_Mgr_ergotamine'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ERENUMAB-AOOE%'
				or DrugNameWithDose like '%FREMANEZUMAB-VFRM%'
				or DrugNameWithDose like '%GALCANEZUMAB-GNLM%'
				or DrugNameWithDose like '%RIMEGEPANT%'
				or DrugNameWithDose like '%ATOGEPANT%' THEN 'RX_Mgr_aCGRP'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like 'Topiramate%' THEN 'RX_Mgr_TPM'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN (DrugNameWithDose like '%LISINOPRIL%' or DrugNameWithDose like '%CANDESARTAN%')
				AND DrugNameWithDose not like '%HYDROCHLOROTHIAZIDE%' THEN 'RX_Mgr_ACEI_ARB' 
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%BENAZEPRIL%'
				or DrugNameWithDose like '%CAPTOPRIL%'
				or DrugNameWithDose like '%ENALAPRIL%'	
				or DrugNameWithDose like '%FOSINOPRIL%'				
				or DrugNameWithDose like '%LISINOPRIL%'
				or DrugNameWithDose like '%MOEXIPRIL%'
				or DrugNameWithDose like '%PERINDOPRIL%'
				or DrugNameWithDose like '%QUINAPRIL%'
				or DrugNameWithDose like '%RAMIPRIL%'
				or DrugNameWithDose like '%TRANDOLAPRIL%'

				or DrugNameWithDose like '%AZILSARTAN%'
				or DrugNameWithDose like '%CANDESARTAN%'
				or DrugNameWithDose like '%EPROSARTAN%'
				or DrugNameWithDose like '%IRBESARTAN%'
				or DrugNameWithDose like '%LOSARTAN%'
				or DrugNameWithDose like '%OLMESARTAN%'
				or DrugNameWithDose like '%TELMISARTAN%'
				or DrugNameWithDose like '%VALSARTAN%' THEN 'RX_Mgr_ACEIs_ARBs' 
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%LAMOTRIGINE%'
				or DrugNameWithDose like '%PREGABALIN%'
				or DrugNameWithDose like '%VALPROATE%'
				or DrugNameWithDose like '%VALPROIC%' THEN 'RX_Mgr_otherAnticonvulsant'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%ABOBOTULINUMTOXINA%'
				or DrugNameWithDose like '%INCOBOTULINUMTOXINA%'
				or DrugNameWithDose like '%ONABOTULINUMTOXINA%' THEN 'RX_Mgr_neurotoxin'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%memantine%' THEN 'RX_Mgr_NMDA'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like '%CLONIDINE%' THEN 'RX_Mgr_clonidine'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like 'VENLAFAXINE%' THEN 'RX_Mgr_SNRI'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	UNION
	SELECT DISTINCT
		CASE WHEN DrugNameWithDose like 'AMITRIPTYLINE%'
				or DrugNameWithDose like '%NORTRIPTYLINE%' THEN 'RX_Mgr_TCA'
			END AS RX_Cat, DrugNameWithDose, NationalDrugSID, DosageFormSID, DrugNameWithoutDoseSID
			FROM [CDWWork].[Dim].[NationalDrug]
	)
SELECT DISTINCT a.NationalDrugSID, a.RX_Cat, b.DrugNameWithoutDose, a.DrugNameWithDose, c.DosageForm
INTO #Rx
FROM RX0 AS a LEFT JOIN CDWWork.Dim.DrugNameWithoutDose as b on a.DrugNameWithoutDoseSID = b.DrugNameWithoutDoseSID
			  LEFT JOIN CDWWork.Dim.DosageForm as c on a.DosageFormSID = c.DosageFormSID
WHERE RX_Cat IS NOT NULL
GO

/* Note:

	HRTs included topical gels
	lipid-modifying agent such as Alirocumab and evinacumab are injections

	Exclusion:
		warfarin bracelets
		timolol eye drops
		any dosage forms for injection/topical
*/

DROP TABLE IF EXISTS dflt.dim_rx;
SELECT *
INTO dflt.dim_rx
from #Rx
where (RX_Cat in ('RX_Mgr_neurotoxin', 'RX_GLP1', 'RX_HRT', 'RX_lipid_other', 'RX_Mgr_aCGRP'))
	OR (RX_Cat not in ('RX_Mgr_neurotoxin', 'RX_GLP1', 'RX_HRT', 'RX_lipid_other')							
			AND DosageForm not in ('INJ,SOLN', 'MISCELLANEOUS', 'INJ,CONC, W/BUF', 'INJ', 'INJ,SUSP,SA',
									'INJ,PWDR', 'SOLN,OPH', 'GEL,OPH', 'OPH IRR', 
									'SOLN,TOP', 'FOAM,TOP', 'GEL,TOP', 'CREAM,TOP'))
