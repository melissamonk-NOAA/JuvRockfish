/****** SQL code for looking a juvenile rockfish data
        Melissa Monk, NMFS SWFSC
		
 ******/

 --Number of individual adults for species
SELECT distinct species_codes.SPECIES, common_name, MATURITY, sum(total_no) as sum_total
  FROM [juv_rockfish].[dbo].[JUV_CATCH]
  inner join species_codes on juv_catch.species=species_codes.species
  where maturity like 'A'
  group by species_codes.SPECIES, common_name, MATURITY
  order by sum_total desc


--Pull all juvenile rockfish catch data
--Catch data
Select JUV_HAUL.CRUISE,
	   JUV_HAUL.HAUL_NO,
	   JUV_CATCH.SPECIES,
	   SPECIES_CODES.COMMON_NAME,
	   MATURITY,
	   JUV_CATCH.TOTAL_NO
 from JUV_HAUL
			right join	JUV_CATCH on JUV_HAUL.CRUISE=JUV_CATCH.CRUISE and JUV_HAUL.HAUL_NO=JUV_CATCH.HAUL_NO
            inner join 	SPECIES_CODES on JUV_CATCH.SPECIES=SPECIES_CODES.SPECIES
where TOTAL_NO is not null
   and MATURITY not like 'A'
   and PROBLEM not in (4,5,6,7,8)
   and not (PROBLEM=0 and STANDARD_STATION=0)
