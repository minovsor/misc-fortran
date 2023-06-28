FUNCTION NDIASMES(MES,ANO)
IMPLICIT NONE
INTEGER NDIASMES
INTEGER MES
INTEGER ANO
INTEGER NDIAS


SELECT CASE(MES)
	CASE(1,3,5,7,8,10,12)!SE O MES FOR JAN,MAR,MAI,JUL,AGO,OUT,DEZ ENTAO O MES TEM 31 DIAS
		NDIAS=31
	CASE(4,6,9,11)
		NDIAS=30
	CASE(2)
		!WRITE(*,*)'FEVEREIRO PODE TER 28 OU 29 DIAS'
		IF(MOD(ANO,400)==0)THEN
			NDIAS=29 !ANO BISSEXTO
		ELSEIF (MOD(ANO,100)==0) THEN
			NDIAS=28 !NAO � ANO BISSEXTO
		ELSEIF (MOD(ANO,4)==0) THEN
			NDIAS=29 !ANO BISSEXTO
		ELSE
			NDIAS=28 !NAO � ANO BISSEXTO
		END IF
	CASE(0)
		STOP
CASE DEFAULT
        NDIAS=0
END SELECT

NDIASMES=NDIAS

RETURN
END FUNCTION