    !ESSA ROTINA CRIA O INDICE DE DATAS (DIA,MES,ANO,HORA)A SSOCIADAS AO CONTADOR DE TEMPO DE 1..NT
    SUBROUTINE INIDATAS
    USE VARS_MAIN
    IMPLICIT NONE
    
    !variaveis globais
    !integer:: NT
    !integer,allocatable:: anoh(:),diah(:),mesh(:),horah(:)

    !variaveis locais
    integer :: i,k
    integer :: horax,diax,mesx,anox
    integer :: ndiasmes
    
    !allocate(anoh(nt),diah(nt),horah(nt),mesh(nt))
    
    !Data inicial - ler do parhig
    !teste
    !horax=8
    !diax=1
    !mesx=1
    !anox=2013
    !nt=3000

    !modelo
    horax=horaini
    diax=idia
    mesx=imes
    anox=iano  
    
    !Atribui indices
    DO i=1,NT
        k=i
        
        ANOH(k)=anox
        DIAH(k)=diax
        HORAH(k)=horax
        MESH(k)=mesx
        
        horax=horax+1
        if (horax==24) then
            horax=0
            diax=diax+1
        end if
        
        if (diax>ndiasmes(mesx,anox)) then
            diax=1
            mesx=mesx+1
        end if
        
        if (mesx==13) then 
            mesx=1
            anox=anox+1
        end if
        
    
    END DO

    !do i=1,nt
     !   write(*,*)i,diah(i),mesh(i),anoh(i),horah(i)
!        write(1,*)i,diah(i),mesh(i),anoh(i),horah(i)
    !end do
!    
    !pause


    END SUBROUTINE

    !----------------------------------------------------------
    !FUNCAO AUXILIAR PARA CALCULAR NUMERO DE DIAS DO MES/ANO
        
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
			    NDIAS=28 !NAO É ANO BISSEXTO
		    ELSEIF (MOD(ANO,4)==0) THEN
			    NDIAS=29 !ANO BISSEXTO
		    ELSE
			    NDIAS=28 !NAO É ANO BISSEXTO
		    END IF
	    CASE(0)
		    STOP
    CASE DEFAULT
            NDIAS=0
    END SELECT

    NDIASMES=NDIAS

    RETURN
    END FUNCTION    