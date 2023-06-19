	SUBROUTINE LESECAO
	!#------------------------------------------------------------------------------------
	! Autor: Mino V. Sorribas
	!
	!=====================================================================================
	! Data			Observacoes
	! 05.05.2010	Primeira versao - Faz leitura de dados e interpola
	!
	!-------------------------------------------------------------------------------------
	! Subrotina para:
	! -ler dados de secao transversal em arquivo
	!	(nivel,largura da superficie,area molhada,perimetro molhado,raio hidraulico,prof.media
	! -interpolar dados para nivel especificado
	!
	!-------------------------------------------------------------------------------------
	! Chamada na rotina:
	! -SECAO.F90
	!
	!-------------------------------------------------------------------------------------
	! Descrição das variáveis locais:
	!   IERROR: Flag para verificar existência do arquivo. 0=existe (padrao)
	!   NDADOS: numero de dados da tabela de entrada
	!	I: contador
	!	POS: posicao de referencia para interpolacao
	!   HTEMP,BTEMP,ATEMP,PTEMP,RTEMP,PROFTEMP: auxiliares para contar dados na tabela
	!   HT,BT,AT,PT,RT,PROFT: vetores que armazenam dados da tabela
	!   HMIN,HMAX: nivel minimo e maximo (limites da tabela)
	!   H,B,A,P,R,PROF: nivel de interesse (arg. de entrada), saidas de interesse (arg. de saida)
	!   DH: auxiliar para interpolacao
	!
	!-------------------------------------------------------------------------------------
	! Informacoes necessárias para utilizar a rotina:
	! -Arquivo com dados de nivel,largura da superficie,area molhada,perimetro molhado,raio hidraulico,prof.media
	! -Nivel de interesse.
	!-------------------------------------------------------------------------------------
	! Observacoes pertinentes:
	! -A rotina nao extrapola. Se estiver fora dos limites da tabela, encerra.
	! -Considera que a tabela de entrada esta ordenada de forma crescente
	!
	!-------------------------------------------------------------------------------------
	IMPLICIT NONE
	INTEGER:: IERROR
	INTEGER:: NDADOS,I,POS
	REAL::HTEMP,BTEMP,ATEMP,PTEMP,RTEMP,PROFTEMP
	REAL,ALLOCATABLE::HT(:),BT(:),AT(:),PT(:),RT(:),PROFT(:)
	REAL::HMIN,HMAX
	REAL::H,B,A,P,R,PROF
	REAL::DH

	!INICIA VARIAVEIS
	IERROR=0
	NDADOS=0
	I=0
	HTEMP=0.
	BTEMP=0.
	ATEMP=0.
	RTEMP=0.
	PROFTEMP=0.
	HT=0.
	BT=0.
	AT=0.
	PT=0.
	RT=0.
	PROFT=0.

	!ABERTURA DO ARQUIVO COM TABELA DE DADOS DE SECAO
	OPEN(UNIT=3,FILE='SECAO.SAI',STATUS='OLD',ACTION='READ',IOSTAT=IERROR)
	IF (IERROR/=0) THEN
    		WRITE(*,*)'-------------------------------------------'
			WRITE(*,*)'ERRO AO ABRIR O ARQUIVO DE SECAO: SECAO.SAI'
    		WRITE(*,*)'-------------------------------------------'            
        STOP
	END IF
	
	!DIMENSIONA VETORES DE DADOS DE TABELA
	READ(3,*)				!LE O CABECALHO
	NDADOS=0
	DO WHILE (.NOT.EOF(3))
		READ(3,*)HTEMP,BTEMP,ATEMP,PTEMP,RTEMP,PROFTEMP
		NDADOS=NDADOS+1
	END DO
	
	ALLOCATE(HT(NDADOS),BT(NDADOS),AT(NDADOS),PT(NDADOS),RT(NDADOS),PROFT(NDADOS))

	!GRAVA DADOS DO ARQUIVO NOS VETORES
	REWIND(3)
	READ(3,*)				!LE O CABECALHO
	DO I=1,NDADOS
		READ(3,*)HT(I),BT(I),AT(I),PT(I),RT(I),PROFT(I)
	END DO
	
	!MOSTRA DADOS ARMAZENADOS
	WRITE(*,*)
	WRITE(*,*) 'DADOS DA SECAO TRANSVERSAL (SECAO.SAI)'
	WRITE(*,*) '     NIVEL   LARGURA      AREA PERIMETRO    RAIO_H  PROF_MED'
	DO I=1,NDADOS
		WRITE(*,'(6f8.2)')HT(I),BT(I),AT(I),PT(I),RT(I),PROFT(I)
	END DO
	
	!AQUI COMECA OS PASSOS PARA INTERPOLACAO DE DADOS
	!NIVEL DE INTERESSE
	WRITE(*,*)
	WRITE(*,*) 'ENTRE O NIVEL DESEJADO'
	READ(*,*)H
	!H=5.52
	
	!TESTA SE O NIVEL ESTA DENTRO DOS LIMITES DA TABELA
	HMIN=MINVAL(HT)
	HMAX=MAXVAL(HT)
	IF ((H<HMIN).OR.(H>HMAX)) THEN
		WRITE(*,*)
		WRITE(*,*)'NIVEL FORA DOS LIMITES DA TABELA'
		STOP
	END IF
	
	!TESTA SE H É O MAXIMO DA TABELA
	IF (H==HMAX) THEN
		B = BT(NDADOS)
		A = AT(NDADOS)
		P = PT(NDADOS)
		R = RT(NDADOS)
		PROF = PROFT(NDADOS)
			
	ELSE
	
		!VARRE A TABELA ATÉ ENCONTRAR A POSICAO DO VALOR MAIOR QUE H DESEJADO
		DO POS=2,NDADOS
			IF (HT(POS)>H) THEN
			EXIT
			END IF
		END DO
	
		!REALIZA INTERPOLACAO
		DH = HT(POS) - HT(POS-1)
		B = BT(POS-1) + (BT(POS) - BT(POS-1))*(H - HT(POS-1))/DH
		A = AT(POS-1) + (AT(POS) - AT(POS-1))*(H - HT(POS-1))/DH
		P = PT(POS-1) + (PT(POS) - PT(POS-1))*(H - HT(POS-1))/DH
		R = RT(POS-1) + (RT(POS) - RT(POS-1))*(H - HT(POS-1))/DH
		PROF = PROFT(POS-1) + (PROFT(POS) - PROFT(POS-1))*(H - HT(POS-1))/DH
	
	END IF

	DEALLOCATE(HT,BT,AT,PT,RT,PROFT)
	
	WRITE(*,'(A40,F6.2)') 'DADOS DA SECAO TRANSVERSAL PARA A H= ',H
	WRITE(*,'(6A12)')'NIVEL','LARGURA','AREA','PERIMETRO','RAIO_H','PROF_MED'
	WRITE(*,'(6F12.2)')H,B,A,P,R,PROF
	
	END SUBROUTINE
	
	