    !PROGRAMA PARA LER OS ARQUIVOS .TXT DO FORMATO INMET-BDMEP
    !MINO VIANA SORRIBAS JUL/2012
    !INSTITUTO DE PESQUISAS HIDRAULICAS
    ! 1)CONFIGURAR NUMERO DE POSTOS E NOME DOS ARQUIVOS DE ENTRADA EM LEBDMEP.TXT
    ! 2)SAIDAS EM ARQUIVOS FORT. DE ACORDO COM SEQUENCIA
    ! 3)PARA ALTERAR O PERIODO DE DADOS ALTERAR AS VARIAVEIS anomin,anomax, etc mais abaixo.
    !    EXEMPLO DE CABECALHO DO ARQUIVO
    !    --------------------
    !    BDMEP - INMET
    !    --------------------
    !    Estação           : CRUZ ALTA - RS (OMM: 83912)
    !    Latitude  (graus) : -28.63
    !    Longitude (graus) : -53.6
    !    Altitude  (metros): 472.50
    !    Estação Operante
    !    Inicio de operação: 02/02/1912
    !    Periodo solicitado dos dados: 01/01/1961 a 19/09/2012
    !    Os dados listados abaixo são os que encontram-se digitados no BDMEP
    !    --------------------
    !    Obs.: Os dados aparecem separados por ; (ponto e vírgula) no formato txt.
    !          Para o formato planilha XLS, siga as instruções
    !    --------------------
    !    Estacao;Data;Hora;Precipitacao;TempBulboSeco;TempBulboUmido;TempMaxima;TempMinima;UmidadeRelativa;PressaoAtmEstacao;DirecaoVento;VelocidadeVento;Insolacao;
    
    program LeBDMEP
    implicit none

    !------------Variaveis---------------------------------
    integer:: iFileMain,iFileDados,iFileOutput    
    character:: header                                      !strings leituras de lixo nos arquivos
    character(200)::header_big
    character(50), allocatable:: ArqPosto(:)                !nomes de arquivo dos postos com dados
    
    real rDadosAux
    real,allocatable:: rDados(:,:,:,:,:,:)                   ![ano,mes,dia,hora,posto,nvar]
    
    integer:: ip,np                                         !contador de posto; numero de postos
    integer:: iDia, iMes, iAno, iHora, iQuinze
    integer:: ndiasmes !retorna da function
    integer:: ndias !variavel

    integer,parameter:: npmax=1000
    integer,parameter:: anomin=1960,anomax=2013
    integer,parameter:: mesmin=1,mesmax=12
    integer,parameter:: diamin=1,diamax=31
    integer,parameter:: horamin=0,horamax=23
    
    real,parameter:: rfalha=-99.
    
    integer :: ivar
    integer, parameter :: nvar=10
    character(4) :: textvar(nvar)
   
    character(2)::strDD,strMM
    character(1)::strSlash
    character(4)::strAAAA
    character(4)::strPrefix
    character(50)::strHeaderOutput
    
    logical::EhFalha
    
    !linhas para leitura de cadeia de caracteres
    integer,parameter::field_max_size=20                            !numero de caracteres maximos por campo
    integer,parameter::field_max_number=13                          !numero de campos no arquivo 
    character(field_max_size),dimension(field_max_number):: field   !
    character(field_max_size):: string,string_old
    character(1),dimension(100):: c                                 !100 caracteres de cada linha 

    integer iVirg
    integer i,k,j,kmax

   
    !Separador
    iVirg = iachar(",")          !Armazena o inteiro da tabela ASCii associado à virgula
    iVirg = iachar(";")          !Armazena o inteiro da tabela ASCii associado ao ponto e virgula
    !write(*,*)iVirg,char(iVirg)
    c(:)="" !Inicializa string

    !Texto dos campos de variaveis
    textvar(1)='PREC'
    textvar(2)='TSEC'
    textvar(3)='TUMI'
    textvar(4)='TMAX'
    textvar(5)='TMIN'
    textvar(6)='UREL'
    textvar(7)='PATM'
    textvar(8)='DIRV'
    textvar(9)='VENT'
    textvar(10)='INSO'

    !Atribuicao de Referencia para Arquivos
    iFileMain=1
    iFileDados=2
    iFileOutput=3

    !Abre e Le o arquivo de entrada
    OPEN(iFileMain,FILE='Lebdmep.txt',STATUS='OLD',ACTION='READ')
    READ(iFileMain,*) header
    READ(iFileMain,*) np            !numero de postos 
    allocate(ArqPosto(np))
    READ(iFileMain,*) header 
    do ip=1,np	                    !lista de arquivos com dados de clima no formato BDMEP txt
	    read(iFileMain,*)ArqPosto(ip)
	    write(*,*)ArqPosto(ip)
    end do

   
    WRITE(*,*)'Leu o arquivo de entrada: LeBDMEP.txt'
    close(1)

    !Inicializa vetores de dados    
    allocate(rDados(anomin:anomax,mesmin:mesmax,diamin:diamax,horamin:horamax,np,nvar))
        
    rDados=rFalha

    !i Leitura dos Dados
    Do IP=1,NP
    	
	    write(*,*)'Lendo dados do posto:'
	    write(*,*)IP,'   ',ArqPosto(IP)
    	
	    !Abre o arquivo e le os dados
	    open(iFileDados,FILE=ArqPosto(IP),STATUS='OLD')
	    !Le Cabecalho
	    read(iFileDados,*)header    !--------------------
	    read(iFileDados,*)header    !BDMEP - INMET
	    read(iFileDados,*)header    !-------------------- 
        read(iFileDados,*)header    !Estação           : CRUZ ALTA - RS (OMM: 83912)
        read(iFileDados,*)header    !Latitude  (graus) : -28.63
        read(iFileDados,*)header    !Longitude (graus) : -53.6
        read(iFileDados,*)header    !Altitude  (metros): 472.50
        read(iFileDados,*)header     !Estação Operante
        read(iFileDados,*)header    !Inicio de operação: 02/02/1912
        read(iFileDados,*)header    !Periodo solicitado dos dados: 01/01/1961 a 19/09/2012
        read(iFileDados,*)header    !Os dados listados abaixo são os que encontram-se digitados no BDMEP
        read(iFileDados,*)header    !--------------------
        read(iFileDados,*)header    !Obs.: Os dados aparecem separados por ; (ponto e vírgula) no formato txt.
        read(iFileDados,*)header    !      Para o formato planilha XLS, siga as instruções
        read(iFileDados,*)header    !--------------------
        read(iFileDados,*)header    !Estacao;Data;Hora;Precipitacao;TempBulboSeco;TempBulboUmido;TempMaxima;TempMinima;UmidadeRelativa;PressaoAtmEstacao;DirecaoVento;VelocidadeVento;Insolacao;	    
	    write(*,*)header
	    
	    !Varre linhas
	    do while (.not.(eof(iFileDados)))
	        
	        !Le a linha em string
            read(iFileDados,'(100a1)') (c(i),i=1,100) 
            k=1
            string=""
            string_old=''
            field(:)=''
            
            !Varre a cadeia de caracteres
            do i=1,100 
              !write(*,*)i,c(i)                  
              if ( iachar(c(i))==iVirg ) then !encontrou o separador, armazena string
                        
                field(k)=trim(string)    
                k=k+1
                string=""    
                
                else        !concatena
                    
                    string_old=trim(string)        
                    string=trim(string_old)//trim(c(i))

              end if
              !#verificacao              
              !write(*,*)i,k !mostra o indice do caracter e campo atual
              !write(*,'(a20,2x,a20)')trim(string_old),trim(string)
              
            end do !cadeia de caracteres

            !mostra na tela os campos lidos
            do i=1,field_max_number
               write(*,'(i4,a20)') i,trim(field(i)) !,len(field(i))
            end do

            
            !Converte string para o formato dos campos correspondentes
            
            !Codigo
            string=field(1)
            
            !Data
            string=field(2)
            read(string,'(i2,1X,i2,1X,i4)')iDia,iMes,iAno
            !write(*,*)iDia,iMes,iAno !echo
            
            !Hora
            string=field(3)
            read(string,'(i2)')iHora            
            !write(*,*)iHora !echo
            
            !Variaveis de estados     
            do ivar=1,10
                k=ivar+3           !desloca coluna dos campos lidos
                string=field(k)
                if (string/='') then                
                    read(string,*)rDados(iAno,iMes,iDia,iHora,ip,ivar)                                                 
                end if        
            end do
            !write(*,'(<nvar>f12.2)')rDados(iAno,iMes,iDia,iHora,ip,:)
           ! pause


       end do !linha
	   close(iFileDados)
	END Do   !arquivo 
	
	!grava em arquivos sequenciais fort.ip
	DO IP=1,NP
	    write(ip,'(2a4,a6,a4,<nvar>a12)')'DD','MM','AAAA','HH',textvar(:)
	    do iAno=anomin,anomax
	      do iMes=1,12
	        ndias= ndiasmes(iMes,iAno) !function
	        do iDia=1,ndias
	          
	          do iHora=0,23
	          
	            !write(ip,'(2i4,i6,i4,<nvar>f12.2)')iDia,iMes,iAno,iHora,rDados(iAno,iMes,iDia,iHora,ip,:) !para gravar em intervalo horario
	            
	            if (iHora==0.or.ihora==12.or.iHora==18) then
	                write(ip,'(2i4,i6,i4,<nvar>f12.2)')iDia,iMes,iAno,iHora,rDados(iAno,iMes,iDia,iHora,ip,:) !para gravas registros usuais
	            end if
	            
	          end do
	        end do
	      end do
	    end do	    
	END DO

!    !Dealoca tudo
    deallocate(rDados)
    deallocate(ArqPosto) 
    write(*,*)'Fim do Programa' 

	        
    read(*,*)

   
    STOP

    END PROGRAM

