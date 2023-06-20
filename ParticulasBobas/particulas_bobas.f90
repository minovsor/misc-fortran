
program main

integer,parameter :: npmax =100000
integer :: idp(:) !identificador (se precisar)
real :: sc(ip)  !local da particula
real :: velp(:)      !velocidade da particula




!le mini.gtp



!  tempo de residencia (local)
!TR(:) = QJ2(:)/VOL(:)

! tempo de residencia acumulado
!-> descer a rede acumulada


!aloca e npmax particulas
! id, celula, posicao na celula, status, velocidade, tempo de viagem, espaco viajado
allocate(idp(npmax),icp(npmax),sp(npmax),ligap(npmax),velp(npmax),travelt(npmax),travels(npmax))



!inicializa particulas (local de origem)
! aqui vai do experimento, pode ser uma minibacia especifica, ou varias de uma vez
! pode considerar o tamanho do trecho
ligap(:)=1

!loop do tempo
do it=1,nt

	! loop de movimento de particula
	do ip=1,npmax
		
		!particula desligada
		if (ligap(ip)==0) cycle
	
		ic = icp(ip)  !minibacia da particula
		smax = dx(ic)  !comprimento do trecho		
		sdx = dt*velp(ip)	!deslocamento da particula
		snew = sp(ip) +sdx
		if (snew > smax) then !avançou para trecho da frente		
			sdel = smax - sc(ip)   ! quanto andou no trecho anterior		
			sc(ip) = snew - sdel   ! posicao no trecho novo		
			icp(ip) = ijus(ic)     ! atualiza minibacia da particula
		
		elseif (snew<smax) then !continua no mesmo trecho
			sc(ip) = snew
		
		elseif( snew<sp(ip)) then !avançou para trecho anterior (velocidade negativa?)
			!faz qq coisa
		
		end if
		
		!atualiza outros atributos
		travelt(ip)=travelt(ip)+dt  !tempo em deslocamento
		travels(ip)=travels(ip)+sdx  !distancia percorrida
		
		!controles para desativar a particula
		if (icp(ip)==-1) then !fim da bacia
			opp(ip) = 0		
		
		end if
		
	end do

end do


end program





