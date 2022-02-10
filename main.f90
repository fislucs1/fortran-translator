! ************************************************    !
!		          Diffusion Monte Carlo						!
!		            Sample Program     						!
!				      												!
!   This program pefforms Monte Carlo Integration 		!
!   wave equation for several systems, including		!
!   the H.O., LJ. P., and for H, H2+, and H2				!

PROGRAM main		! main function
	
	IMPLICIT NONE
	
	! ********************************************************** !
	!	                 Variable Definitions				   		 !
	!																				 !	
	!	2 = replica matrix											 !
	!	ener1 = array used to store E0 values (for deviation)     !
	!	wz1 = 'boxes' used to sort replicas								 !
	!	itime = measures time												 !
	!	rf & ri = right & left measuring bounds for replica boxes !
	!	others = misc															 !
	!																				 !
	! ********************************************************** !
	
	
	! ***************************************************** !
	! 			    Help translations ( FORTRAN == C )			  !
	!																		  !
	!		double precision == double								  !
	!		integer(kind = 8) == long int                     !
	!		&& --> AND (C) 
	! ***************************************************** !
	
	
	DOUBLE PRECISION :: psips2, ener1, ener0, wz1, vrefav, dev0, bi, deviation, tmcount, dt, rhh, rhh2, crhh, wz
	INTEGER(KIND = 8) :: itot, i, itime, id, ie, ij, iw, potnum, Ndim, Npsips, Nmax, seed, Nchec, ri, mrf, Nwf, Ncol
	!!!! FILE (line 34)
	
	! *********** USER initialization *************** !
	
	CALL system('clear') ! clear console in terminal
	WRITE (*,*) '*****************************************************'
	WRITE (*,*) '*                                                   *'
	WRITE (*,*) '*           Diffusion Monte Carlo                   *'
	WRITE (*,*) '*                Simulator                          *'
	WRITE (*,*) '*                                                   *'
	WRITE (*,*) 'A.	One-dimensional potentials'
	WRITE (*,*) '		1. Harmonic Oscillator'
	WRITE (*,*) '		2. Morse Potential'
	WRITE (*,*) '-------------------------------'
	WRITE (*,*) 'B. 	3-dimensional potentials'
	WRITE (*,*) '		3. Hydrogen atom'
	WRITE (*,*) '		4. H2+ molecule'
	WRITE (*,*) '		5. H2 molecule'
	WRITE (*,*) '-------------------------------'
	WRITE (*,*) 'Select a potential to simulate:  '
	READ (*,*) potnum
	IF (potnum < 3) THEN ! (potnum = 2,1,0, etc)
		Ndim = 1
	END IF
	IF ((potnum > 2) .AND. (potnum < 5)) THEN ! (potnum = 3,4)
		Ndim = 3
	END IF
	IF (potnum == 5) THEN ! (potnum = 5)
		Ndim = 6
	END IF
	WRITE (*,*) 'Number in parenthesis are suggestions'
	WRITE (*,*) 'Ndim = ', Ndim
	WRITE (*,*) 'Enter the Number of Replicas: (500)'
	READ (*,*) Npsips
	WRITE (*,*) 'Replicas = ', Npsips
	WRITE (*,*) 'Enter the max number of replicas: (2000)'
	READ (*,*) Nmax
	WRITE (*,*) 'Nmax = ', Nmax
	WRITE (*,*) 'Enter random number seed: '
	READ (*,*) seed
	IF (seed > 0) THEN
		seed = -1 * seed
	END IF
	WRITE (*,*) 'Seed = ', -1 * seed
	WRITE (*,*) 'Enter the amount of time to run the simulation:    (1000)'
	READ (*,*) Nchec
	IF (potnum /= 3) THEN
		WRITE (*,*) 'Enter the left limit for sampling data: (-20)'
		READ (*,*) ri
	ELSE 
		ri = 0
	END IF
	WRITE (*,*) 'left = ', ri
	WRITE (*,*) 'Enter the right limit for sampling data: (20) '
	READ (*,*) mrf
	WRITE (*,*) 'right = ', mrf
	WRITE (*,*) 'Enter the number of boxes to sort into: (200)'
	READ (*,*) Nwf
	Ncol = Nchec
	WRITE (*,*) 'boxes = ', Nwf
	WRITE (*,*) 'Enter a time step: (.1)'
	READ (*,*) dt
	
	IF ( (potnum < 6) .AND. (potnum > 3) ) THEN ! potnum = 4 e 5
		WRITE (*,*) 'Enter deviation from R ( H - H distance ) in units of bohr radius: '
		READ (*,*) deviation
		IF (potnum == 4) THEN 
			rhh = rhh2 + deviation
		END IF
		IF (potnum == 5) THEN
			rhh = crhh + deviation
			WRITE (*,*) 'Using', rhh, 'as hydrogen-hydrogen distance.'
		END IF
	END IF
	

!      Initialize all arrays with user's entered data        !

print *, 'Ncol = ', Ncol
! 
!	real, allocatable, dimension(:) :: ener
!	allocate ( ener(Ncol) )
!	real, dimension(Nmax, 1 + Ndim) :: psips

!	real, dimension(Nwf) :: wz
	
!		Open all files for output		!


	
	
	
	
	
	
end program main
	
