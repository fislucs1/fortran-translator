! ************************************************    !
!		          Diffusion Monte Carlo						!
!		            Sample Program     						!
!				      												!
!   This program pefforms Monte Carlo Integration 		!
!   wave equation for several systems, including		!
!   the H.O., LJ. P., and for H, H2+, and H2				!

program main		! main function
	
	implicit none
	
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
	
	
	double precision :: psips2, ener1, ener0, wz1, vrefav, dev0, bi, deviation, tmcount, dt, rhh, rhh2, crhh, wz
	integer(kind = 8) :: itot, i, itime, id, ie, ij, iw, potnum, Ndim, Npsips, Nmax, seed, Nchec, ri, mrf, Nwf, Ncol
	!!!! FILE (line 34)
	
	! *********** USER initialization *************** !
	
	call system('clear') ! clear console in terminal
	print *, '*****************************************************'
	print *, '*                                                   *'
	print *, '*           Diffusion Monte Carlo                   *'
	print *, '*                Simulator                          *'
	print *, '*                                                   *'
	print *, 'A.	One-dimensional potentials'
	print *, '		1. Harmonic Oscillator'
	print *, '		2. Morse Potential'
	print *, '-------------------------------'
	print *, 'B. 	3-dimensional potentials'
	print *, '		3. Hydrogen atom'
	print *, '		4. H2+ molecule'
	print *, '		5. H2 molecule'
	print *, '-------------------------------'
	print *, 'Select a potential to simulate:  '
	read *, potnum
	if (potnum < 3) then ! (potnum = 2,1,0, etc)
		Ndim = 1
	end if
	if ((potnum > 2) .and. (potnum < 5)) then ! (potnum = 3,4)
		Ndim = 3
	end if
	if (potnum == 5) then ! (potnum = 5)
		Ndim = 6
	end if
	print *, 'Number in parenthesis are suggestions'
	print *, 'Ndim = ', Ndim
	print *, 'Enter the Number of Replicas: (500)'
	read *, Npsips
	print *, 'Replicas = ', Npsips
	print*, 'Enter the max number of replicas: (2000)'
	read *, Nmax
	print *, 'Nmax = ', Nmax
	print *, 'Enter random number seed: '
	read *, seed
	if (seed > 0) then
		seed = -1 * seed
	end if
	print *, 'Seed = ', -1 * seed
	print *, 'Enter the amount of time to run the simulation'
	read *, Nchec
	if (potnum /= 3) then
		print *, 'Enter the left limit for sampling data: (-20)'
		read *, ri
	else 
		ri = 0
	end if
	print *, 'left = ', ri
	print *, 'Enter the right limit for sampling data: (20) '
	read *, mrf
	print *, 'right = ', mrf
	print *, 'Enter the number of boxes to sort into: (200)'
	read *, Nwf
	Ncol = Nchec
	print *, 'boxes = ', Nwf
	print *, 'Enter a time step: (.1)'
	read *, dt
	
	if ( (potnum < 6) .and. (potnum > 3) ) then ! potnum = 4 e 5
		print *, 'Enter deviation from R ( H - H distance ) in units of bohr radius: '
		read *, deviation
		if (potnum == 4) then 
			rhh = rhh2 + deviation
		end if
		if (potnum == 5) then
			rhh = crhh + deviation
			print *, 'Using', rhh, 'as hydrogen-hydrogen distance.'
		end if
	end if
	

!      Initialize all arrays with user's entered data        !

print *, 'Ncol = ', Ncol
! 
!	real, allocatable, dimension(:) :: ener
!	allocate ( ener(Ncol) )
!	real, dimension(Nmax, 1 + Ndim) :: psips

!	real, dimension(Nwf) :: wz
	

	
	
	
	
	
	
end program main
	
