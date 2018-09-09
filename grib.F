module grib
    
	contains
	
	subroutine loadgrib(gfile,lvls,tarr,qarr,parr, &
		&harr,uarr,varr,sfcarr,sbcape,mlcape,mucape)
		
	use wgrib2api
	
	character(len = 100), intent(in) :: gfile
	integer, intent(in) :: lvls
	real, dimension(lvls,:,:), allocatable, intent(out) :: tarr, qarr, &
						&parr, harr, uarr, varr, sfcarr, sbcape, mlcape, mucape
	character(len = 15), dimension(6) :: vlist
	
	
	
	vlist = (':TMP:',':SPFH:',':PRES:',':HGT:',':UGRD:',':VGRD:')
	
	do i=1, 6
		
		do j=1, lvls
		
		
		
		end do
	end do
	
	
