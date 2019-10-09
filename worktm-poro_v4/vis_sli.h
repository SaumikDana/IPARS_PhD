c	vis_sli.dh
c definitions of vis_sli interface types
c
	integer numsli
	integer maxsli
	parameter (maxsli=3	)
	real*8 vxc(3	,2)
	real*8 vyc(3	,2)
	real*8 vzc(3	,2)
	integer vblk(3	)
	common /cvissli/ numsli,vblk
c  ,vyc,vzc








