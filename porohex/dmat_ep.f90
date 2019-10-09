!   Author: Ruijie Liu, Aerospace Engineering,
!           University of Texas at Austin
!                              
!          
subroutine mat_solver(ielem,igauss,stress_pred,&
                D_ep,EP_MODEL,cutback)   
!
!rliu 
! 
use truth
use model_type 
use system
use control
implicit none
integer mat_id,ielem,iface,master,igauss,np,&
        yield,n_residue,nTens,convergent,plas_model,&
        harden_model,loc,mmm,cutback,EP_MODEL
real(kind=double) young,pssn,a,b,c,strain(6,1),&
tmp(6,1),TOL,stress_pred(6),D_ep(6,6),dsig0de,sig_bar,&
delta_pstrain(6),sig0,stress_temp(6),&
vstate(2),yield_para(2),flow_para(2),iso_harden(2),I1,J2,&
dJ2dsig(6),ddJ2ddsig(6,6),Y,dYdsig(6),dYdX,&
dFdsig(6),ddFddX(6,7),dFdI1,ddFddI1,delta_lamda,&
XX(8),mresidue(8),estrain_old(6),pstrain_old(6),&
vstate_old(2),stress_old(6),delta_strain(6),L2,&
dResiduedX(8,8),delta_XX(8),pstrain(6),dsigma0dep
!write(*,*) 'IN MATERIAL SOLVER'
! transversely isotropic
!real(kind=double) pa2psi
!parameter (pa2psi=0.0001450378911491d0)
! pascal to psi

nTens=6
n_residue=nTens+2
D_ep=zero_d
mat_id=ielem
young=mat_prpty(mat_id)%prpty_sld(1)
pssn=mat_prpty(mat_id)%prpty_sld(2)
a=young/(one_d+pssn)/(one_d-two_d*pssn)
b=one_d-pssn
c=half_d-pssn
! isotropic
D_ep(1,1)=b*a
D_ep(2,2)=D_ep(1,1)
D_ep(3,3)=D_ep(1,1)
D_ep(4,4)=young/2.0d0/(1.0d0+pssn)
D_ep(5,5)=D_ep(4,4)
D_ep(6,6)=D_ep(4,4)
D_ep(1,2)=pssn*a
D_ep(1,3)=D_ep(1,2)
D_ep(2,1)=D_ep(1,2)
D_ep(2,3)=D_ep(1,2)
D_ep(3,1)=D_ep(1,2)
D_ep(3,2)=D_ep(1,2)
! transversely isotropic
! mandel
!D_ep(1,1)=24.1d9*pa2psi
!D_ep(2,2)=21.0d9*pa2psi
!D_ep(3,3)=24.1d9*pa2psi
!D_ep(4,4)=7.23d9*pa2psi
!D_ep(5,5)=8.65d9*pa2psi
!D_ep(6,6)=7.23d9*pa2psi
!D_ep(1,2)=7.62d9*pa2psi
!D_ep(1,3)=6.8d9*pa2psi
!D_ep(2,1)=7.62d9*pa2psi
!D_ep(2,3)=7.62d9*pa2psi
!D_ep(3,1)=6.8d9*pa2psi
!D_ep(3,2)=7.62d9*pa2psi
cutback=0
!using delta_u (inside a fixed load step) to 
!get an incremental strain
!
!       write(*,*) 'ielem igauss',ielem,igauss
yield=0
!write(*,*) 'ielem igauss',ielem,igauss
stress_pred=0.0d0
 !write(*,*) 'ielem igauss',ielem,igauss
! cutback=0
 !write(*,*) 'ielem igauss',ielem,igauss

 estrain_old(1:6)=hexa(ielem)%gpt_estrn_t(igauss,1:6)
!write(*,*) 'estrain====',estrain_old
pstrain_old(1:6)=hexa(ielem)%gpt_pstrn_t(igauss,1:6)
!write(*,*) 'before stress predicted'
call stress_prediction(ielem,igauss,stress_pred,D_ep,&
    stress_old,delta_strain)
plas_model=mat_prpty(mat_id)%model_id
yield_para(1:2)=mat_prpty(mat_id)%yield_para(1:2)
harden_model=mat_prpty(mat_id)%harden_id
flow_para(1:2)=mat_prpty(mat_id)%flow_para(1:2)
!write(*,*)'flow_para===',flow_para(1:2)
iso_harden(1:2)=mat_prpty(mat_id)%iso_harden(1:2)
!write(*,*)' iso_harden===', iso_harden(1:2)
! write(*,*)'harden_model===',harden_model      
call stress_invariants(stress_pred,I1,J2)
!write(500,*)'plas_model q===',plas_model,sqrt(3*J2)
vstate_old(1:2)=hexa(ielem)%gpt_vstate_t(igauss,1:2)
if (EP_MODEL.eq.0) then
  yield=0
else
  call mat_yield(I1,J2,vstate_old,yield_para,yield,plas_model)
endif
!write(*,*) 'yield EP_MODEL==',yield,EP_MODEL
if(yield.lt.1.or.EP_MODEL<1) then
   hexa(ielem)%gpt_strss(igauss,:)=stress_pred
   hexa(ielem)%gpt_strn(igauss,:)=estrain_old+pstrain_old+delta_strain
   hexa(ielem)%gpt_estrn(igauss,:)=estrain_old+delta_strain
   return
end if
XX=0.0d0
L2=1.0
vstate=vstate_old
!hexa(ielem)%gpt_yield(igauss)=1.0
stress_old=stress_pred
no_iteration=0
!write(*,*) 'MAT_NR_TOL===',MAT_NR_TOL
do while(L2.gt.MAT_NR_TOL)   
   call J2_deriv(stress_pred,dJ2dsig,ddJ2ddsig)
   call mat_harden(harden_model,iso_harden,&
     vstate,dsigma0dep)
   call mat_flow(plas_model,stress_pred,vstate,&
       yield_para,flow_para,I1,J2,dJ2dsig,&
       ddJ2ddsig,Y,dYdsig,dYdX,dFdsig,ddFddX,&
       dFdI1,ddFddI1) 
   call mat_stiff_load(D_ep,vstate,XX,Y,dYdsig,&
       dYdX,dFdsig,ddFddX,dFdI1,ddFddI1,&
       dsigma0dep,dResiduedX,mresidue) 
   call mat_core_solver(dResiduedX,mresidue,&
       delta_XX)
   call mat_update(harden_model,iso_harden,&
      XX,delta_XX,vstate_old,vstate,&
      stress_old,stress_pred,pstrain_old,&
      pstrain,dsigma0dep,D_ep,dFdsig,dFdI1)     
   call L2_norm(mresidue,n_residue,L2)
   L2=L2/vstate_old(1)   ! noralizing
  !write(*,*) 'material no_iteration L2=',no_iteration,L2
   no_iteration=no_iteration+1 
   if(no_iteration.gt.MAT_MAX_ITER) then
     cutback=1
!           write(*,*) 'No Convergence at a Material Point. &
!                       Time Step Will be Reduced'
           return
        end if
        if(L2.lt.MAT_NR_TOL) then
           call mat_cstiff(dResiduedX,D_ep)        
           hexa(ielem)%gpt_strss(igauss,:)=stress_pred
           hexa(ielem)%gpt_strn(igauss,:)=estrain_old+&
                                pstrain_old+delta_strain
           hexa(ielem)%gpt_pstrn(igauss,:)=pstrain
           hexa(ielem)%gpt_estrn(igauss,:)=&
           hexa(ielem)%gpt_strn(igauss,:)-pstrain
           hexa(ielem)%gpt_vstate(igauss,:)=vstate
           delta_pstrain=pstrain-pstrain_old
           dsig0de=dsigma0dep 
           sig_bar=vstate(1)
           return
        end if
  call stress_invariants(stress_pred,I1,J2)
end do
return
end


subroutine stress_prediction(ielem,igauss,stress,D_ep,&
           stress_old,delta_strain)
use truth
use model_type 
use system  
implicit none
integer ielem,inode,igauss,local,ncase
real(kind=double) stress(6),D_ep(6,6),estrain_old(6),&
pstrain_old(6),stress_old(6),r,s,t,&
delta_strain(6),temp(3,1),temp2(6,1),dvolu,temp3(3,1),&
u_vectort(24,1),u_total(8,3),bmatx(6,24),N_matx(3,24),&
u_vector(24,1),u_add(8,3) 
b_dof=6
node_dof=3
ele_node=hexa(ielem)%tl_node
ele_type=hexa(ielem)%ele_type
ngauss=hexa(ielem)%ngauss
ele_dof=ele_node*node_dof
ncase=2                                         
call projection(ncase,ielem,u_add)
do inode=1,ele_node
   local=(inode-1)*node_dof+1 
   u_vector(local:local+2,1)=u_add(inode,1:node_dof)
end do
delta_strain=zero_d
if(ele_type==1) then           
   r=gauss_coord_8(igauss,1)   
   s=gauss_coord_8(igauss,2)   
   t=gauss_coord_8(igauss,3)   
else                          
   r=gauss_coord_27(igauss,1)  
   s=gauss_coord_27(igauss,2)  
   t=gauss_coord_27(igauss,3)  
end if                          
call hexanb0(ielem,r,s,t,bmatx,dvolu,N_matx)
stress_old(1:6)=hexa(ielem)%gpt_strss_t(igauss,1:6)
temp2=matmul(bmatx,u_vector)
delta_strain(1:6)=temp2(1:6,1)
temp2=matmul(D_ep,temp2)
stress(1:6)=stress_old(1:6)+temp2(1:6,1)
hexa(ielem)%gpt_strss(igauss,1:6)=stress
return
end


subroutine mat_yield(I1,J2,state_v,yield_para,yield,plas_model)
use truth
use model_type  
use system
implicit none                                             
integer plas_model,yield
real(kind=double) I1,J2,state_v(2),yield_para(2)
!
!
 yield=0
! write(*,*) 'state_v q ',state_v(1),sqrt(3.0d0*J2)
! write(*,*) 'yield_para',yield_para(1),yield_para(2)
! if(plas_model.eq.1.and.(sqrt(3.0d0*J2)-state_v(1)).gt.0.0d0) then
!   yield=1
!   !J2 flow theory
! elseif(plas_model.eq.2.and.(sqrt(3.0d0*J2)+&
!        yield_para(2)*I1/3.0d0-state_v(1)).gt.0.0d0) then
!   yield=1
 !end if
! write(*,*) 'sqrtJ2 I1/3 Y===== ',sqrt(3.0d0*J2),I1/3,sqrt(3.0d0*J2)+&
!       yield_para(2)*I1/3.0d0-state_v(1)
if((sqrt(3.0d0*J2)+&
       yield_para(2)*I1/3.0d0-state_v(1)).gt.0.0d0) then
   yield=1
end if
 return
 end
 
     
subroutine mat_flow(plas_model,stress,vstate,&
          yield_para,flow_para,I1,J2,dJ2dsig,&
          ddJ2ddsig,Y,dYdsig,dYdX,dFdsig,ddFddX,&
          dFdI1,ddFddI1)
use truth
use model_type 
use system
implicit none                                             
integer plas_model,i,j
real(kind=double) sigma0,&
dsigma0dep,eqpl,yield_para(2),flow_para(2),I1,J2,&
dJ2dsig(6),ddJ2ddsig(6,6),Y,dYdsig(6),dYdX,dFdsig(6),ddFddX(6,7),&
dFdI1,ddFddI1,dFdJ2,ddFddJ2,dYdI1,dYdJ2,dYdsig0,vstate(2),I63(6),&
ddFdJ2dsig0,ddFdI1dsig0,stress(6)
I63=0.0d0
I63(1)=1.0d0
I63(2)=1.0d0
I63(3)=1.0d0
!if(plas_model.eq.1) then
!   dFdI1=0.0d0
!   dFdJ2=sqrt(3.0d0)/2.0d0/sqrt(J2)
!   ddFddI1=0.0d0
!   ddFddJ2=-sqrt(3.0d0)/4.0d0/sqrt(J2)/J2
!   ddFdJ2dsig0=0.0d0
!   ddFdI1dsig0=0.0d0
!   dYdI1=0.0d0
!   dYdJ2=dFdJ2
!   dYdsig0=-1.0d0
!   Y=sqrt(3.0d0*J2)-vstate(1)
!elseif(plas_model.eq.2) then  
   dFdI1=flow_para(1)/3.0d0
   dFdJ2=sqrt(3.0d0)/2.0d0/sqrt(J2)
   ddFddI1=0.0d0
   ddFddJ2=-sqrt(3.0d0)/4.0d0/sqrt(J2)/J2
   ddFdJ2dsig0=0.0d0
   ddFdI1dsig0=0.0d0
   dYdI1=yield_para(2)/3.0d0
   dYdJ2=dFdJ2
   dYdsig0=-1.0d0
   Y=sqrt(3.0d0*J2)+yield_para(2)*I1/3.0d0-vstate(1)
!end if
dFdsig=0.0d0
dFdsig(1:3)=dFdI1
dFdsig=dFdsig+dFdJ2*dJ2dsig
dYdsig=0.0d0
dYdsig(1:3)=dYdI1
dYdsig(1:6)=dYdsig(1:6)+dYdJ2*dJ2dsig
dYdX=dYdsig0
ddFddX=0.0d0
ddFddX(1:3,1:3)=ddFddI1
do i=1,6
   do j=1,6
      ddFddX(i,j)=ddFddX(i,j)+ddFddJ2*dJ2dsig(i)*dJ2dsig(j)
   end do
end do
ddFddX(1:6,1:6)=ddFddX(1:6,1:6)+dFdJ2*ddJ2ddsig
ddFddX(1:6,7)=ddFdJ2dsig0*dJ2dsig+ddFdI1dsig0*I63
return
end



subroutine mat_harden(harden_id,iso_harden,&
           vstate,dsigma0dep)
use truth
use model_type 
use system
implicit none                                             
integer harden_id
real(kind=double) iso_harden(2),eqpl,sigma0,&
                  dsigma0dep,harden_para(2),vstate(2)
eqpl=vstate(2)
harden_para=iso_harden                
!if(harden_id.eq.1) then
!   sigma0=harden_para(1)+harden_para(2)*eqpl
   dsigma0dep=harden_para(1)
!elseif(harden_id.eq.2) then
!   sigma0=harden_para(2)*eqpl+harden_para(3)-&
!   (harden_para(3)-harden_para(1))*exp(-harden_para(4)*eqpl)
!   dsigma0dep=harden_para(2)+harden_para(4)*(harden_para(3)-harden_para(1))*&
!   exp(-harden_para(4)*eqpl)
!end if
!vstate(1)=sigma0
!write(500,*) 'dsigma0dep==',dsigma0dep
return
end

subroutine mat_stiff_load(D_ep,vstate,XX,Y,dYdsig,dYdX,dFdsig,&
           ddFddX,dFdI1,ddFddI1,dsig0dep,dResiduedX,mresidue)        
use truth
use model_type 
use system
implicit none 
integer i,j,nTens                                           
real(kind=double) D_ep(6,6),XX(8),Y,dYdsig(6),dYdX,dFdsig(6),&
ddFddX(6,7),dResiduedX(8,8),mresidue(8),I63(6),vstate(2),&
temp2(6,6),dsig0dep,dFdI1,ddFddI1,epdev(6),dadsig(6),a,b,dev(6,6),&
lam,dlamdsig(6),dlamdsig0
nTens=6
I63=0.0d0
I63(1:3)=1.0d0
mresidue=0.0d0
dResiduedX=0.0d0
dev=0.0d0
dev(1:3,1:3)=-ddFddI1
dev=dev+ddFddX(1:6,1:6)
a=0.0d0
epdev=dFdsig-dFdI1*I63
do i=1,3
   a=a+epdev(i)*epdev(i)
enddo
do i=4,6
   a=a+epdev(i)*epdev(i)/2.0d0
end do
a=sqrt(2.0d0*a/3.0d0)
epdev(4:6)=epdev(4:6)/2.0d0
dadsig=2.0d0/3.0d0/a*matmul(dev,epdev)
mresidue(1:nTens)=XX(1:nTens)+XX(8)*matmul(D_ep,dFdsig)
mresidue(nTens+1)=dsig0dep*XX(8)*a-XX(7)
mresidue(nTens+2)=Y
do i=1,nTens   
   dResiduedX(i,i)=1.0d0
end do
dResiduedX(1:nTens,1:nTens)=dResiduedX(1:nTens,1:nTens)+&
   XX(8)*matmul(D_ep,ddFddX(1:6,1:6))                     
dResiduedX(1:nTens,nTens+1)=XX(8)*matmul(D_ep,ddFddX(1:6,7))
dResiduedX(1:nTens,nTens+2)=matmul(D_ep,dFdsig)
dResiduedX(nTens+1,1:nTens)=XX(8)*dsig0dep*dadsig
dResiduedX(nTens+1,nTens+1)=-1.0d0
dResiduedX(nTens+1,nTens+2)=dsig0dep*a
dResiduedX(nTens+2,1:nTens)=dYdsig  
dResiduedX(nTens+2,nTens+1)=dYdX
!do i=1,7
!   do j=1,7
!write(500,*)
!'j,i,dResiduedX(j,i),mresidue(i)',j,i,dResiduedX(j,i),mresidue(i)
!   end do
!end do
return
end

subroutine mat_core_solver(dResiduedX,mresidue,delta_XX)
use truth
use model_type 
use system
implicit none  
integer iposn(8),n_residue,i,iii                                          
real(kind=double) delta_XX(8),&
dResiduedX(8,8),mresidue(8),&
pivin(8),tmp1(8,1)
n_residue=8
!call inverse(dResiduedX,pivin,iposn,n_residue)
call MATINV(dResiduedX,n_residue,n_residue,iii)
delta_XX=0.0d0
tmp1(:,1)=mresidue
tmp1=matmul(dResiduedX,tmp1)
delta_XX=tmp1(:,1)
!write(500,*) 'delta_XX',delta_XX
return
end


subroutine mat_cstiff(dResiduedX,D_ep)
use truth
use model_type 
use system
implicit none
integer i,j                                           
real(kind=double) dResiduedX(8,8),D_ep(6,6),D_ep_tmp(6,6)
D_ep_tmp=0.0d0
D_ep_tmp=matmul(dResiduedX(1:6,1:6),D_ep)
D_ep=D_ep_tmp
return
end


subroutine mat_update(harden_model,iso_harden,XX,delta_XX,&
           vstate_old,vstate,stress_prdc,stress,pstrain_old,&
           pstrain,dsigma0dep,D_ep,dFdsig,dFdI1)
use truth
use model_type 
use system
implicit none  
integer harden_model,i,j                                         
real(kind=double) iso_harden(2),XX(8),delta_XX(8),&
        vstate_old(2),vstate(2),stress_prdc(6),stress(6),&
        pstrain_old(6),pstrain(6),dsigma0dep,a,b,I63(6),epdev(6),&
        D_ep(6,6),dFdsig(6),dFdI1
I63=0.0d0
I63(1)=1.0d0
I63(2)=1.0d0
I63(3)=1.0d0
XX=XX-delta_XX
stress=stress_prdc+XX(1:6)
a=0.0d0
epdev=dFdsig-dFdI1*I63
do i=1,3
   a=a+epdev(i)*epdev(i)
enddo
do i=4,6
   a=a+epdev(i)*epdev(i)/2.0d0
end do
a=sqrt(2.0d0*a/3.0d0)
pstrain=pstrain_old+XX(8)*dFdsig
vstate(2)=vstate_old(2)+XX(8)*a
!write(*,*) 'eqpl sigma0===',vstate(2),vstate(1)
!write(500,*) 'vstate(1)_old',vstate_old(1)
vstate(1)=vstate_old(1)+XX(7)
!write(500,*) 'vstate(1)_new',vstate(1)
return
end


