program main
!
!Author: Ruijie Liu
!       
!
!master or main program to drive forward and get solution
!for the system
!
use truth
implicit none
integer icpl_flag,i
integer gravity_flag            
integer initial_flag            
integer node_total                  
integer ele_total                  
integer mat_grp_total            
integer mat_nprop           
integer total_0_dch            
integer total_load_point 
integer total_traction_face
integer total_pressured_face
integer elm_gnode_id(8,16)             
integer elm_mat_id(16) 
integer solve_flag,ncase
real(kind=double) mat_grp_prop(10,16)
real(kind=double) node_coord(3,45)             
real(kind=double) dis_bc_node(3,33)             
real(kind=double) force_bc_node(3,1)
real(kind=double) traction_bc_face(5,8)
real(kind=double) pressure_bc_face(3,1)
real(kind=double) elm_stress0(6,16)             
real(kind=double) elm_pore_pressure0(16)      
real(kind=double) elm_pore_pressure(16)      
real(kind=double) node_displacement(3,45)       
real(kind=double) node_stress(6,45)            
real(kind=double) node_strain(6,45)           
real(kind=double) elm_vstrain(16)
real(kind=double) rhs_at_n(135)
real(kind=double) gravity(3)
open(100, file='output.dat', status='unknown')
open(200, file='ux.txt', status='unknown')
open(300, file='uy.txt', status='unknown')
open(400, file='uz.txt', status='unknown')
open(500, file='stressx.txt', status='unknown')
open(600, file='stressy.txt', status='unknown')
open(700, file='stressz.txt', status='unknown')
node_total=45
ele_total=16                 
mat_grp_total=16            
mat_nprop=10           
total_0_dch=33            
total_load_point=0 
total_traction_face=8
total_pressured_face=0
icpl_flag=1
gravity_flag=1
initial_flag=1
mat_nprop=10
gravity=0.0d0
gravity(1)=0.0d0
mat_grp_prop=0.0d0
do i=1,mat_grp_total
   mat_grp_prop(1,i)=2.0e+7
   mat_grp_prop(2,i)=0.3
   mat_grp_prop(6,i)=0.8
   mat_grp_prop(7,i)=5.0e+7
   mat_grp_prop(8,i)=250.0d0
end do
do i=1,ele_total
   elm_mat_id(i)=i
end do
node_coord(1,1)=0.0
node_coord(2,1)=1.0
node_coord(3,1)=0.0
node_coord(1,2)=0.0
node_coord(2,2)=0.9
node_coord(3,2)=-0.3
node_coord(1,3)=0.0
node_coord(2,3)=0.707
node_coord(3,3)=-0.707
node_coord(1,4)=0.0
node_coord(2,4)=0.3
node_coord(3,4)=-0.9
node_coord(1,5)=0.0
node_coord(2,5)=0.0
node_coord(3,5)=-1.0
node_coord(1,6)=0.0
node_coord(2,6)=3.0
node_coord(3,6)=0.0
node_coord(1,7)=0.0
node_coord(2,7)=2.7
node_coord(3,7)=-0.9
node_coord(1,8)=0.0
node_coord(2,8)=2.1
node_coord(3,8)=-2.1
node_coord(1,9)=0.0
node_coord(2,9)=0.9
node_coord(3,9)=-2.7
node_coord(1,10)=0.0
node_coord(2,10)=0.0
node_coord(3,10)=-3.0
node_coord(1,11)=0.0
node_coord(2,11)=8.0
node_coord(3,11)=0.0
node_coord(1,12)=0.0
node_coord(2,12)=8.0
node_coord(3,12)=-4.0
node_coord(1,13)=0.0
node_coord(2,13)=8.0
node_coord(3,13)=-8.0
node_coord(1,14)=0.0
node_coord(2,14)=4.0
node_coord(3,14)=-8.0
node_coord(1,15)=0.0
node_coord(2,15)=0.0
node_coord(3,15)=-8.0
node_coord(1,16)=2.0
node_coord(2,16)=1.0
node_coord(3,16)=0.0
node_coord(1,17)=2.0
node_coord(2,17)=0.9
node_coord(3,17)=-0.3
node_coord(1,18)=2.0
node_coord(2,18)=0.707
node_coord(3,18)=-0.707
node_coord(1,19)=2.0
node_coord(2,19)=0.3
node_coord(3,19)=-0.9
node_coord(1,20)=2.0
node_coord(2,20)=0.0
node_coord(3,20)=-1.0
node_coord(1,21)=2.0
node_coord(2,21)=3.0
node_coord(3,21)=0.0
node_coord(1,22)=2.0
node_coord(2,22)=2.7
node_coord(3,22)=-0.9
node_coord(1,23)=2.0
node_coord(2,23)=2.1
node_coord(3,23)=-2.1
node_coord(1,24)=2.0
node_coord(2,24)=0.9
node_coord(3,24)=-2.7
node_coord(1,25)=2.0
node_coord(2,25)=0.0
node_coord(3,25)=-3.0
node_coord(1,26)=2.0
node_coord(2,26)=8.0
node_coord(3,26)=0.0
node_coord(1,27)=2.0
node_coord(2,27)=8.0
node_coord(3,27)=-4.0
node_coord(1,28)=2.0
node_coord(2,28)=8.0
node_coord(3,28)=-8.0
node_coord(1,29)=2.0
node_coord(2,29)=4.0
node_coord(3,29)=-8.0
node_coord(1,30)=2.0
node_coord(2,30)=0.0
node_coord(3,30)=-8.0
node_coord(1,31)=4.0
node_coord(2,31)=1.0
node_coord(3,31)=0.0
node_coord(1,32)=4.0
node_coord(2,32)=0.9
node_coord(3,32)=-0.3
node_coord(1,33)=4.0
node_coord(2,33)=0.707
node_coord(3,33)=-0.707
node_coord(1,34)=4.0
node_coord(2,34)=0.3
node_coord(3,34)=-0.9
node_coord(1,35)=4.0
node_coord(2,35)=0.0
node_coord(3,35)=-1.0
node_coord(1,36)=4.0
node_coord(2,36)=3.0
node_coord(3,36)=0.0
node_coord(1,37)=4.0
node_coord(2,37)=2.7
node_coord(3,37)=-0.9
node_coord(1,38)=4.0
node_coord(2,38)=2.1
node_coord(3,38)=-2.1
node_coord(1,39)=4.0
node_coord(2,39)=0.9
node_coord(3,39)=-2.7
node_coord(1,40)=4.0
node_coord(2,40)=0.0
node_coord(3,40)=-3.0
node_coord(1,41)=4.0
node_coord(2,41)=8.0
node_coord(3,41)=0.0
node_coord(1,42)=4.0
node_coord(2,42)=8.0
node_coord(3,42)=-4.0
node_coord(1,43)=4.0
node_coord(2,43)=8.0
node_coord(3,43)=-8.0
node_coord(1,44)=4.0
node_coord(2,44)=4.0
node_coord(3,44)=-8.0
node_coord(1,45)=4.0
node_coord(2,45)=0.0
node_coord(3,45)=-8.0
elm_gnode_id(1,1)=10
elm_gnode_id(2,1)=25
elm_gnode_id(3,1)=9
elm_gnode_id(4,1)=24
elm_gnode_id(5,1)=5
elm_gnode_id(6,1)=20
elm_gnode_id(7,1)=4
elm_gnode_id(8,1)=19
elm_gnode_id(1,2)=9
elm_gnode_id(2,2)=24
elm_gnode_id(3,2)=8
elm_gnode_id(4,2)=23
elm_gnode_id(5,2)=4
elm_gnode_id(6,2)=19
elm_gnode_id(7,2)=3
elm_gnode_id(8,2)=18
elm_gnode_id(1,3)=8
elm_gnode_id(2,3)=23
elm_gnode_id(3,3)=7
elm_gnode_id(4,3)=22
elm_gnode_id(5,3)=3
elm_gnode_id(6,3)=18
elm_gnode_id(7,3)=2
elm_gnode_id(8,3)=17
elm_gnode_id(1,4)=7
elm_gnode_id(2,4)=22
elm_gnode_id(3,4)=6
elm_gnode_id(4,4)=21
elm_gnode_id(5,4)=2
elm_gnode_id(6,4)=17
elm_gnode_id(7,4)=1
elm_gnode_id(8,4)=16
elm_gnode_id(1,5)=15
elm_gnode_id(2,5)=30
elm_gnode_id(3,5)=14
elm_gnode_id(4,5)=29
elm_gnode_id(5,5)=10
elm_gnode_id(6,5)=25
elm_gnode_id(7,5)=9
elm_gnode_id(8,5)=24
elm_gnode_id(1,6)=14
elm_gnode_id(2,6)=29
elm_gnode_id(3,6)=13
elm_gnode_id(4,6)=28
elm_gnode_id(5,6)=9
elm_gnode_id(6,6)=24
elm_gnode_id(7,6)=8
elm_gnode_id(8,6)=23
elm_gnode_id(1,7)=13
elm_gnode_id(2,7)=28
elm_gnode_id(3,7)=12
elm_gnode_id(4,7)=27
elm_gnode_id(5,7)=8
elm_gnode_id(6,7)=23
elm_gnode_id(7,7)=7
elm_gnode_id(8,7)=22
elm_gnode_id(1,8)=12
elm_gnode_id(2,8)=27
elm_gnode_id(3,8)=11
elm_gnode_id(4,8)=26
elm_gnode_id(5,8)=7
elm_gnode_id(6,8)=22
elm_gnode_id(7,8)=6
elm_gnode_id(8,8)=21
elm_gnode_id(1,9)=25
elm_gnode_id(2,9)=40
elm_gnode_id(3,9)=24
elm_gnode_id(4,9)=39
elm_gnode_id(5,9)=20
elm_gnode_id(6,9)=35
elm_gnode_id(7,9)=19
elm_gnode_id(8,9)=34
elm_gnode_id(1,10)=24
elm_gnode_id(2,10)=39
elm_gnode_id(3,10)=23
elm_gnode_id(4,10)=38
elm_gnode_id(5,10)=19
elm_gnode_id(6,10)=34
elm_gnode_id(7,10)=18
elm_gnode_id(8,10)=33
elm_gnode_id(1,11)=23
elm_gnode_id(2,11)=38
elm_gnode_id(3,11)=22
elm_gnode_id(4,11)=37
elm_gnode_id(5,11)=18
elm_gnode_id(6,11)=33
elm_gnode_id(7,11)=17
elm_gnode_id(8,11)=32
elm_gnode_id(1,12)=22
elm_gnode_id(2,12)=37
elm_gnode_id(3,12)=21
elm_gnode_id(4,12)=36
elm_gnode_id(5,12)=17
elm_gnode_id(6,12)=32
elm_gnode_id(7,12)=16
elm_gnode_id(8,12)=31
elm_gnode_id(1,13)=30
elm_gnode_id(2,13)=45
elm_gnode_id(3,13)=29
elm_gnode_id(4,13)=44
elm_gnode_id(5,13)=25
elm_gnode_id(6,13)=40
elm_gnode_id(7,13)=24
elm_gnode_id(8,13)=39
elm_gnode_id(1,14)=29
elm_gnode_id(2,14)=44
elm_gnode_id(3,14)=28
elm_gnode_id(4,14)=43
elm_gnode_id(5,14)=24
elm_gnode_id(6,14)=39
elm_gnode_id(7,14)=23
elm_gnode_id(8,14)=38
elm_gnode_id(1,15)=28
elm_gnode_id(2,15)=43
elm_gnode_id(3,15)=27
elm_gnode_id(4,15)=42
elm_gnode_id(5,15)=23
elm_gnode_id(6,15)=38
elm_gnode_id(7,15)=22
elm_gnode_id(8,15)=37
elm_gnode_id(1,16)=27
elm_gnode_id(2,16)=42
elm_gnode_id(3,16)=26
elm_gnode_id(4,16)=41
elm_gnode_id(5,16)=22
elm_gnode_id(6,16)=37
elm_gnode_id(7,16)=21
elm_gnode_id(8,16)=36
dis_bc_node(1,1)=31
dis_bc_node(2,1)=1
dis_bc_node(3,1)=0.0
dis_bc_node(1,2)=32
dis_bc_node(2,2)=1
dis_bc_node(3,2)=0.0
dis_bc_node(1,3)=45
dis_bc_node(2,3)=1
dis_bc_node(3,3)=0.0
dis_bc_node(1,4)=33
dis_bc_node(2,4)=1
dis_bc_node(3,4)=0.0
dis_bc_node(1,5)=34
dis_bc_node(2,5)=1
dis_bc_node(3,5)=0.0
dis_bc_node(1,6)=35
dis_bc_node(2,6)=1
dis_bc_node(3,6)=0.0
dis_bc_node(1,7)=36
dis_bc_node(2,7)=1
dis_bc_node(3,7)=0.0
dis_bc_node(1,8)=37
dis_bc_node(2,8)=1
dis_bc_node(3,8)=0.0
dis_bc_node(1,9)=38
dis_bc_node(2,9)=1
dis_bc_node(3,9)=0.0
dis_bc_node(1,10)=39
dis_bc_node(2,10)=1
dis_bc_node(3,10)=0.0
dis_bc_node(1,11)=40
dis_bc_node(2,11)=1
dis_bc_node(3,11)=0.0
dis_bc_node(1,12)=41
dis_bc_node(2,12)=1
dis_bc_node(3,12)=0.0
dis_bc_node(1,13)=42
dis_bc_node(2,13)=1
dis_bc_node(3,13)=0.0
dis_bc_node(1,14)=43
dis_bc_node(2,14)=1
dis_bc_node(3,14)=0.0
dis_bc_node(1,15)=44
dis_bc_node(2,15)=1
dis_bc_node(3,15)=0.0
dis_bc_node(1,16)=5
dis_bc_node(2,16)=2
dis_bc_node(3,16)=0.0
dis_bc_node(1,17)=10
dis_bc_node(2,17)=2
dis_bc_node(3,17)=0.0
dis_bc_node(1,18)=15
dis_bc_node(2,18)=2
dis_bc_node(3,18)=0.0
dis_bc_node(1,19)=20
dis_bc_node(2,19)=2
dis_bc_node(3,19)=0.0
dis_bc_node(1,20)=25
dis_bc_node(2,20)=2
dis_bc_node(3,20)=0.0
dis_bc_node(1,21)=30
dis_bc_node(2,21)=2
dis_bc_node(3,21)=0.0
dis_bc_node(1,22)=35
dis_bc_node(2,22)=2
dis_bc_node(3,22)=0.0
dis_bc_node(1,23)=40
dis_bc_node(2,23)=2
dis_bc_node(3,23)=0.0
dis_bc_node(1,24)=45
dis_bc_node(2,24)=2
dis_bc_node(3,24)=0.0
dis_bc_node(1,25)=1
dis_bc_node(2,25)=3
dis_bc_node(3,25)=0.0
dis_bc_node(1,26)=6
dis_bc_node(2,26)=3
dis_bc_node(3,26)=0.0
dis_bc_node(1,27)=11
dis_bc_node(2,27)=3
dis_bc_node(3,27)=0.0
dis_bc_node(1,28)=16
dis_bc_node(2,28)=3
dis_bc_node(3,28)=0.0
dis_bc_node(1,29)=21
dis_bc_node(2,29)=3
dis_bc_node(3,29)=0.0
dis_bc_node(1,30)=26
dis_bc_node(2,30)=3
dis_bc_node(3,30)=0.0
dis_bc_node(1,31)=31
dis_bc_node(2,31)=3
dis_bc_node(3,31)=0.0
dis_bc_node(1,32)=36
dis_bc_node(2,32)=3
dis_bc_node(3,32)=0.0
dis_bc_node(1,33)=41
dis_bc_node(2,33)=3
dis_bc_node(3,33)=0.0
total_load_point=0
do i=1,8
   traction_bc_face(1,i)=i
   traction_bc_face(2,i)=1
   traction_bc_face(3,i)=100
end do
elm_stress0=0.0d0
do i=1,16
   elm_stress0(1,i)=-10
   elm_stress0(2,i)=-5
   elm_stress0(3,i)=-10
   elm_pore_pressure0(i)=5
   elm_pore_pressure(i)=20
end do

call module_elasticity(icpl_flag,&                   !icpl_flag=1: undrained iterative coupled; 2: Drained iterative coupled;
                          gravity_flag,&             ! 1: gravity taken into account;
                          initial_flag,&             ! 1: initial total stress and initial pore pressure is considered; 
                          node_total,&               !total node # in this model;
                          ele_total,&                !toal element # in this model
                          mat_grp_total,&            !total material group # in this model;
                          mat_nprop,&                !number of parameters in each material (assume the same for all materials);
                          total_0_dch,&              !total # of prescribed displacement constraints in this model;
                          total_load_point,&         !total # of nodal force applied;
                          total_traction_face,&      !total # of surface applied by traction (traction vector is global based);
                          total_pressured_face,&     !total # of faces applied by uniform loading (pressure loading but NOT pore pressure);
                          mat_grp_prop,&             !material properties;
                          node_coord,&               !node coordinates;
                          elm_gnode_id,&             !element global nodal ID;
                          elm_mat_id,&               !element material ID;
                          dis_bc_node,&              !dis_bc_node(1:tl_0_dch,i):  i=1: The node id of this consttraint; i=2:local dof; i=3: value;
                          force_bc_node,&            !force_bc_node(1:tl_load_point,i):  i=1: The node id of this force applied to; i=2:local dof; i=3: value;
                          traction_bc_face,&         !traction surface loading defined:  i=1:  element id  i=2: local face #  i=3,4,5: traction real value;
                          pressure_bc_face,&         !pressure_bc_face(tl_pressured_face,i):  i=1: The element id of this surface loading; i=2:local face id; i=3: value;
                          elm_stress0,&              !element-based initial total stress;
                          elm_pore_pressure0,&       !element-based initial pore pressure;
                          elm_pore_pressure,&        !pore pressure at center;
                          node_displacement,&        !output node displacement;
                          node_stress,&              !output node stress for visulization;
                          node_strain,&              !output node strain for visulization;
                          elm_vstrain,&              !output element-based volume strain;
                          rhs_at_n,&                 !prevous time step righ hand side load vector;
                          gravity,&                  !gravity vector;
                          solve_flag)  
stop
end
