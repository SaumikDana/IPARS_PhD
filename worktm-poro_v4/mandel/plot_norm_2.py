from pylab import *

import pylab as plot
params = {'legend.fontsize': 25,
          'legend.linewidth': 2}
plot.rcParams.update(params)


days_threshold = 60

offset = 20

####################################
days_sr = [0]
times_sr = [0]
gciter_sr = [0]
linear_sr_f = [0]
linear_sr_m = [0]
norms = [0]
norms_index = [0]

#infile = open('test_may14','r')
#infile = open('sr_0_016_may05.dat','r')
#infile = open('MR2_TEST_JUNE15.dat','r')
#infile = open('mr8_june18_reported.dat','r')
#infile = open('res_sr_aug12.dat','r')
#infile = open('mr8_sep6.dat','r')
#infile = open('dummy_1.dat','r')
#infile = open('ls_ver_4.dat', 'r')
infile = open('result_Feb18.dat','r')

for line in infile:
    line = line.split(' ')
    corrected_line = []
    for item in line:
        if item != '':
           corrected_line.append(item)
#    print corrected_line
    if corrected_line[0] == 'TIME':
       days_sr.append(float(corrected_line[1]))
    if corrected_line[0] == 'ELAPSED':
       times_sr.append(float(corrected_line[1]))
    if corrected_line[0] == 'GCITER_C':
       gciter_sr.append(float(corrected_line[1]))
    if corrected_line[0] == 'LINEAR_F':
       linear_sr_f.append(float(corrected_line[1]))
    if corrected_line[0] == 'LINEAR_M':
       linear_sr_m.append(float(corrected_line[1]))
    if corrected_line[0] == 'COMP_NORM':
       norms.append(float(corrected_line[1]))
       norms_index.append(norms_index[-1]+1)


#times_sr_min = [x/60 for x in times_sr]

#accum_gciter_sr = [0]

#for item in gciter_sr:
#    accum_gciter_sr.append(accum_gciter_sr[-1]+item)
#accum_gciter_sr.pop(0)
#print 'length of days_sr:', len(days_sr)
#print 'length of accum_gciter_sr:', len(accum_gciter_sr)
#print gciter_sr
#print  accum_gciter_sr
####################################

print norms


norm_ratio = [0]
for i in range(2,len(norms)):
      if norms[i-1] != 0.0:
           norm_ratio.append(norms[i]/norms[i-1])

print 'Norms'
for item in norms:
    print item

print 'Norm Ratios'
for item in norm_ratio:
#    if float(item) < 0.0001:
#       print '-------------------'
#    else:
     if float(item) > 0.000001:
       print item

###########################################
###########################################

coup_itr = [1,2,3,4]
coup_itr_2 = [1,2,3]
norms_t_1 = [40875.2579430690, 5604.59946238284, 890.313642737772, 148.577026090582]

norms_t_2 = [29338.9913986868, 4129.52658098431, 685.207655262748, 118.141174963535]

norms_t_3 = [22475.8504100505, 3229.69019005621, 557.084110590304]
plt.plot(coup_itr,norms_t_1,'k.-',label='1st Time Step')
plt.plot(coup_itr,norms_t_2,'r.-',label='2nd Time Step')
plt.plot(coup_itr_2,norms_t_3,'b.-',label='3rd Time Step')

plt.xlabel('Coupling Iterations', fontsize=19)
plt.ylabel('Fixed-Stress Split Error Indicator', fontsize=19)

###########################################

#plt.plot(days_sr,times_sr,'kD-',label='Single Rate')

######
#plt.plot(days_sr,times_sr,'k.-',label='Single Rate')
#plt.plot(days_sr,accum_gciter_sr,'kD-',label='Single Rate')
#plt.plot(days_sr,linear_sr_f,'k.-',label='Single Rate')
#plt.plot(days_sr,linear_sr_m,'k.-',label='Single Rate')
######


#plt.plot(days_btr,times_btr_min,'r.-',label='Backtracking')
#plt.plot(days_btr,accum_linear_btr,'rD-',label='Backtracking')
#plt.plot(days_btr,accum_nonlinear_btr,'bD-',label='Backtracking')

#plt.plot(days_btr_s,times_btr_s,'k.-',label='Backtracking')
#plt.plot(days_btr_s,times_btr_min_s,'r.-',label='Backtracking')
#plt.plot(days_btr_s,accum_linear_btr_s,'r.-',label='Backtracking')
#plt.plot(days_btr_s,accum_nonlinear_btr_s,'r.-',label='Backtracking')

#plt.xlabel('Simulation Period (days)', fontsize=19)
#plt.ylabel('Accumulated CPU Time (seconds)', fontsize=19)
#plt.ylabel('Accumulated CPU Time (minutes)', fontsize=19)

#plt.xlabel('Simulation Period (days)', fontsize=25)
#plt.ylabel('Accumulated Iterative Coupling Iterations', fontsize=25)

#plt.xlabel('Simulation Time (days)', fontsize=19)
#plt.ylabel('Accumulated Flow Linear Iterations', fontsize=19)

#plt.xlabel('Simulation Time (days)', fontsize=19)
#plt.ylabel('Accumulated Mechanics Linear Iterations', fontsize=19)

#plt.title('SPE10 First Layer Results', fontsize=25)

#plt.legend(loc='upper left') 
#plt.legend(loc='lower right') 

#savefig("test.png")
#plt.show()

######
#plt.plot(days_mul2,times_mul2,'r.-',label='Multirate (q = 2)')
#plt.plot(days_mul2,accum_gciter_mul2,'r.-',label='Multirate (q = 2)')
#plt.plot(days_mul2,linear_mul2_f,'r.-',label='Multirate (q = 2)')
#plt.plot(days_mul2,linear_mul2_m,'r.-',label='Multirate (q = 2)')
######

#plt.plot(days_pure_fc,accum_linear_pure_fc,'g.-',label='Forcing = 1.0: RHS = 0.5 * eta * ||F(Xk)||')
#plt.plot(days_pure_fc,accum_nonlinear_pure_fc,'r.-',label='Forcing = 1.0: RHS = 0.5 * eta * ||F(Xk)||')

#plt.plot(days_pure_fc_s,times_pure_fc_s,'r.-',label='Forcing = 0.5: RHS = 0.5 * eta * ||F(Xk)||')
#plt.plot(days_pure_fc_s,times_pure_fc_min_s,'b.-',label='Forcing = 0.5: RHS = 0.5 * eta * ||F(Xk)||')
#plt.plot(days_pure_fc_s,accum_linear_pure_fc_s,'b.-',label='Forcing = 0.5: RHS = 0.5 * eta * ||F(Xk)||')
#plt.plot(days_pure_fc_s,accum_nonlinear_pure_fc_s,'b.-',label='Forcing = 0.5: RHS = 0.5 * eta * ||F(Xk)||')

#plt.legend(loc='upper left')
#plt.legend(loc='lower right') 

######
#plt.plot(days_mul4,times_mul4,'g.-',label='Multirate (q = 4)')
#plt.plot(days_mul4,accum_gciter_mul4,'g.-',label='Multirate (q = 4)')
#plt.plot(days_mul4,linear_mul4_f,'g.-',label='Multirate (q = 4)')
#plt.plot(days_mul4,linear_mul4_m,'g.-',label='Multirate (q = 4)')
######

#plt.plot(days_safe_fc,accum_linear_safe_fc,'b.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')
#plt.plot(days_safe_fc,accum_nonlinear_safe_fc,'g.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')

#plt.plot(days_safe_fc_s,times_safe_fc_s,'g.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')
#plt.plot(days_safe_fc_s,times_safe_fc_min_s,'g.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')
#plt.plot(days_safe_fc_s,accum_linear_safe_fc_s,'g.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')
#plt.plot(days_safe_fc_s,accum_nonlinear_safe_fc_s,'g.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')

#plt.legend(loc='upper left')
#plt.legend(loc='lower right') 

###### 
#plt.plot(days_mul8,times_mul8,'b.-',label='Multirate (q = 8)')
#plt.plot(days_mul8,accum_gciter_mul8,'b.-',label='Multirate (q = 8)')
#plt.plot(days_mul8,linear_mul8_f,'b.-',label='Multirate (q = 8)')
#plt.plot(days_mul8,linear_mul8_m,'b.-',label='Multirate (q = 8)')
######

#plt.plot(days_no_fc,accum_linear_no_fc,'k.-',label='No forcing, RHS = LSOL_TOL * ||F(Xk)||,\n LSOL_TOL = 1.E-6')
#plt.plot(days_no_fc,accum_nonlinear_no_fc,'k.-',label='No forcing, RHS = LSOL_TOL * ||F(Xk)||, LSOL_TOL = 1.E-6')
#plt.legend(loc='upper left') 

#plt.plot(days_mul16,times_mul16,'m.-',label='Multirate (q = 16)')
#plt.plot(days_mul16,times_mul16,'m-',label='Multirate (q = 16)')
#plt.legend(loc='upper left') 

#plt.plot(days_mul20,times_mul20,'y.-',label='Multirate (q = 20)')
#plt.legend(loc='upper left') 


#plt.plot(days_forc_0_5,times_forc_0_5,'k.-',label='Forcing = 0.5: RHS = 0.5 * eta * ||F(Xk)||')
#plt.legend(loc='upper left') 

#plt.plot(days_forc_0_6,times_forc_0_6,linestyle="dashed", marker="o",color="blue",label='Forcing = 0.6: RHS = 0.6 * eta * ||F(Xk)||')

#plt.legend(loc='upper left')
#plt.legend(loc='lower right') 

#plt.plot(days_forc_0_7,times_forc_0_7,linestyle="dashed", marker="o",color="green",label='Forcing = 0.7: RHS = 0.7 * eta * ||F(Xk)||')
#plt.legend(loc='upper left') 

#plt.plot(days_forc_0_8,times_forc_0_8,linestyle="dashed", marker="o",color="red",label='Forcing = 0.8: RHS = 0.8 * eta * ||F(Xk)||')
#plt.legend(loc='upper left') 

#plt.plot(days_forc_0_9,times_forc_0_9,linestyle="dashed", marker="o",color="black",label='Forcing = 0.9: RHS = 0.9 * eta * ||F(Xk)||')
#plt.legend(loc='upper left') 


#plt.show()






######
######
#days_btr_s = [0]
#times_btr_s = [0]
#accum_linear_btr_s = [0]
#accum_nonlinear_btr_s = [0]
#index = 0
#for item in days_btr:
#    if (index % offset) == 0:
#        days_btr_s.append(days_btr[index])
#    index = index + 1

#index = 0
#for item in times_btr:
#    if (index % offset) == 0:
#        times_btr_s.append(times_btr[index])
#    index = index + 1

#times_btr_min_s = [x/60 for x in times_btr_s]

######
######

########################################


####################################
#days_pure_fc = [0]
#times_pure_fc = [0]
#linear_pure_fc = [0]
#nonlinear_pure_fc = [0]

#infile = open('doutput_Ben_fc_10_July21','r')
#for line in infile:
#    line = line.split(' ')
#    corrected_line = []
#    for item in line:
#        if item != '':
#           corrected_line.append(item)
##    print corrected_line
#    if corrected_line[0] == 'TIME':
#       days_pure_fc.append(float(corrected_line[1]))
#    if corrected_line[0] == 'NEWTON':
#       nonlinear_pure_fc.append(float(corrected_line[1]))
#    if corrected_line[0] == 'LINEAR':
#       linear_pure_fc.append(float(corrected_line[1]))
#    if corrected_line[0] == 'ELAPSED':
#       times_pure_fc.append(float(corrected_line[1]))


#times_pure_fc_min = [x/60 for x in times_pure_fc]

##print days_pure_fc
##print times_pure_fc
##print linear_pure_fc
##print nonlinear_pure_fc


#accum_linear_pure_fc = [0]
#accum_nonlinear_pure_fc = [0]

#for item in linear_pure_fc:
#     accum_linear_pure_fc.append(accum_linear_pure_fc[-1]+item)

#accum_linear_pure_fc.pop(0)

#for item in nonlinear_pure_fc:
#     accum_nonlinear_pure_fc.append(accum_nonlinear_pure_fc[-1]+item)

#accum_nonlinear_pure_fc.pop(0)

#print accum_linear_pure_fc
#print accum_nonlinear_pure_fc

#days_btr_1 = [0]
#times_btr_1 = [0]
#for index in range(len(times_btr)):
#    days_btr_1.append(days_btr[index])
#    times_btr_1.append(times_btr[index])
#    if (days_btr[index] > days_threshold):
#       break


######
######
#days_pure_fc_s = [0]
#times_pure_fc_s = [0]
#accum_linear_pure_fc_s = [0]
#accum_nonlinear_pure_fc_s = [0]

#index = 0
#for item in days_pure_fc:
#    if (index % offset) == 0:
#        days_pure_fc_s.append(days_pure_fc[index])
#    index = index + 1

#index = 0
#for item in times_pure_fc:
#    if (index % offset) == 0:
#        times_pure_fc_s.append(times_pure_fc[index])
#    index = index + 1

#times_pure_fc_min_s = [x/60 for x in times_pure_fc_s]

#index = 0
#for item in accum_linear_pure_fc:
#    if (index % offset) == 0:
#        accum_linear_pure_fc_s.append(accum_linear_pure_fc[index])
#    index = index + 1

#index = 0
#for item in accum_nonlinear_pure_fc:
#    if (index % offset) == 0:
#        accum_nonlinear_pure_fc_s.append(accum_nonlinear_pure_fc[index])
#    index = index + 1
######
######


########################################


####################################
#days_safe_fc = [0]
#times_safe_fc = [0]
#linear_safe_fc = [0]
#nonlinear_safe_fc = [0]

#infile = open('doutput_Ben_fc_01_July21','r')
#for line in infile:
#    line = line.split(' ')
#    corrected_line = []
#    for item in line:
#        if item != '':
#           corrected_line.append(item)
##    print corrected_line
#    if corrected_line[0] == 'TIME':
#       days_safe_fc.append(float(corrected_line[1]))
#    if corrected_line[0] == 'NEWTON':
#       nonlinear_safe_fc.append(float(corrected_line[1]))
#    if corrected_line[0] == 'LINEAR':
#       linear_safe_fc.append(float(corrected_line[1]))
#    if corrected_line[0] == 'ELAPSED':
#       times_safe_fc.append(float(corrected_line[1]))


#times_safe_fc_min = [x/60 for x in times_safe_fc]

##print days_safe_fc
##print times_safe_fc
##print linear_safe_fc
##print nonlinear_safe_fc


#accum_linear_safe_fc = [0]
#accum_nonlinear_safe_fc = [0]

#for item in linear_safe_fc:
#     accum_linear_safe_fc.append(accum_linear_safe_fc[-1]+item)

#accum_linear_safe_fc.pop(0)

#for item in nonlinear_safe_fc:
#     accum_nonlinear_safe_fc.append(accum_nonlinear_safe_fc[-1]+item)

#accum_nonlinear_safe_fc.pop(0)

##print accum_linear_safe_fc
##print accum_nonlinear_safe_fc

##days_btr_1 = [0]
##times_btr_1 = [0]
##for index in range(len(times_btr)):
##    days_btr_1.append(days_btr[index])
##    times_btr_1.append(times_btr[index])
##    if (days_btr[index] > days_threshold):
##       break


######
######
#days_safe_fc_s = [0]
#times_safe_fc_s = [0]
#accum_linear_safe_fc_s = [0]
#accum_nonlinear_safe_fc_s = [0]

#index = 0
#for item in days_safe_fc:
#    if (index % offset) == 0:
#        days_safe_fc_s.append(days_safe_fc[index])
#    index = index + 1

#index = 0
#for item in times_safe_fc:
#    if (index % offset) == 0:
#        times_safe_fc_s.append(times_safe_fc[index])
#    index = index + 1

#times_safe_fc_min_s = [x/60 for x in times_safe_fc_s]

#index = 0
#for item in accum_linear_safe_fc:
#    if (index % offset) == 0:
#        accum_linear_safe_fc_s.append(accum_linear_safe_fc[index])
#    index = index + 1

#index = 0
#for item in accum_nonlinear_safe_fc:
#    if (index % offset) == 0:
#        accum_nonlinear_safe_fc_s.append(accum_nonlinear_safe_fc[index])
#    index = index + 1
######
######


########################################

####################################
#days_no_fc = [0]
#times_no_fc = [0]
#linear_no_fc = [0]
#nonlinear_no_fc = [0]

#infile = open('doutput_Ben_nf_July21','r')
#for line in infile:
#    line = line.split(' ')
#    corrected_line = []
#    for item in line:
#        if item != '':
#           corrected_line.append(item)
##    print corrected_line
#    if corrected_line[0] == 'TIME':
#       days_no_fc.append(float(corrected_line[1]))
#    if corrected_line[0] == 'NEWTON':
#       nonlinear_no_fc.append(float(corrected_line[1]))
#    if corrected_line[0] == 'LINEAR':
#       linear_no_fc.append(float(corrected_line[1]))
#    if corrected_line[0] == 'ELAPSED':
#       times_no_fc.append(float(corrected_line[1]))

#times_no_fc_min = [x/60 for x in times_no_fc]


##print days_safe_fc
##print times_safe_fc
##print linear_safe_fc
##print nonlinear_safe_fc


#accum_linear_no_fc = [0]
#accum_nonlinear_no_fc = [0]

#for item in linear_no_fc:
#     accum_linear_no_fc.append(accum_linear_no_fc[-1]+item)

#accum_linear_no_fc.pop(0)

#for item in nonlinear_no_fc:
#     accum_nonlinear_no_fc.append(accum_nonlinear_no_fc[-1]+item)

#accum_nonlinear_no_fc.pop(0)

##print accum_linear_safe_fc
##print accum_nonlinear_safe_fc

##days_btr_1 = [0]
##times_btr_1 = [0]

########################################

#plt.plot(days_btr,times_btr,'kD-',label='Single Rate')
#plt.plot(days_btr,times_btr_min,'r.-',label='Backtracking')
#plt.plot(days_btr,accum_linear_btr,'rD-',label='Backtracking')
#plt.plot(days_btr,accum_nonlinear_btr,'bD-',label='Backtracking')

#plt.plot(days_btr_s,times_btr_s,'k.-',label='Backtracking')
#plt.plot(days_btr_s,times_btr_min_s,'r.-',label='Backtracking')
#plt.plot(days_btr_s,accum_linear_btr_s,'r.-',label='Backtracking')
#plt.plot(days_btr_s,accum_nonlinear_btr_s,'r.-',label='Backtracking')

#plt.xlabel('Simulation Period (days)', fontsize=19)
#plt.ylabel('Accumulated CPU Time (seconds)', fontsize=19)
#plt.ylabel('Accumulated CPU Time (minutes)', fontsize=19)

#plt.xlabel('Simulation Period (days)', fontsize=25)
#plt.ylabel('Accumulated Linear Iterations', fontsize=25)

#plt.xlabel('Simulation Pime (days)', fontsize=19)
#plt.ylabel('Accumulated Nonlinear Iterations', fontsize=19)

#plt.title('SPE10 First Layer Results', fontsize=25)
#plt.legend(loc='upper left') 
#savefig("test.png")
#plt.show()

#plt.plot(days_pure_fc,times_pure_fc,'r.-',label='Forcing = 1.0: RHS =  eta * ||F(Xk)||')
#plt.plot(days_pure_fc,times_pure_fc_min,'b.-',label='Forcing = 0.5: RHS = 0.5 * eta * ||F(Xk)||')
#plt.plot(days_pure_fc,accum_linear_pure_fc,'g.-',label='Forcing = 1.0: RHS = 0.5 * eta * ||F(Xk)||')
#plt.plot(days_pure_fc,accum_nonlinear_pure_fc,'r.-',label='Forcing = 1.0: RHS = 0.5 * eta * ||F(Xk)||')

#plt.plot(days_pure_fc_s,times_pure_fc_s,'r.-',label='Forcing = 0.5: RHS = 0.5 * eta * ||F(Xk)||')
#plt.plot(days_pure_fc_s,times_pure_fc_min_s,'b.-',label='Forcing = 0.5: RHS = 0.5 * eta * ||F(Xk)||')
#plt.plot(days_pure_fc_s,accum_linear_pure_fc_s,'b.-',label='Forcing = 0.5: RHS = 0.5 * eta * ||F(Xk)||')
#plt.plot(days_pure_fc_s,accum_nonlinear_pure_fc_s,'b.-',label='Forcing = 0.5: RHS = 0.5 * eta * ||F(Xk)||')

#plt.legend(loc='upper left') 

#plt.plot(days_safe_fc,times_safe_fc,'g.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')
#plt.plot(days_safe_fc,times_safe_fc_min,'g.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')
#plt.plot(days_safe_fc,accum_linear_safe_fc,'b.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')
#plt.plot(days_safe_fc,accum_nonlinear_safe_fc,'g.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')

#plt.plot(days_safe_fc_s,times_safe_fc_s,'g.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')
#plt.plot(days_safe_fc_s,times_safe_fc_min_s,'g.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')
#plt.plot(days_safe_fc_s,accum_linear_safe_fc_s,'g.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')
#plt.plot(days_safe_fc_s,accum_nonlinear_safe_fc_s,'g.-',label='Forcing = 0.1: RHS = 0.1 * eta * ||F(Xk)||')

#plt.legend(loc='upper left')
 
#plt.plot(days_no_fc,times_no_fc,'b.-',label='No forcing, RHS = LSOL_TOL * ||F(Xk)||, LSOL_TOL = 1.E-6')
#plt.plot(days_no_fc,times_no_fc_min,'b.-',label='No forcing, RHS = LSOL_TOL * ||F(Xk)||, LSOL_TOL = 1.E-6')
#plt.plot(days_no_fc,accum_linear_no_fc,'k.-',label='No forcing, RHS = LSOL_TOL * ||F(Xk)||,\n LSOL_TOL = 1.E-6')
#plt.plot(days_no_fc,accum_nonlinear_no_fc,'k.-',label='No forcing, RHS = LSOL_TOL * ||F(Xk)||, LSOL_TOL = 1.E-6')
#plt.legend(loc='upper left') 

#plt.plot(days_forc_0_3_1,times_forc_0_3_1,'m.-',label='Forcing = 0.3: RHS = 0.3 * eta * ||F(Xk)||')
#plt.legend(loc='upper left') 

#plt.plot(days_forc_0_4,times_forc_0_4,'y.-',label='Forcing = 0.4: RHS = 0.4 * eta * ||F(Xk)||')
#plt.legend(loc='upper left') 


#plt.plot(days_forc_0_5,times_forc_0_5,'k.-',label='Forcing = 0.5: RHS = 0.5 * eta * ||F(Xk)||')
#plt.legend(loc='upper left') 

#plt.plot(days_forc_0_6,times_forc_0_6,linestyle="dashed", marker="o",color="blue",label='Forcing = 0.6: RHS = 0.6 * eta * ||F(Xk)||')

#plt.legend(loc='upper left') 

#plt.plot(days_forc_0_7,times_forc_0_7,linestyle="dashed", marker="o",color="green",label='Forcing = 0.7: RHS = 0.7 * eta * ||F(Xk)||')
#plt.legend(loc='upper left') 

#plt.plot(days_forc_0_8,times_forc_0_8,linestyle="dashed", marker="o",color="red",label='Forcing = 0.8: RHS = 0.8 * eta * ||F(Xk)||')
#plt.legend(loc='upper left') 

#plt.plot(days_forc_0_9,times_forc_0_9,linestyle="dashed", marker="o",color="black",label='Forcing = 0.9: RHS = 0.9 * eta * ||F(Xk)||')
#plt.legend(loc='upper left') 

#plt.show()
