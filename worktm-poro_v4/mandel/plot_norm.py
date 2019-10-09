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


## Feb. 18
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
#plt.ylabel('Accumulated CPU Time (minutes)', fontsize=19)

#plt.xlabel('Simulation Period (days)', fontsize=25)
#plt.ylabel('Accumulated Iterative Coupling Iterations', fontsize=25)

#plt.xlabel('Simulation Time (days)', fontsize=19)
#plt.ylabel('Accumulated Flow Linear Iterations', fontsize=19)

#plt.xlabel('Simulation Time (days)', fontsize=19)
#plt.ylabel('Accumulated Mechanics Linear Iterations', fontsize=19)

plt.title('Fixed Stress Split Error Indicator for The First Three Time Steps (Mandel Problem)', fontsize=19)

plt.legend(loc='upper right')
#plt.legend(loc='lower right') 

savefig("test.png")
plt.show()


## Feb 18



