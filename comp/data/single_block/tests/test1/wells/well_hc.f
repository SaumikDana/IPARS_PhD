      program well
      implicit none
      integer i,k,n,nw,kd,nkd,Nt,NMax,MxW,itens,iunits,funit
      parameter (MxW=10,Nt=5378,NMax=1000)
      character*40 fileWel,fileCum,tecname1(MxW),tecname2(MxW),
     &             tecname3(MxW),tecname4(MxW),tecname5(MxW),
     &             tecname6(MxW),tecname7(MxW),tecname8(MxW),
     &             tecname9(MxW),tecname10(MxW),tecname11(MxW),
     &             tecname12(MxW),tecname13(MxW),tecname14(MxW),
     &             tecname15(MxW)
      character*70 title
      character*2 tag
      integer jwi(MxW),jgi(MxW),jop(MxW),jwp(MxW),jgp(MxW),joi(MxW),
     &        jwo(MxW),jgo(MxW),jbhp(MxW)
      real*8 twi(MxW,Nt),tgi(MxW,Nt),top(MxW,Nt),twp(MxW,Nt),tgp(MxW,Nt)
     &  ,toi(MxW,Nt),two(MxW,Nt),tgo(MxW,Nt),tbhp(MxW,Nt),watinj(MxW,Nt)
     &  ,oilprod(MxW,Nt),watprod(MxW,Nt),gasinj(MxW,Nt),gasprod(MxW,Nt)
     &  ,oilinj(MxW,Nt),wor(MxW,Nt),gor(MxW,Nt),bhp(MxW,Nt)
     &  ,watinjR(MxW,Nt), gasinjR(MxW,Nt),oilinjR(MxW,Nt)
     &  ,watprodR(MxW,Nt),oilprodR(MxW,Nt),gasprodR(MxW,Nt)

      fileWel = 'SPE5_v2.WEL'
      fileCum = 'SPE5_v2.CUM'

c Set output file names and initialize counters to zero

      do i=1,MxW
         itens=i/10
         iunits=MOD(i,10)
         tag=char(48+itens)//char(48+iunits)
         tecname1(i) = 'watInj_well'//tag//'.dat'
         tecname2(i) = 'gasInj_well'//tag//'.dat'
         tecname3(i) = 'oilProd_well'//tag//'.dat'
         tecname4(i) = 'watProd_well'//tag//'.dat'
         tecname5(i) = 'gasProd_well'//tag//'.dat'
         tecname6(i) = 'oilInj_well'//tag//'.dat'
         tecname7(i) = 'bhp_well'//tag//'.dat'
         tecname8(i) = 'watInjR_well'//tag//'.dat'
         tecname9(i) = 'gasInjR_well'//tag//'.dat'
         tecname10(i) = 'oilInjR_well'//tag//'.dat'
         tecname11(i) = 'watProdR_well'//tag//'.dat'
         tecname12(i) = 'oilProdR_well'//tag//'.dat'
         tecname13(i) = 'gasProdR_well'//tag//'.dat'
         tecname14(i) = 'woRatio_well'//tag//'.dat'
         tecname15(i) = 'goRatio_well'//tag//'.dat'
         jwi(i)=0
         jgi(i)=0
         jop(i)=0
         jwp(i)=0
         jgp(i)=0
         joi(i)=0
         jbhp(i)=0
         jwo(i)=0
         jgo(i)=0
      enddo

      open(50,file=fileCum,status='old')

c Read cumulatives from cumulative data file

      do i = 1,NMax
         read(50,*,end=100,err=100)
         read(50,*,end=100,err=100) nw,kd,nkd
         if(kd.eq.1) then
            read(50,*,end=100,err=100)
     &         (twi(nw,k),k=jwi(nw)+1,jwi(nw)+nkd)
            read(50,*,end=100,err=100)
     &         (watinj(nw,k),k=jwi(nw)+1,jwi(nw)+nkd)
            jwi(nw)=jwi(nw)+nkd
         elseif(kd.eq.2) then
            read(50,*,end=100,err=100)
     &         (top(nw,k),k=jop(nw)+1,jop(nw)+nkd)
            read(50,*,end=100,err=100)
     &         (oilprod(nw,k),k=jop(nw)+1,jop(nw)+nkd)
            jop(nw)=jop(nw)+nkd
         elseif(kd.eq.3) then
            read(50,*,end=100,err=100)
     &         (twp(nw,k),k=jwp(nw)+1,jwp(nw)+nkd)
            read(50,*,end=100,err=100)
     &         (watprod(nw,k),k=jwp(nw)+1,jwp(nw)+nkd)
            jwp(nw)=jwp(nw)+nkd
         elseif(kd.eq.4) then
            read(50,*,end=100,err=100)
     &         (tgp(nw,k),k=jgp(nw)+1,jgp(nw)+nkd)
            read(50,*,end=100,err=100)
     &         (gasprod(nw,k),k=jgp(nw)+1,jgp(nw)+nkd)
            jgp(nw)=jgp(nw)+nkd
         elseif(kd.eq.8) then
            read(50,*,end=100,err=100)
     &         (tgi(nw,k),k=jgi(nw)+1,jgi(nw)+nkd)
            read(50,*,end=100,err=100)
     &         (gasinj(nw,k),k=jgi(nw)+1,jgi(nw)+nkd)
            jgi(nw)=jgi(nw)+nkd
         elseif(kd.eq.9) then
            read(50,*,end=100,err=100)
     &         (toi(nw,k),k=joi(nw)+1,joi(nw)+nkd)
            read(50,*,end=100,err=100)
     &         (oilinj(nw,k),k=joi(nw)+1,joi(nw)+nkd)
            joi(nw)=joi(nw)+nkd
         else
            stop 'Unknown well data type!!!'
         endif
      enddo
 100  continue

      do n=1,MxW
         itens=n/10
         iunits=MOD(n,10)
         tag=char(48+itens)//char(48+iunits)
         if(jwi(n).gt.0) then
            funit=70+n-1
            title='TITLE="CUMULATIVE WATER INJECTION, WELL '//tag//'"'
            open(funit,file=tecname1(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Water Injection [stb]",' 
            write(funit,*)'ZONE F=BLOCK, T="", I =',jwi(n)
            write(funit,*)(twi(n,i),i=1,jwi(n))
            write(funit,*)
            write(funit,*)(watinj(n,i),i=1,jwi(n))
         endif

         if(jgi(n).gt.0) then
            funit=71+n-1
            title='TITLE="CUMULATIVE GAS INJECTION, WELL '//tag//'"'
            open(funit,file=tecname2(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Gas Injection [mscf]",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',jgi(n)
            write(funit,*)(tgi(n,i),i=1,jgi(n))
            write(funit,*)
            write(funit,*)(gasinj(n,i),i=1,jgi(n))
         endif

         if(jop(n).gt.0) then
            funit=72+n-1
            title='TITLE="CUMULATIVE OIL PRODUCTION, WELL '//tag//'"'
            open(funit,file=tecname3(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Oil Production [stb]",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',jop(n)
            write(funit,*)(top(n,i),i=1,jop(n))
            write(funit,*)
            write(funit,*)(oilprod(n,i),i=1,jop(n))
         endif

         if(jwp(n).gt.0) then
            funit=73+n-1
            title='TITLE="CUMULATIVE WATER PRODUCTION, WELL '//tag//'"'
            open(funit,file=tecname4(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Water Production [stb]",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',jwp(n)
            write(funit,*)(twp(n,i),i=1,jwp(n))
            write(funit,*)
            write(funit,*)(watprod(n,i),i=1,jwp(n))
         endif

         if(jgp(n).gt.0) then
            funit=74+n-1
            title='TITLE="CUMULATIVE GAS PRODUCTION, WELL '//tag//'"'
            open(funit,file=tecname5(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Gas Production [mscf]",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',jgp(n)
            write(funit,*)(tgp(n,i),i=1,jgp(n))
            write(funit,*)
            write(funit,*)(gasprod(n,i),i=1,jgp(n))
         endif

        if(joi(n).gt.0) then
            funit=75+n-1
            title='TITLE="CUMULATIVE OIL INJECTION, WELL '//tag//'"'
            open(funit,file=tecname6(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Oil Injection [stb]",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',joi(n)
            write(funit,*)(toi(n,i),i=1,joi(n))
            write(funit,*)
            write(funit,*)(oilinj(n,i),i=1,joi(n))
        endif
      enddo

      close(50)
      do i=1,MxW
         if(jwi(n).gt.0) close(70+n-1)
         if(jgi(n).gt.0) close(71+n-1)
         if(jop(n).gt.0) close(72+n-1)
         if(jwp(n).gt.0) close(73+n-1)
         if(jgp(n).gt.0) close(74+n-1)
         if(joi(n).gt.0) close(75+n-1)
      enddo

c Re-initialize counters that were used to zero for next read.

      do i=1,MxW
         jwi(i)=0
         jgi(i)=0
         jop(i)=0
         jwp(i)=0
         jgp(i)=0
         joi(i)=0
      enddo

      open(51,file=fileWel,status='old')

c Read well quantity rates from well file.

      do i = 1,NMax
         read(51,*,end=101,err=101)
         read(51,*,end=101,err=101) nw,kd,nkd
         if(kd.eq.1) then
            read(51,*,end=101,err=101)
     &         (twi(nw,k),k=jwi(nw)+1,jwi(nw)+nkd)
            read(51,*,end=101,err=101)
     &         (watinjR(nw,k),k=jwi(nw)+1,jwi(nw)+nkd)
            jwi(nw)=jwi(nw)+nkd
         elseif(kd.eq.2) then
            read(51,*,end=101,err=101)
     &         (top(nw,k),k=jop(nw)+1,jop(nw)+nkd)
            read(51,*,end=101,err=101)
     &         (oilprodR(nw,k),k=jop(nw)+1,jop(nw)+nkd)
            jop(nw)=jop(nw)+nkd
         elseif(kd.eq.3) then
            read(51,*,end=101,err=101)
     &         (twp(nw,k),k=jwp(nw)+1,jwp(nw)+nkd)
            read(51,*,end=101,err=101)
     &         (watprodR(nw,k),k=jwp(nw)+1,jwp(nw)+nkd)
            jwp(nw)=jwp(nw)+nkd
         elseif(kd.eq.4) then
            read(51,*,end=101,err=101)
     &         (tgp(nw,k),k=jgp(nw)+1,jgp(nw)+nkd)
            read(51,*,end=101,err=101)
     &         (gasprodR(nw,k),k=jgp(nw)+1,jgp(nw)+nkd)
            jgp(nw)=jgp(nw)+nkd
         elseif(kd.eq.5) then
            read(51,*,end=101,err=101)
     &         (two(nw,k),k=jwo(nw)+1,jwo(nw)+nkd)
            read(51,*,end=101,err=101)
     &         (wor(nw,k),k=jwo(nw)+1,jwo(nw)+nkd)
            jwo(nw)=jwo(nw)+nkd
         elseif(kd.eq.6) then
            read(51,*,end=101,err=101)
     &         (tgo(nw,k),k=jgo(nw)+1,jgo(nw)+nkd)
            read(51,*,end=101,err=101)
     &         (gor(nw,k),k=jgo(nw)+1,jgo(nw)+nkd)
            jgo(nw)=jgo(nw)+nkd
         elseif(kd.eq.7) then
            read(51,*,end=101,err=101)
     &         (tbhp(nw,k),k=jbhp(nw)+1,jbhp(nw)+nkd)
            read(51,*,end=101,err=101)
     &         (bhp(nw,k),k=jbhp(nw)+1,jbhp(nw)+nkd)
            jbhp(nw)=jbhp(nw)+nkd
         elseif(kd.eq.8) then
            read(51,*,end=101,err=101)
     &         (tgi(nw,k),k=jgi(nw)+1,jgi(nw)+nkd)
            read(51,*,end=101,err=101)
     &         (gasinjR(nw,k),k=jgi(nw)+1,jgi(nw)+nkd)
            jgi(nw)=jgi(nw)+nkd
         elseif(kd.eq.9) then
            read(51,*,end=101,err=101)
     &         (toi(nw,k),k=joi(nw)+1,joi(nw)+nkd)
            read(51,*,end=101,err=101)
     &         (oilinjR(nw,k),k=joi(nw)+1,joi(nw)+nkd)
            joi(nw)=joi(nw)+nkd
         else
            stop 'Unknown well data type!!!'
         endif
      enddo
 101  continue

      do n=1,MxW
         itens=n/10
         iunits=MOD(n,10)
         tag=char(48+itens)//char(48+iunits)
         if(jwi(n).gt.0) then
            funit=76+n-1
            title='TITLE="WATER INJECTION RATE, WELL '//tag//'"'
            open(funit,file=tecname8(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Water Injn. Rate[stb/day]",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',jwi(n)
            write(funit,*)(twi(n,i),i=1,jwi(n))
            write(funit,*)
            write(funit,*)(watinjR(n,i),i=1,jwi(n))
         endif

         if(jgi(n).gt.0) then
            funit=77+n-1
            title='TITLE="GAS INJECTION RATE, WELL '//tag//'"'
            open(funit,file=tecname9(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Gas Injn. Rate [mscf/day]",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',jgi(n)
            write(funit,*)(tgi(n,i),i=1,jgi(n))
            write(funit,*)
            write(funit,*)(gasinjR(n,i),i=1,jgi(n))
         endif

         if(jop(n).gt.0) then
            funit=78+n-1
            title='TITLE="OIL PRODUCTION RATE, WELL '//tag//'"'
            open(funit,file=tecname12(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Oil Prodn. Rate [stb/day]",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',jop(n)
            write(funit,*)(top(n,i),i=1,jop(n))
            write(funit,*)
            write(funit,*)(oilprodR(n,i),i=1,jop(n))
         endif

         if(jwp(n).gt.0) then
            funit=79+n-1
            title='TITLE="WATER PRODUCTION RATE, WELL '//tag//'"'
            open(funit,file=tecname11(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Water Prod. Rate [stb/day]",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',jwp(n)
            write(funit,*)(twp(n,i),i=1,jwp(n))
            write(funit,*)
            write(funit,*)(watprodR(n,i),i=1,jwp(n))
         endif

         if(jgp(n).gt.0) then
            funit=80+n-1
            title='TITLE="GAS PRODUCTION RATE, WELL '//tag//'"'
            open(funit,file=tecname13(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Gas Prodn. Rate [mscf/day]",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',jgp(n)
            write(funit,*)(tgp(n,i),i=1,jgp(n))
            write(funit,*)
            write(funit,*)(gasprodR(n,i),i=1,jgp(n))
         endif

        if(joi(n).gt.0) then
            funit=81+n-1
            title='TITLE="WATER INJECTION RATE, WELL '//tag//'"'
            open(funit,file=tecname10(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Oil Injn. Rate [stb/day]",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',joi(n)
            write(funit,*)(toi(n,i),i=1,joi(n))
            write(funit,*)
            write(funit,*)(oilinjR(n,i),i=1,joi(n))
        endif

        if(jbhp(n).gt.0) then
            funit=82+n-1
            title='TITLE="BOTTOM HOLE PRESSURE, WELL '//tag//'"'
            open(funit,file=tecname7(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Well BHP [psi]",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',jbhp(n)
            write(funit,*)(tbhp(n,i),i=1,jbhp(n))
            write(funit,*)
            write(funit,*)(bhp(n,i),i=1,jbhp(n))
        endif

        if(jwo(n).gt.0) then
            funit=83+n-1
            title='TITLE="WATER-OIL RATIO, WELL '//tag//'"'
            open(funit,file=tecname14(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Water-Oil Ratio",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',jwo(n)
            write(funit,*)(two(n,i),i=1,jwo(n))
            write(funit,*)
            write(funit,*)(wor(n,i),i=1,jwo(n))
        endif

        if(jgo(n).gt.0) then
            funit=84+n-1
            title='TITLE="GAS-OIL RATIO, WELL '//tag//'"'
            open(funit,file=tecname15(n),status='new')
            write(funit,*) title
            write(funit,*)
     $           'VARIABLES="Time [days]","Gas-Oil Ratio",'
            write(funit,*)'ZONE F=BLOCK, T="", I =',jgo(n)
            write(funit,*)(tgo(n,i),i=1,jgo(n))
            write(funit,*)
            write(funit,*)(gor(n,i),i=1,jgo(n))
        endif
      enddo

      close(51)
      do i=1,MxW
         if(jwi(n).gt.0) close(76+n-1)
         if(jgi(n).gt.0) close(77+n-1)
         if(jop(n).gt.0) close(78+n-1)
         if(jwp(n).gt.0) close(79+n-1)
         if(jgp(n).gt.0) close(80+n-1)
         if(joi(n).gt.0) close(81+n-1)
         if(jbhp(n).gt.0) close(82+n-1)
         if(jwo(n).gt.0) close(83+n-1)
         if(jgo(n).gt.0) close(84+n-1)
      enddo

c 11   format(6 F15.6)
      end
