
C      -------------------------------------------------------------  
      SUBROUTINE UEXTERNALDB(LOP,LRESTART,TIME,DTIME,KSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
      
#include <SMAAspUserSubroutines.hdr>
      
C
      DIMENSION TIME(2)
      
      REAL :: k_WW, k_rr, k_M0, k_Dist_min, k_C0, k_CC, k_r0, k_rend, 
     1 k_rend_1, k_Volume, k_p, k_Max, k_Min, k_dal, k_nx0, k_ny0, 
     2 k_nz0, k_mu1, k_mu2  
      INTEGER :: K_Number_Elements, k_Number_Neighb, 
     1 K_Number_Elements_0, k_Npt
      common k_WW, k_rr, k_M0, k_Dist_min, K_Number_Elements, k_p, 
     1 k_C0, k_CC, k_r0, k_Number_Neighb, k_rend, k_rend_1, k_Volume,
     2 k_Max, k_Min, k_Npt, K_Number_Elements_0, k_dal, 
     3 k_nx0, k_ny0, k_nz0, k_mu1, k_mu2 
      
      
      character*256 OUTDIR
 
      IF (LOP.EQ.0) THEN

C---- Input          
          
        CALL GETOUTDIR( OUTDIR, LENOUTDIR )        
        OUTDIR = trim(OUTDIR) // trim('\Input_Data_Aniso.txt')

        open(unit=105, file=OUTDIR)
                
        read (105,*)
        read (105,*) k_p

        read (105,*)
        read (105,*) k_r0    

        read (105,*)
        read (105,*) k_rend

        read (105,*)
        read (105,*) k_C0
        
        read (105,*)
        read (105,*) k_CC
        
        read (105,*)
        read (105,*) k_dist_min
        
        read (105,*)
        read (105,*) k_Max
        
        read (105,*)
        read (105,*) k_Min     
        
        read (105,*)
        read (105,*) k_dal
        
        read (105,*)
        read (105,*) k_nx0 
        
        read (105,*)
        read (105,*) k_ny0 
        
        read (105,*)
        read (105,*) k_nz0       
       
        close(105)          
          
      
C-------------------------------        
        k_rend_1=k_rend
      
        K_Number_Elements=1
     
        k_Number_Neighb=1
        
        k_Npt=1
        
      END IF 
          
      
      RETURN
      END        

C      -------------------------------------------------------------  
      SUBROUTINE USDFLD(FIELD,STATEV,PNEWDT,DIRECT,T,CELENT,
     1 TIME,DTIME,CMNAME,ORNAME,NFIELD,NSTATV,NOEL,NPT,LAYER,
     2 KSPT,KSTEP,KINC,NDI,NSHR,COORD,JMAC,JMATYP,MATLAYO,LACCFLA)
C
      INCLUDE 'ABA_PARAM.INC'
      
#include <SMAAspUserSubroutines.hdr>
      
C
      CHARACTER*80 CMNAME,ORNAME
      CHARACTER*3  FLGRAY(15)
      DIMENSION FIELD(NFIELD),STATEV(NSTATV),DIRECT(3,3),
     1 T(3,3),TIME(2)
      DIMENSION ARRAY(15),JARRAY(15),JMAC(*),JMATYP(*),COORD(*)
      
      REAL :: k_WW, k_rr, k_M0, k_Dist_min, k_C0, k_CC, k_r0, k_rend, 
     1 k_rend_1, k_Volume, k_p, k_Max, k_Min, k_dal, k_nx0, k_ny0, 
     2 k_nz0, k_mu1, k_mu2  
      INTEGER :: K_Number_Elements, k_Number_Neighb, 
     1 K_Number_Elements_0, k_Npt
      common k_WW, k_rr, k_M0, k_Dist_min, K_Number_Elements, k_p, 
     1 k_C0, k_CC, k_r0, k_Number_Neighb, k_rend, k_rend_1, k_Volume,
     2 k_Max, k_Min, k_Npt, K_Number_Elements_0, k_dal, 
     3 k_nx0, k_ny0, k_nz0, k_mu1, k_mu2  
      
      DOUBLE PRECISION :: k_S(6), k_E(6), PS(3), PE(3), ANPS(3,3), 
     1 ANPE(3,3), k_EE(6), K_Ener(3), k_princ, k_vector(3), k_mod, 
     2 k_theta, k_phi, k_pi 

     
      real k_w(10)
      pointer(ptrk_w,k_w)
      
      real k_r(10)
      pointer(ptrk_r,k_r) 
      
      real k_Coord_1(10)
      pointer(ptrk_Coord_1,k_Coord_1) 
      
      real k_Coord_2(10)
      pointer(ptrk_Coord_2,k_Coord_2)
      
      real k_Coord_3(10)
      pointer(ptrk_Coord_3,k_Coord_3)  
      
      real k_Neighb(10)
      pointer(ptrk_Neighb,k_Neighb)
      
      real k_rd(10)
      pointer(ptrk_rd,k_rd) 
      

C----- S1-S6
      real k_S1(10)
      pointer(ptrS1,k_S1)   
      real k_S2(10)
      pointer(ptrS2,k_S2) 
      real k_S3(10)
      pointer(ptrS3,k_S3) 
      real k_S4(10)
      pointer(ptrS4,k_S4)             
      real k_S5(10)
      pointer(ptrS5,k_S5)  
      real k_S6(10)
      pointer(ptrS6,k_S6)  
      
C----- E1-E6      
      real k_E1(10)
      pointer(ptrE1,k_E1)   
      real k_E2(10)
      pointer(ptrE2,k_E2) 
      real k_E3(10)
      pointer(ptrE3,k_E3) 
      real k_E4(10)
      pointer(ptrE4,k_E4)             
      real k_E5(10)
      pointer(ptrE5,k_E5)  
      real k_E6(10)
      pointer(ptrE6,k_E6)  
      

   
C----- k_V
      real k_V(10)
      pointer(ptrk_V,k_V)      
 
C----- k_al      
      real k_nx(10)
      pointer(ptrk_nx,k_nx) 
      
      real k_ny(10)
      pointer(ptrk_ny,k_ny) 
      
      real k_nz(10)
      pointer(ptrk_nz,k_nz) 
     
      
      real k_nx_n1(10)
      pointer(ptrk_nx_n1,k_nx_n1) 
      
      real k_ny_n1(10)
      pointer(ptrk_ny_n1,k_ny_n1) 
      
      real k_nz_n1(10)
      pointer(ptrk_nz_n1,k_nz_n1) 

      real k_nx_n2(10)
      pointer(ptrk_nx_n2,k_nx_n2) 
      
      real k_ny_n2(10)
      pointer(ptrk_ny_n2,k_ny_n2) 
      
      real k_nz_n2(10)
      pointer(ptrk_nz_n2,k_nz_n2) 
      
      real k_nx_n3(10)
      pointer(ptrk_nx_n3,k_nx_n3) 
      
      real k_ny_n3(10)
      pointer(ptrk_ny_n3,k_ny_n3) 
      
      real k_nz_n3(10)
      pointer(ptrk_nz_n3,k_nz_n3) 

      real k_en1(10)
      pointer(ptrk_en1,k_en1)   
      
      real k_en2(10)
      pointer(ptrk_en2,k_en2)   
      
      real k_en3(10)
      pointer(ptrk_en3,k_en3)   
      
      real k_enmin(10)
      pointer(ptrk_enmin,k_enmin)
                  
      k_pi=3.141592653589793
      
      
      ptrS1 = SMAFloatArrayCreateSP(1,100,0.0)
      ptrS2 = SMAFloatArrayCreateSP(2,100,0.0)
      ptrS3 = SMAFloatArrayCreateSP(3,100,0.0)
      ptrS4 = SMAFloatArrayCreateSP(4,100,0.0)
      ptrS5 = SMAFloatArrayCreateSP(5,100,0.0)
      ptrS6 = SMAFloatArrayCreateSP(6,100,0.0)
   
      ptrE1 = SMAFloatArrayCreateSP(7,100,0.0)
      ptrE2 = SMAFloatArrayCreateSP(8,100,0.0)
      ptrE3 = SMAFloatArrayCreateSP(9,100,0.0)
      ptrE4 = SMAFloatArrayCreateSP(10,100,0.0)
      ptrE5 = SMAFloatArrayCreateSP(11,100,0.0)
      ptrE6 = SMAFloatArrayCreateSP(12,100,0.0)
     
      ptrk_V = SMAFloatArrayCreateSP(15,100,0.0)
      
      ptrk_nx = SMAFloatArrayCreateSP(22,100,0.0)
      
      ptrk_ny = SMAFloatArrayCreateSP(23,100,0.0)
      
      ptrk_nz = SMAFloatArrayCreateSP(24,100,0.0)
      
      ptrk_nx_n1 = SMAFloatArrayCreateSP(25,100,0.0)
      
      ptrk_ny_n1 = SMAFloatArrayCreateSP(26,100,0.0)
      
      ptrk_nz_n1 = SMAFloatArrayCreateSP(27,100,0.0)
      
      ptrk_nx_n2 = SMAFloatArrayCreateSP(28,100,0.0)
      
      ptrk_ny_n2 = SMAFloatArrayCreateSP(29,100,0.0)
      
      ptrk_nz_n2 = SMAFloatArrayCreateSP(30,100,0.0)      

      ptrk_nx_n3 = SMAFloatArrayCreateSP(31,100,0.0)
      
      ptrk_ny_n3 = SMAFloatArrayCreateSP(32,100,0.0)
      
      ptrk_nz_n3 = SMAFloatArrayCreateSP(33,100,0.0) 

      ptrk_en1 = SMAFloatArrayCreateSP(34,100,0.0)
      
      ptrk_en2 = SMAFloatArrayCreateSP(35,100,0.0)
      
      ptrk_en3 = SMAFloatArrayCreateSP(36,100,0.0)     
      
      ptrk_enmin = SMAFloatArrayCreateSP(37,100,0.0)   
  
      
C     KSTEP=1 
      
      IF (KSTEP.EQ.1) THEN 
      
      FIELD(1)=k_r0
      
      FIELD(2)=0
      
      FIELD(3)=0  
      
      IF (NOEL.GE.K_Number_Elements_0) THEN
           K_Number_Elements_0=NOEL
      END IF
        
      IF (Npt.GE.k_Npt) THEN
           k_Npt=Npt
      END IF
        
      K_Number_Elements=K_Number_Elements_0*k_Npt
      
      END IF
      
C     KSTEP=2 
      
      IF (KSTEP.EQ.2) THEN 
      
C     KINC=1
      
      ptrS1 = SMAFloatArrayCreateSP(1,K_Number_Elements,0.0)
      ptrS2 = SMAFloatArrayCreateSP(2,K_Number_Elements,0.0)
      ptrS3 = SMAFloatArrayCreateSP(3,K_Number_Elements,0.0)
      ptrS4 = SMAFloatArrayCreateSP(4,K_Number_Elements,0.0)
      ptrS5 = SMAFloatArrayCreateSP(5,K_Number_Elements,0.0)
      ptrS6 = SMAFloatArrayCreateSP(6,K_Number_Elements,0.0)
      
      ptrE1 = SMAFloatArrayCreateSP(7,K_Number_Elements,0.0)
      ptrE2 = SMAFloatArrayCreateSP(8,K_Number_Elements,0.0)
      ptrE3 = SMAFloatArrayCreateSP(9,K_Number_Elements,0.0)
      ptrE4 = SMAFloatArrayCreateSP(10,K_Number_Elements,0.0)
      ptrE5 = SMAFloatArrayCreateSP(11,K_Number_Elements,0.0)
      ptrE6 = SMAFloatArrayCreateSP(12,K_Number_Elements,0.0)
    
      ptrk_V = SMAFloatArrayCreateSP(15,K_Number_Elements,0.0)
      
      ptrk_nx = SMAFloatArrayCreateSP(22,K_Number_Elements,0.0)
      
      ptrk_ny = SMAFloatArrayCreateSP(23,K_Number_Elements,0.0)
      
      ptrk_nz = SMAFloatArrayCreateSP(24,K_Number_Elements,0.0)
      
      ptrk_nx_n1 = SMAFloatArrayCreateSP(25,K_Number_Elements,0.0)
      
      ptrk_ny_n1 = SMAFloatArrayCreateSP(26,K_Number_Elements,0.0)
      
      ptrk_nz_n1 = SMAFloatArrayCreateSP(27,K_Number_Elements,0.0)
      
      ptrk_nx_n2 = SMAFloatArrayCreateSP(28,K_Number_Elements,0.0)
      
      ptrk_ny_n2 = SMAFloatArrayCreateSP(29,K_Number_Elements,0.0)
      
      ptrk_nz_n2 = SMAFloatArrayCreateSP(30,K_Number_Elements,0.0)      

      ptrk_nx_n3 = SMAFloatArrayCreateSP(31,K_Number_Elements,0.0)
      
      ptrk_ny_n3 = SMAFloatArrayCreateSP(32,K_Number_Elements,0.0)
      
      ptrk_nz_n3 = SMAFloatArrayCreateSP(33,K_Number_Elements,0.0) 
      
      ptrk_en1 = SMAFloatArrayCreateSP(34,K_Number_Elements,0.0)
      
      ptrk_en2 = SMAFloatArrayCreateSP(35,K_Number_Elements,0.0)
      
      ptrk_en3 = SMAFloatArrayCreateSP(36,K_Number_Elements,0.0) 
      
      ptrk_enmin = SMAFloatArrayCreateSP(37,K_Number_Elements,0.0)   
      
      
C------ GET S and E      
      CALL GETVRM('S',ARRAY,JARRAY,FLGRAY,JRCD,JMAC,JMATYP,
     1 MATLAYO,LACCFLA)
     
      k_S(1)=ARRAY(1) 
      k_S(2)=ARRAY(2)
      k_S(3)=ARRAY(3)
      k_S(4)=ARRAY(4)      
      k_S(5)=ARRAY(5)
      k_S(6)=ARRAY(6)  
      
      
      k_S1((NOEL-1)*k_Npt+Npt)=k_S(1)
      k_S2((NOEL-1)*k_Npt+Npt)=k_S(2)
      k_S3((NOEL-1)*k_Npt+Npt)=k_S(3)
      k_S4((NOEL-1)*k_Npt+Npt)=k_S(4)
      k_S5((NOEL-1)*k_Npt+Npt)=k_S(5)
      k_S6((NOEL-1)*k_Npt+Npt)=k_S(6)
      
      
       CALL GETVRM('E',ARRAY,JARRAY,FLGRAY,JRCD,JMAC,JMATYP,
     1 MATLAYO,LACCFLA)
     
      k_E1((NOEL-1)*k_Npt+Npt)=ARRAY(1) 
      k_E2((NOEL-1)*k_Npt+Npt)=ARRAY(2)
      k_E3((NOEL-1)*k_Npt+Npt)=ARRAY(3)
      k_E4((NOEL-1)*k_Npt+Npt)=ARRAY(4)      
      k_E5((NOEL-1)*k_Npt+Npt)=ARRAY(5)
      k_E6((NOEL-1)*k_Npt+Npt)=ARRAY(6)  
      
       CALL GETVRM('IVOL',ARRAY,JARRAY,FLGRAY,JRCD,JMAC,JMATYP,
     1 MATLAYO,LACCFLA)
      
      k_V((NOEL-1)*k_Npt+Npt)=ARRAY(1) 

C------ GET Princapal value         
      
      LSTR = 1

      CALL SPRIND(k_S,PS,ANPS,LSTR,NDI,NSHR)  

      
      k_nx_n1((NOEL-1)*k_Npt+Npt)=ANPS(1,1)
      k_ny_n1((NOEL-1)*k_Npt+Npt)=ANPS(1,2)
      k_nz_n1((NOEL-1)*k_Npt+Npt)=ANPS(1,3)
    
      k_nx_n2((NOEL-1)*k_Npt+Npt)=ANPS(2,1)
      k_ny_n2((NOEL-1)*k_Npt+Npt)=ANPS(2,2)
      k_nz_n2((NOEL-1)*k_Npt+Npt)=ANPS(2,3)      
      
      k_nx_n3((NOEL-1)*k_Npt+Npt)=ANPS(3,1)
      k_ny_n3((NOEL-1)*k_Npt+Npt)=ANPS(3,2)
      k_nz_n3((NOEL-1)*k_Npt+Npt)=ANPS(3,3)      
      
  
      IF (KINC.EQ.1) THEN
        
        IF (NOEL.GE.K_Number_Elements_0) THEN
           K_Number_Elements_0=NOEL
        END IF
        
        IF (Npt.GE.k_Npt) THEN
           k_Npt=Npt
        END IF
        
        K_Number_Elements=K_Number_Elements_0*k_Npt
        
        FIELD(1)=k_r0**k_p
        FIELD(2)=0
        FIELD(3)=0
        

      END IF 
      
C     KINC=2
      
      IF (KINC.EQ.2) THEN
    
      ptrk_Coord_1 = SMAFloatArrayCreateSP(17,K_Number_Elements,0.0)
      
      ptrk_Coord_2 = SMAFloatArrayCreateSP(18,K_Number_Elements,0.0)
      
      ptrk_Coord_3 = SMAFloatArrayCreateSP(19,K_Number_Elements,0.0)
      
      k_Coord_1((NOEL-1)*k_Npt+Npt)=COORD(1)
      k_Coord_2((NOEL-1)*k_Npt+Npt)=COORD(2)
      k_Coord_3((NOEL-1)*k_Npt+Npt)=COORD(3)
      
      ptrk_nx = SMAFloatArrayCreateSP(22,K_Number_Elements,0.0)
      
      ptrk_ny = SMAFloatArrayCreateSP(23,K_Number_Elements,0.0)
      
      ptrk_nz = SMAFloatArrayCreateSP(24,K_Number_Elements,0.0)
      
      ptrk_nx_n1 = SMAFloatArrayCreateSP(25,K_Number_Elements,0.0)
      
      ptrk_ny_n1 = SMAFloatArrayCreateSP(26,K_Number_Elements,0.0)
      
      ptrk_nz_n1 = SMAFloatArrayCreateSP(27,K_Number_Elements,0.0)
      
      ptrk_nx_n2 = SMAFloatArrayCreateSP(28,K_Number_Elements,0.0)
      
      ptrk_ny_n2 = SMAFloatArrayCreateSP(29,K_Number_Elements,0.0)
      
      ptrk_nz_n2 = SMAFloatArrayCreateSP(30,K_Number_Elements,0.0)      

      ptrk_nx_n3 = SMAFloatArrayCreateSP(31,K_Number_Elements,0.0)
      
      ptrk_ny_n3 = SMAFloatArrayCreateSP(32,K_Number_Elements,0.0)
      
      ptrk_nz_n3 = SMAFloatArrayCreateSP(33,K_Number_Elements,0.0) 
      
      
      ptrk_en1 = SMAFloatArrayCreateSP(34,K_Number_Elements,0.0)
      
      ptrk_en2 = SMAFloatArrayCreateSP(35,K_Number_Elements,0.0)
      
      ptrk_en3 = SMAFloatArrayCreateSP(36,K_Number_Elements,0.0)  
      ptrk_enmin = SMAFloatArrayCreateSP(37,K_Number_Elements,0.0)  


       STATEV(1)=k_r0
       
       STATEV(2)=k_r0
       
       STATEV(3)=k_nx((NOEL-1)*k_Npt+Npt)
       
       STATEV(4)=k_ny((NOEL-1)*k_Npt+Npt)
       
       STATEV(5)=k_nz((NOEL-1)*k_Npt+Npt)
       
            

       FIELD(1)=k_r0
       
       FIELD(2)=0
      
       FIELD(3)=0

      
      END IF 
      
 
C     KINC>2     
      IF (KINC.GT.2) THEN    
    
      ptrk_r = SMAFloatArrayCreateSP(16,K_Number_Elements,0.0)
      ptrk_rd = SMAFloatArrayCreateSP(21,K_Number_Elements,0.0)
      
      ptrk_nx = SMAFloatArrayCreateSP(22,K_Number_Elements,0.0)
      
      ptrk_ny = SMAFloatArrayCreateSP(23,K_Number_Elements,0.0)
      
      ptrk_nz = SMAFloatArrayCreateSP(24,K_Number_Elements,0.0)
      
      
      ptrS1 = SMAFloatArrayCreateSP(1,K_Number_Elements,0.0)
      ptrS2 = SMAFloatArrayCreateSP(2,K_Number_Elements,0.0)
      ptrS3 = SMAFloatArrayCreateSP(3,K_Number_Elements,0.0)
      ptrS4 = SMAFloatArrayCreateSP(4,K_Number_Elements,0.0)
      ptrS5 = SMAFloatArrayCreateSP(5,K_Number_Elements,0.0)
      ptrS6 = SMAFloatArrayCreateSP(6,K_Number_Elements,0.0)
      
      ptrE1 = SMAFloatArrayCreateSP(7,K_Number_Elements,0.0)
      ptrE2 = SMAFloatArrayCreateSP(8,K_Number_Elements,0.0)
      ptrE3 = SMAFloatArrayCreateSP(9,K_Number_Elements,0.0)
      ptrE4 = SMAFloatArrayCreateSP(10,K_Number_Elements,0.0)
      ptrE5 = SMAFloatArrayCreateSP(11,K_Number_Elements,0.0)
      ptrE6 = SMAFloatArrayCreateSP(12,K_Number_Elements,0.0)
      
      
      ptrk_nx_n1 = SMAFloatArrayCreateSP(25,K_Number_Elements,0.0)
      
      ptrk_ny_n1 = SMAFloatArrayCreateSP(26,K_Number_Elements,0.0)
      
      ptrk_nz_n1 = SMAFloatArrayCreateSP(27,K_Number_Elements,0.0)
      
      ptrk_nx_n2 = SMAFloatArrayCreateSP(28,K_Number_Elements,0.0)
      
      ptrk_ny_n2 = SMAFloatArrayCreateSP(29,K_Number_Elements,0.0)
      
      ptrk_nz_n2 = SMAFloatArrayCreateSP(30,K_Number_Elements,0.0)      

      ptrk_nx_n3 = SMAFloatArrayCreateSP(31,K_Number_Elements,0.0)
      
      ptrk_ny_n3 = SMAFloatArrayCreateSP(32,K_Number_Elements,0.0)
      
      ptrk_nz_n3 = SMAFloatArrayCreateSP(33,K_Number_Elements,0.0) 
      
      ptrk_enmin = SMAFloatArrayCreateSP(37,K_Number_Elements,0.0)  
      
      ptrk_en1 = SMAFloatArrayCreateSP(34,K_Number_Elements,0.0)
      
      ptrk_en2 = SMAFloatArrayCreateSP(35,K_Number_Elements,0.0)
      
      ptrk_en3 = SMAFloatArrayCreateSP(36,K_Number_Elements,0.0)  
   
       STATEV(1)=k_r((NOEL-1)*k_Npt+Npt)
       
       STATEV(2)=k_rd((NOEL-1)*k_Npt+Npt)
       
       STATEV(3)=k_mu1 
       
       STATEV(4)=k_mu2
       
       STATEV(5)=k_rend_1
       
    
        STATEV(6)=k_rr
        
        STATEV(7)=k_WW
       
       IF (TIME(2).LE.200) THEN
       
           FIELD(1)=STATEV(2)**(1+(k_p-1.0)*TIME(2)/200.0)
           
       ELSE
        
          FIELD(1)=STATEV(2)**k_p
       
       END IF
       
C       FIELD(2)=k_nx((NOEL-1)*k_Npt+Npt)
       
       IF (k_nx((NOEL-1)*k_Npt+Npt).GT.0.999) THEN
           
           k_theta=0
           k_phi=0
           
       ELSE
           
      
       k_theta=acos(k_nx((NOEL-1)*k_Npt+Npt))*180.0/k_pi 
       
       IF ((k_ny((NOEL-1)*k_Npt+Npt).EQ.0).AND.
     1 (k_nz((NOEL-1)*k_Npt+Npt).EQ.0)) THEN
           
           k_phi=0
           
      ELSE
          
          k_phi=atan2(k_nz((NOEL-1)*k_Npt+Npt),k_ny((NOEL-1)*k_Npt+Npt))
          
          k_phi=k_phi*180.0/k_pi 
          
      END IF
      
      END IF
      
       
       FIELD(2)=k_theta
       FIELD(3)=k_phi

      END IF  

      END IF
      
      
      RETURN
      END
      
C-------------------------------------------------------------
      SUBROUTINE URDFIL(LSTOP,LOVRWRT,KSTEP,KINC,DTIME,TIME)
C
      INCLUDE 'ABA_PARAM.INC'

#include <SMAAspUserSubroutines.hdr>
 
C 
      DIMENSION ARRAY(513),JRRAY(NPRECD,513),TIME(2)
      EQUIVALENCE (ARRAY(1),JRRAY(1,1))
  
      REAL :: k_WW, k_rr, k_M0, k_Dist_min, k_C0, k_CC, k_r0, k_rend, 
     1 k_rend_1, k_Volume, k_p, k_Max, k_Min, k_dal, k_nx0, k_ny0, 
     2 k_nz0, k_mu1, k_mu2  
      INTEGER :: K_Number_Elements, k_Number_Neighb, 
     1 K_Number_Elements_0, k_Npt
      common k_WW, k_rr, k_M0, k_Dist_min, K_Number_Elements, k_p, 
     1 k_C0, k_CC, k_r0, k_Number_Neighb, k_rend, k_rend_1, k_Volume,
     2 k_Max, k_Min, k_Npt, K_Number_Elements_0, k_dal, 
     3 k_nx0, k_ny0, k_nz0, k_mu1, k_mu2  
 
      DOUBLE PRECISION :: k_ener, k_C1, k_Dist, k_Number_Neighb_0, 
     1 k_delta_2, k_axis_act(3), k_pr_1(3), k_pr_2(3), k_pr_3(3),
     2 k_pr_rot_1(3), k_pr_rot_2(3), k_pr_rot_3(3), k_rdd,  k_delta,
     3 k_vector_rotate(3), k_sign, k_mod, k_mat_rotate(3,3),
     4 k_nx_sum, k_ny_sum, k_nz_sum, k_S(6), k_en(3), k_mid, k_move 
      INTEGER :: k_i, k_j, k_N, k_F, k_ii

C----- S1-S6
      real k_S1(10)
      pointer(ptrS1,k_S1)   
      real k_S2(10)
      pointer(ptrS2,k_S2) 
      real k_S3(10)
      pointer(ptrS3,k_S3) 
      real k_S4(10)
      pointer(ptrS4,k_S4)             
      real k_S5(10)
      pointer(ptrS5,k_S5)  
      real k_S6(10)
      pointer(ptrS6,k_S6)  
      
C----- E1-E6      
      real k_E1(10)
      pointer(ptrE1,k_E1)   
      real k_E2(10)
      pointer(ptrE2,k_E2) 
      real k_E3(10)
      pointer(ptrE3,k_E3) 
      real k_E4(10)
      pointer(ptrE4,k_E4)             
      real k_E5(10)
      pointer(ptrE5,k_E5)  
      real k_E6(10)
      pointer(ptrE6,k_E6)  
      
C----- k_w      
      real k_w(10)
      pointer(ptrk_w,k_w)
      
C----- k_C
      real k_C(10)
      pointer(ptrk_C,k_C)
   
C----- k_V
      real k_V(10)
      pointer(ptrk_V,k_V)      
      
C----- k_r
      real k_r(10)
      pointer(ptrk_r,k_r) 

C----- k_Coord      
      real k_Coord_1(10)
      pointer(ptrk_Coord_1,k_Coord_1) 
      
      real k_Coord_2(10)
      pointer(ptrk_Coord_2,k_Coord_2)
      
      real k_Coord_3(10)
      pointer(ptrk_Coord_3,k_Coord_3)
      
C-----   k_Neighb          
      real k_Neighb(10)
      pointer(ptrk_Neighb,k_Neighb)
      
C-----   k_rd        
      real k_rd(10)
      pointer(ptrk_rd,k_rd) 
      
C----- Angle
      
      real k_nx(10)
      pointer(ptrk_nx,k_nx) 
      
      real k_ny(10)
      pointer(ptrk_ny,k_ny) 
      
      real k_nz(10)
      pointer(ptrk_nz,k_nz) 
      
      real k_nx_pr(10)
      pointer(ptrk_nx_pr,k_nx_pr) 
      
      real k_ny_pr(10)
      pointer(ptrk_ny_pr,k_ny_pr) 
      
      real k_nz_pr(10)
      pointer(ptrk_nz_pr,k_nz_pr) 
      
      real k_nx_n1(10)
      pointer(ptrk_nx_n1,k_nx_n1) 
      
      real k_ny_n1(10)
      pointer(ptrk_ny_n1,k_ny_n1) 
      
      real k_nz_n1(10)
      pointer(ptrk_nz_n1,k_nz_n1) 

      real k_nx_n2(10)
      pointer(ptrk_nx_n2,k_nx_n2) 
      
      real k_ny_n2(10)
      pointer(ptrk_ny_n2,k_ny_n2) 
      
      real k_nz_n2(10)
      pointer(ptrk_nz_n2,k_nz_n2) 
      
      real k_nx_n3(10)
      pointer(ptrk_nx_n3,k_nx_n3) 
      
      real k_ny_n3(10)
      pointer(ptrk_ny_n3,k_ny_n3) 
      
      real k_nz_n3(10)
      pointer(ptrk_nz_n3,k_nz_n3)     
      
      real k_en1(10)
      pointer(ptrk_en1,k_en1)   
      
      real k_en2(10)
      pointer(ptrk_en2,k_en2)   
      
      real k_en3(10)
      pointer(ptrk_en3,k_en3)   
      
      real k_enmin(10)
      pointer(ptrk_enmin,k_enmin)   
      
      real k_rc(10)
      pointer(ptrk_rc,k_rc)      
      
      real k_rdc(10)
      pointer(ptrk_rdc,k_rdc)       

C----- k_C
      real k_Cn(10)
      pointer(ptrk_Cn,k_Cn)      
      
      
      interface

       function Rotate_Vector(nr0,n0,ksi) result(f)
        DOUBLE PRECISION, dimension(3), intent(in):: nr0
        DOUBLE PRECISION, dimension(3), intent(in):: n0 
        DOUBLE PRECISION, intent(in)              :: ksi        
        DOUBLE PRECISION, dimension(3)            :: f
       end function Rotate_Vector

       function k_Energy(s,n) result(E)
        DOUBLE PRECISION, dimension(6), intent(in):: s
        DOUBLE PRECISION, dimension(3), intent(in):: n
       DOUBLE PRECISION                           :: E       
       end function k_Energy


      end interface
      
      
      
      LOVRWRT=1
      
      ptrS1 = SMAFloatArrayCreateSP(1,100,0.0)
      ptrS2 = SMAFloatArrayCreateSP(2,100,0.0)
      ptrS3 = SMAFloatArrayCreateSP(3,100,0.0)
      ptrS4 = SMAFloatArrayCreateSP(4,100,0.0)
      ptrS5 = SMAFloatArrayCreateSP(5,100,0.0)
      ptrS6 = SMAFloatArrayCreateSP(6,100,0.0)
   
      ptrE1 = SMAFloatArrayCreateSP(7,100,0.0)
      ptrE2 = SMAFloatArrayCreateSP(8,100,0.0)
      ptrE3 = SMAFloatArrayCreateSP(9,100,0.0)
      ptrE4 = SMAFloatArrayCreateSP(10,100,0.0)
      ptrE5 = SMAFloatArrayCreateSP(11,100,0.0)
      ptrE6 = SMAFloatArrayCreateSP(12,100,0.0)
      
      ptrk_w = SMAFloatArrayCreateSP(13,100,0.0)
      
      ptrk_C = SMAFloatArrayCreateSP(14,100,0.0)
      
      ptrk_V = SMAFloatArrayCreateSP(15,100,0.0)
      
      ptrk_r = SMAFloatArrayCreateSP(16,100,0.0)
      
      ptrk_Coord_1 = SMAFloatArrayCreateSP(17,100,0.0)
      
      ptrk_Coord_2 = SMAFloatArrayCreateSP(18,100,0.0)
      
      ptrk_Coord_3 = SMAFloatArrayCreateSP(19,100,0.0)
      
      ptrk_Neighb = SMAFloatArrayCreateSP(20,100,0.0)
      
      ptrk_rd = SMAFloatArrayCreateSP(21,100,0.0)
      
      ptrk_nx = SMAFloatArrayCreateSP(22,100,0.0)
      
      ptrk_ny = SMAFloatArrayCreateSP(23,100,0.0)
      
      ptrk_nz = SMAFloatArrayCreateSP(24,100,0.0)
      
       ptrk_nx_n1 = SMAFloatArrayCreateSP(25,100,0.0)
      
      ptrk_ny_n1 = SMAFloatArrayCreateSP(26,100,0.0)
      
      ptrk_nz_n1 = SMAFloatArrayCreateSP(27,100,0.0)
      
      ptrk_nx_n2 = SMAFloatArrayCreateSP(28,100,0.0)
      
      ptrk_ny_n2 = SMAFloatArrayCreateSP(29,100,0.0)
      
      ptrk_nz_n2 = SMAFloatArrayCreateSP(30,100,0.0)      

      ptrk_nx_n3 = SMAFloatArrayCreateSP(31,100,0.0)
      
      ptrk_ny_n3 = SMAFloatArrayCreateSP(32,100,0.0)
      
      ptrk_nz_n3 = SMAFloatArrayCreateSP(33,100,0.0)     
      
      ptrk_en1 = SMAFloatArrayCreateSP(34,100,0.0)
      
      ptrk_en2 = SMAFloatArrayCreateSP(35,100,0.0)
      
      ptrk_en3 = SMAFloatArrayCreateSP(36,100,0.0)   
      
      ptrk_enmin = SMAFloatArrayCreateSP(37,100,0.0)  
      
      ptrk_rc = SMAFloatArrayCreateSP(38,100,0.0)      

      ptrk_rdc = SMAFloatArrayCreateSP(39,100,0.0)         
      
      ptrk_Cn = SMAFloatArrayCreateSP(40,100,0.0)         
      
      IF (KSTEP.EQ.2) THEN
      
            IF (KINC.GT.1) THEN
      
      ptrS1 = SMAFloatArrayCreateSP(1,K_Number_Elements,0.0)
      ptrS2 = SMAFloatArrayCreateSP(2,K_Number_Elements,0.0)
      ptrS3 = SMAFloatArrayCreateSP(3,K_Number_Elements,0.0)
      ptrS4 = SMAFloatArrayCreateSP(4,K_Number_Elements,0.0)
      ptrS5 = SMAFloatArrayCreateSP(5,K_Number_Elements,0.0)
      ptrS6 = SMAFloatArrayCreateSP(6,K_Number_Elements,0.0)
      
      ptrE1 = SMAFloatArrayCreateSP(7,K_Number_Elements,0.0)
      ptrE2 = SMAFloatArrayCreateSP(8,K_Number_Elements,0.0)
      ptrE3 = SMAFloatArrayCreateSP(9,K_Number_Elements,0.0)
      ptrE4 = SMAFloatArrayCreateSP(10,K_Number_Elements,0.0)
      ptrE5 = SMAFloatArrayCreateSP(11,K_Number_Elements,0.0)
      ptrE6 = SMAFloatArrayCreateSP(12,K_Number_Elements,0.0)
      
      ptrk_w = SMAFloatArrayCreateSP(13,K_Number_Elements,0.0)
      
      ptrk_C = SMAFloatArrayCreateSP(14,K_Number_Elements,0.0)
      
      ptrk_V = SMAFloatArrayCreateSP(15,K_Number_Elements,0.0)
      
      ptrk_r = SMAFloatArrayCreateSP(16,K_Number_Elements,0.0)
      
      ptrk_Coord_1 = SMAFloatArrayCreateSP(17,K_Number_Elements,0.0)
      
      ptrk_Coord_2 = SMAFloatArrayCreateSP(18,K_Number_Elements,0.0)
      
      ptrk_Coord_3 = SMAFloatArrayCreateSP(19,K_Number_Elements,0.0)
      
      ptrk_Neighb = SMAFloatArrayCreateSP(20,K_Number_Elements,0.0)
      
      ptrk_rd = SMAFloatArrayCreateSP(21,K_Number_Elements,0.0)    
 
      ptrk_nx = SMAFloatArrayCreateSP(22,K_Number_Elements,0.0)
      
      ptrk_ny = SMAFloatArrayCreateSP(23,K_Number_Elements,0.0)
      
      ptrk_nz = SMAFloatArrayCreateSP(24,K_Number_Elements,0.0)
      
      ptrk_nx_pr = SMAFloatArrayCreateSP(25,K_Number_Elements,0.0)
      
      ptrk_ny_pr = SMAFloatArrayCreateSP(26,K_Number_Elements,0.0)
      
      ptrk_nz_pr = SMAFloatArrayCreateSP(27,K_Number_Elements,0.0)      
      
            
      ptrk_en1 = SMAFloatArrayCreateSP(34,K_Number_Elements,0.0)
      
      ptrk_en2 = SMAFloatArrayCreateSP(35,K_Number_Elements,0.0)
      
      ptrk_en3 = SMAFloatArrayCreateSP(36,K_Number_Elements,0.0)   
      
      ptrk_enmin = SMAFloatArrayCreateSP(37,K_Number_Elements,0.0)  
      
      ptrk_rc = SMAFloatArrayCreateSP(38,K_Number_Elements,0.0)      

      ptrk_rdc = SMAFloatArrayCreateSP(39,K_Number_Elements,0.0)          
      
      ptrk_Cn = SMAFloatArrayCreateSP(40,K_Number_Elements,0.0)  
      
      
      END IF

      CALL POSFIL(KSTEP,KINC,ARRAY,JRCD)
C
      N=0
      KK=0
      KKK=0
      

      DO K1=1,999999
         CALL DBFILE(0,JRRAY,JRCD)
        IF (JRCD .NE. 0) GO TO 110
        KEY=JRRAY(1,2)
      
C----- k_V      
      IF (KINC.LE.1) GO TO 110 
      
        IF (KEY.EQ.76) THEN
            KKK=KKK+1
                   
            IF (KKK.LE.K_Number_Elements+1) THEN
                
                    k_V(KKK)=array(3)
C                      k_V(KKK)=0.125
     
            END IF
               
        END IF         
          
C----- k_S      
        IF (KEY.EQ.11) THEN
            N=N+1
C       3 - s11
C       4 - s22 
C       5 - s33
C       6 - s12
C       7 - s13 
C       8 - s23                     
            IF (N.LE.K_Number_Elements+1) THEN
C                k_S1(N)=array(3)
C                k_S2(N)=array(4)
C                k_S3(N)=array(5)
C                k_S4(N)=array(6)
C                k_S5(N)=array(7)
C                k_S6(N)=array(8)
                
   
            END IF
               
        END IF        

C----- k_E        
        IF (KEY.EQ.21) THEN
            KK=KK+1
C       3 - e11
C       4 - e22 
C       5 - e33
C       6 - e12
C       7 - e13 
C       8 - e23                     
            IF (KK.LE.K_Number_Elements+1) THEN
C                k_E1(KK)=array(3)
C                k_E2(KK)=array(4)
C                k_E3(KK)=array(5)
C                k_E4(KK)=array(6)
C                k_E5(KK)=array(7)
C                k_E6(KK)=array(8)
    
            END IF
               
        END IF        
          
      IF (KKK.EQ.K_Number_Elements+2) GO TO 110          
      IF (N.EQ.K_Number_Elements+2) GO TO 110 
      IF (KK.EQ.K_Number_Elements+1) GO TO 110
       
      END DO
      
  110 CONTINUE
      
      
C     Finding stress and strain  
      IF (KINC.GT.1) THEN
      
      ptrS1 = SMAFloatArrayCreateSP(1,K_Number_Elements,0.0)
      ptrS2 = SMAFloatArrayCreateSP(2,K_Number_Elements,0.0)
      ptrS3 = SMAFloatArrayCreateSP(3,K_Number_Elements,0.0)
      ptrS4 = SMAFloatArrayCreateSP(4,K_Number_Elements,0.0)
      ptrS5 = SMAFloatArrayCreateSP(5,K_Number_Elements,0.0)
      ptrS6 = SMAFloatArrayCreateSP(6,K_Number_Elements,0.0)
      
      ptrE1 = SMAFloatArrayCreateSP(7,K_Number_Elements,0.0)
      ptrE2 = SMAFloatArrayCreateSP(8,K_Number_Elements,0.0)
      ptrE3 = SMAFloatArrayCreateSP(9,K_Number_Elements,0.0)
      ptrE4 = SMAFloatArrayCreateSP(10,K_Number_Elements,0.0)
      ptrE5 = SMAFloatArrayCreateSP(11,K_Number_Elements,0.0)
      ptrE6 = SMAFloatArrayCreateSP(12,K_Number_Elements,0.0)
      
      ptrk_w = SMAFloatArrayCreateSP(13,K_Number_Elements,0.0)
      
      ptrk_C = SMAFloatArrayCreateSP(14,K_Number_Elements,0.0)
      
      ptrk_V = SMAFloatArrayCreateSP(15,K_Number_Elements,0.0)
      
      ptrk_r = SMAFloatArrayCreateSP(16,K_Number_Elements,0.0)
      
      ptrk_Coord_1 = SMAFloatArrayCreateSP(17,K_Number_Elements,0.0)
      
      ptrk_Coord_2 = SMAFloatArrayCreateSP(18,K_Number_Elements,0.0)
      
      ptrk_Coord_3 = SMAFloatArrayCreateSP(19,K_Number_Elements,0.0)
      
      ptrk_Neighb = SMAFloatArrayCreateSP(20,K_Number_Elements,0.0)
      
      ptrk_rd = SMAFloatArrayCreateSP(21,K_Number_Elements,0.0)    
 
      ptrk_nx = SMAFloatArrayCreateSP(22,K_Number_Elements,0.0)
      
      ptrk_ny = SMAFloatArrayCreateSP(23,K_Number_Elements,0.0)
      
      ptrk_nz = SMAFloatArrayCreateSP(24,K_Number_Elements,0.0)
      
      ptrk_nx_n1 = SMAFloatArrayCreateSP(25,K_Number_Elements,0.0)
      
      ptrk_ny_n1 = SMAFloatArrayCreateSP(26,K_Number_Elements,0.0)
      
      ptrk_nz_n1 = SMAFloatArrayCreateSP(27,K_Number_Elements,0.0)
      
      ptrk_nx_n2 = SMAFloatArrayCreateSP(28,K_Number_Elements,0.0)
      
      ptrk_ny_n2 = SMAFloatArrayCreateSP(29,K_Number_Elements,0.0)
      
      ptrk_nz_n2 = SMAFloatArrayCreateSP(30,K_Number_Elements,0.0)      

      ptrk_nx_n3 = SMAFloatArrayCreateSP(31,K_Number_Elements,0.0)
      
      ptrk_ny_n3 = SMAFloatArrayCreateSP(32,K_Number_Elements,0.0)
      
      ptrk_nz_n3 = SMAFloatArrayCreateSP(33,K_Number_Elements,0.0)    
            
      ptrk_en1 = SMAFloatArrayCreateSP(34,K_Number_Elements,0.0)
      
      ptrk_en2 = SMAFloatArrayCreateSP(35,K_Number_Elements,0.0)
      
      ptrk_en3 = SMAFloatArrayCreateSP(36,K_Number_Elements,0.0)   
      
      ptrk_enmin = SMAFloatArrayCreateSP(37,K_Number_Elements,0.0) 
 
      ptrk_rc = SMAFloatArrayCreateSP(38,K_Number_Elements,0.0)      

      ptrk_rdc = SMAFloatArrayCreateSP(39,K_Number_Elements,0.0) 
      
      ptrk_Cn = SMAFloatArrayCreateSP(40,K_Number_Elements,0.0)  
      
      
      END IF


C-----KINC=1

      IF (KINC.EQ.1) THEN
      
      ptrk_w = SMAFloatArrayCreateSP(13,K_Number_Elements,0.0)         
      ptrk_C = SMAFloatArrayCreateSP(14,K_Number_Elements,0.0)   
      ptrk_r = SMAFloatArrayCreateSP(16,K_Number_Elements,0.0) 
      ptrk_rd = SMAFloatArrayCreateSP(21,K_Number_Elements,0.0) 
      ptrk_V = SMAFloatArrayCreateSP(15,K_Number_Elements,0.0)
      
      ptrk_nx = SMAFloatArrayCreateSP(22,K_Number_Elements,0.0)
      ptrk_ny = SMAFloatArrayCreateSP(23,K_Number_Elements,0.0)
      ptrk_nz = SMAFloatArrayCreateSP(24,K_Number_Elements,0.0)
      
            
      ptrk_en1 = SMAFloatArrayCreateSP(34,K_Number_Elements,0.0)
      
      ptrk_en2 = SMAFloatArrayCreateSP(35,K_Number_Elements,0.0)
      
      ptrk_en3 = SMAFloatArrayCreateSP(36,K_Number_Elements,0.0)   
      
      ptrk_enmin = SMAFloatArrayCreateSP(37,K_Number_Elements,0.0) 
      
       ptrk_rc = SMAFloatArrayCreateSP(38,K_Number_Elements,0.0)      

      ptrk_rdc = SMAFloatArrayCreateSP(39,K_Number_Elements,0.0)     

      ptrk_Cn = SMAFloatArrayCreateSP(40,K_Number_Elements,0.0)       
     
       DO j=1, K_Number_Elements
        
           k_C(j)=k_C0
           k_r(j)=k_r0
           k_rd(j)=k_r0
           k_w(j)=0 
           
           k_V(j)=1.0  
               
           k_nx(j)=k_nx0
           k_ny(j)=k_ny0
           k_nz(j)=k_nz0    
          
           k_en1(j)=0
           k_en2(j)=0
           k_en3(j)=0
           
           
       END DO
         
      END IF 
      
C----- KINC=2

      IF (KINC.EQ.2) THEN
      
      ptrk_Coord_1 = SMAFloatArrayCreateSP(17,K_Number_Elements,0.0)
      
      ptrk_Coord_2 = SMAFloatArrayCreateSP(18,K_Number_Elements,0.0)
      
      ptrk_Coord_3 = SMAFloatArrayCreateSP(19,K_Number_Elements,0.0)
      
      ptrk_V = SMAFloatArrayCreateSP(15,K_Number_Elements,0.0)
      
C-----  k_Number_Neighb  
       
      k_Number_Neighb=1
       
       DO i=1, K_Number_Elements
        
        k_Number_Neighb_0=1
        k_Dist=0.0
        
          DO j=1, K_Number_Elements
          
            IF (i.NE.j) THEN
       k_Dist=((k_Coord_1(i)-k_Coord_1(j))**2+
     1 (k_Coord_2(i)-k_Coord_2(j))**2+
     2 (k_Coord_3(i)-k_Coord_3(j))**2)**0.5
         
            IF (k_dist_min.GE.k_Dist) then
                k_Number_Neighb_0=k_Number_Neighb_0+1
            END IF
            
           END IF   
         
         END DO
         
         IF (k_Number_Neighb_0.GT.k_Number_Neighb) then
                k_Number_Neighb=k_Number_Neighb_0
         END IF
          
       END DO
       
      ptrk_Neighb = SMAFloatArrayCreateSP(20,
     1 K_Number_Elements*(k_Number_Neighb+1)*2,0.0)

       
C-----  k_Neighb       
       
      DO i=1, K_Number_Elements
      
        k_Neighb((k_Number_Neighb+1)*2*(i-1)+1)=i
        k_Neighb((k_Number_Neighb+1)*2*(i-1)+3)=i
        k_Neighb((k_Number_Neighb+1)*2*(i-1)+4)=k_dist_min*k_V(i)
        
        k_Number_Neighb_0=1
        k_Dist=0.0
        
        DO j=1, K_Number_Elements
        
            IF (i.NE.j) THEN
       k_Dist=((k_Coord_1(i)-k_Coord_1(j))**2+
     1 (k_Coord_2(i)-k_Coord_2(j))**2+
     2 (k_Coord_3(i)-k_Coord_3(j))**2)**0.5
         
            IF (k_dist_min.GE.k_Dist) then
               k_Number_Neighb_0=k_Number_Neighb_0+1
               k_Neighb((k_Number_Neighb+1)*2*(i-1)+
     1          2*k_Number_Neighb_0+1)=j
               k_Neighb((k_Number_Neighb+1)*2*(i-1)+
     1         2*k_Number_Neighb_0+2)=(k_dist_min-k_Dist)*k_V(j)
         
            END IF
            
            END IF
        
      
        END DO
        
        k_Neighb((k_Number_Neighb+1)*2*(i-1)+2)=k_Number_Neighb_0
   
      END DO 
 
 
      DO i=1, K_Number_Elements
        k_Dist=0
        DO j=1,k_Number_Neighb
            k_Dist=k_Dist+
     1       k_Neighb((k_Number_Neighb+1)*2*(i-1)+2*j+2)
        END DO 
        
        DO j=1,k_Number_Neighb
            k_Neighb((k_Number_Neighb+1)*2*(i-1)+2*j+2)=
     1       k_Neighb((k_Number_Neighb+1)*2*(i-1)+2*j+2)/k_Dist
        END DO 
      
      END DO  
               
      k_Volume=0
      
      DO i=1, K_Number_Elements
        k_Volume=k_Volume+k_V(i)
      END DO 
         
      END IF       
      
      
C-----KINC>2

      IF (KINC.GT.2) THEN
   
      ptrS1 = SMAFloatArrayCreateSP(1,K_Number_Elements,0.0)
      ptrS2 = SMAFloatArrayCreateSP(2,K_Number_Elements,0.0)
      ptrS3 = SMAFloatArrayCreateSP(3,K_Number_Elements,0.0)
      ptrS4 = SMAFloatArrayCreateSP(4,K_Number_Elements,0.0)
      ptrS5 = SMAFloatArrayCreateSP(5,K_Number_Elements,0.0)
      ptrS6 = SMAFloatArrayCreateSP(6,K_Number_Elements,0.0)
      
      ptrE1 = SMAFloatArrayCreateSP(7,K_Number_Elements,0.0)
      ptrE2 = SMAFloatArrayCreateSP(8,K_Number_Elements,0.0)
      ptrE3 = SMAFloatArrayCreateSP(9,K_Number_Elements,0.0)
      ptrE4 = SMAFloatArrayCreateSP(10,K_Number_Elements,0.0)
      ptrE5 = SMAFloatArrayCreateSP(11,K_Number_Elements,0.0)
      ptrE6 = SMAFloatArrayCreateSP(12,K_Number_Elements,0.0)
      
      ptrk_w = SMAFloatArrayCreateSP(13,K_Number_Elements,0.0)
      
      ptrk_C = SMAFloatArrayCreateSP(14,K_Number_Elements,0.0)
      
      ptrk_V = SMAFloatArrayCreateSP(15,K_Number_Elements,0.0)
      
      ptrk_r = SMAFloatArrayCreateSP(16,K_Number_Elements,0.0)
      
      ptrk_rd = SMAFloatArrayCreateSP(21,K_Number_Elements,0.0)
      
      ptrk_nx = SMAFloatArrayCreateSP(22,K_Number_Elements,0.0)
      
      ptrk_ny = SMAFloatArrayCreateSP(23,K_Number_Elements,0.0)
      
      ptrk_nz = SMAFloatArrayCreateSP(24,K_Number_Elements,0.0)
      
      ptrk_nx_n1 = SMAFloatArrayCreateSP(25,K_Number_Elements,0.0)
      
      ptrk_ny_n1 = SMAFloatArrayCreateSP(26,K_Number_Elements,0.0)
      
      ptrk_nz_n1 = SMAFloatArrayCreateSP(27,K_Number_Elements,0.0)
      
      ptrk_nx_n2 = SMAFloatArrayCreateSP(28,K_Number_Elements,0.0)
      
      ptrk_ny_n2 = SMAFloatArrayCreateSP(29,K_Number_Elements,0.0)
      
      ptrk_nz_n2 = SMAFloatArrayCreateSP(30,K_Number_Elements,0.0)      

      ptrk_nx_n3 = SMAFloatArrayCreateSP(31,K_Number_Elements,0.0)
      
      ptrk_ny_n3 = SMAFloatArrayCreateSP(32,K_Number_Elements,0.0)
      
      ptrk_nz_n3 = SMAFloatArrayCreateSP(33,K_Number_Elements,0.0)   
      
                  
      ptrk_en1 = SMAFloatArrayCreateSP(34,K_Number_Elements,0.0)
      
      ptrk_en2 = SMAFloatArrayCreateSP(35,K_Number_Elements,0.0)
      
      ptrk_en3 = SMAFloatArrayCreateSP(36,K_Number_Elements,0.0) 
      
      ptrk_enmin = SMAFloatArrayCreateSP(37,K_Number_Elements,0.0) 
      
      ptrk_rc = SMAFloatArrayCreateSP(38,K_Number_Elements,0.0)      

      ptrk_rdc = SMAFloatArrayCreateSP(39,K_Number_Elements,0.0)      

      ptrk_Cn = SMAFloatArrayCreateSP(40,K_Number_Elements,0.0) 
 
C     Finding M0 - k_M0
  
      k_M0=0
      
      DO j=1, K_Number_Elements
           k_M0=k_M0+k_rend*k_V(j)

      END DO
 
      
C     Finding local and global energy 
C     k_w 
      DO j=1, K_Number_Elements

        k_w(j)=k_S1(j)*k_E1(j)+k_S2(j)*k_E2(j)+k_S3(j)*k_E3(j)+
     1   k_S4(j)*k_E4(j)+k_S5(j)*k_E5(j)+k_S6(j)*k_E6(j)

      ENDDO
      
C     k_WW       
      k_ener=0
      k_WW=0
      DO j=1, K_Number_Elements

           k_ener=k_ener+k_w(j)*k_V(j)

      ENDDO
            
      k_WW=k_ener
      
C     k_r
      k_mu1=0.0
      k_mu2=1000.0
      
      DO WHILE ((k_mu2-k_mu1).GT.0.0000001)

C      DO kk=1, 50

      DO j=1, K_Number_Elements

           k_rc(j)=k_r(j)
           k_rdc(j)=k_rd(j)
           k_Cn(j)=k_C(j)
           
      ENDDO          
          
      k_mid=(k_mu2+k_mu1)/2
          
      DO j=1, K_Number_Elements
           
          k_C1=k_p*k_w(j)/k_rdc(j)-k_mid
          
         
           IF (k_C1.GT.0) THEN
               
              IF (k_Cn(j).GT.0) THEN 
                  
                  k_Cn(j)=k_CC*k_Cn(j)
                  
              else
                
                  k_Cn(j)=-k_Cn(j)/k_CC
                  
              END IF
              
           ELSE
               
             IF (k_Cn(j).LT.0) THEN
                 
                 k_Cn(j)=k_CC*k_Cn(j)
                 
             else
                 
                k_Cn(j)=-k_Cn(j)/k_CC
                
             END IF
             
           END IF
           
              k_rc(j)=k_rc(j)+k_Cn(j)

           IF (k_rc(j).LE.k_Min) THEN
                k_rc(j)=k_Min
                k_Cn(j)=k_C0
           END IF
   
           IF (k_rc(j).GE.k_Max) THEN
                k_rc(j)=k_Max
                k_Cn(j)=-k_C0
           END IF
                   
      ENDDO
      
      DO i=1, K_Number_Elements

      k_rdc(i)=0
        
      ENDDO
      
      DO i=1, K_Number_Elements

         DO j=1,k_Number_Neighb

       k_rdc(i)=k_rdc(i)+
     1  k_rc(k_Neighb((k_Number_Neighb+1)*2*(i-1)+2*j+1))
     2      *k_Neighb((k_Number_Neighb+1)*2*(i-1)+2*j+2)


        END DO
         
      ENDDO
      
C----- k_rr         

      k_rr=0
      
      DO j=1, K_Number_Elements

         k_rr=k_rr+k_rdc(j)*k_V(j)
                   
      END DO
      
      k_rr=k_rr/k_Volume
 
      IF (k_rr.GT.k_rend) THEN
          
          k_mu1=k_mid
      ELSE
          
          k_mu2=k_mid

      END IF      

      END DO
      
      DO j=1, K_Number_Elements

           k_r(j)=k_rc(j)
           k_rd(j)=k_rdc(j)
           k_C(j)=k_Cn(j)
           
      ENDDO        
      
      k_rend_1=k_mid
      
C      IF (k_rend.GE.k_rr) THEN
C         k_rend_1=k_rend_1*1.05
C      END IF
      
C------ Al  
    
      DO i=1, K_Number_Elements
      
C------ Stresses

       k_S(1)=k_S1(i)
       k_S(2)=k_S2(i)
       k_S(3)=k_S3(i)
       k_S(3)=k_S3(i)
       k_S(4)=k_S4(i)
       k_S(5)=k_S5(i)     
       k_S(6)=k_S6(i)  

C------ Actual axis       
      
        k_axis_act(1)=k_nx(i)
        k_axis_act(2)=k_ny(i)
        k_axis_act(3)=k_nz(i)
        
C------ Principal vectors
                
        k_pr_1(1)=k_nx_n1(i)
        k_pr_1(2)=k_ny_n1(i)
        k_pr_1(3)=k_nz_n1(i)
        
        k_pr_2(1)=k_nx_n2(i)
        k_pr_2(2)=k_ny_n2(i)
        k_pr_2(3)=k_nz_n2(i)        
        
        k_pr_3(1)=k_nx_n3(i)
        k_pr_3(2)=k_ny_n3(i)
        k_pr_3(3)=k_nz_n3(i)
        
C------   Rotation vector
 
         IF (TIME(1).LT.225) THEN
             
                  k_delta=k_dal*(250.0-TIME(1))/250.0
                  
         ELSE
            
              k_delta=k_dal/10
         
         END IF 
         
C------ Rotation of vectors

        k_pr_rot_1=Rotate_Vector(k_axis_act,k_pr_1,k_delta)
        k_pr_rot_2=Rotate_Vector(k_axis_act,k_pr_2,k_delta)
        k_pr_rot_3=Rotate_Vector(k_axis_act,k_pr_3,k_delta)
  
        k_en(1)=k_Energy(k_S,k_pr_1) 
        k_en(2)=k_Energy(k_S,k_pr_2)
        k_en(3)=k_Energy(k_S,k_pr_3)
        
        k_en1(i)=k_en(1)
        k_en2(i)=k_en(2)
        k_en3(i)=k_en(3)
        
        k_ii=1
        
        k_ii=minloc(k_en,1)
        
        k_enmin(i)=k_ii
        
        IF (k_ii.EQ.1) THEN
        
         k_nx(i)=k_pr_rot_1(1)
         
         k_ny(i)=k_pr_rot_1(2)
         
         k_nz(i)=k_pr_rot_1(3)
         
        END IF
        
        
        IF (k_ii.EQ.2) THEN
        
         k_nx(i)=k_pr_rot_2(1)
         
         k_ny(i)=k_pr_rot_2(2)
         
         k_nz(i)=k_pr_rot_2(3)
         
        END IF
         
        IF (k_ii.EQ.3) THEN
        
         k_nx(i)=k_pr_rot_3(1)
         
         k_ny(i)=k_pr_rot_3(2)
         
         k_nz(i)=k_pr_rot_3(3)
         
        END IF     
        
         
         
      END DO    
    
C----- Filtering of angle    
    
       DO i=1, K_Number_Elements/8
 
        DO j=1, 8
       
        k_nx((i-1)*8+j)= k_nx((i-1)*8+1)
        k_ny((i-1)*8+j)= k_ny((i-1)*8+1)
        k_nz((i-1)*8+j)= k_nz((i-1)*8+1)
        
        ENDDO
       
      ENDDO
          
C       DO i=1, K_Number_Elements/8
      
C       k_nx_sum=k_nx((i-1)*8+1)+k_nx((i-1)*8+2)+k_nx((i-1)*8+3)+
C     1 k_nx((i-1)*8+4)+k_nx((i-1)*8+5)+k_nx((i-1)*8+6)+
C     2 k_nx((i-1)*8+7)+k_nx((i-1)*8+8)
      
C       k_nx_sum=k_nx_sum/8.0
       
C       k_ny_sum=k_ny((i-1)*8+1)+k_ny((i-1)*8+2)+k_ny((i-1)*8+3)+
C     1 k_ny((i-1)*8+4)+k_ny((i-1)*8+5)+k_ny((i-1)*8+6)+
C     2 k_ny((i-1)*8+7)+k_ny((i-1)*8+8)
      
C       k_ny_sum=k_ny_sum/8.0
      
C       k_nz_sum=k_nz((i-1)*8+1)+k_nz((i-1)*8+2)+k_nz((i-1)*8+3)+
C     1 k_nz((i-1)*8+4)+k_nz((i-1)*8+5)+k_nz((i-1)*8+6)+
C     2 k_nz((i-1)*8+7)+k_nz((i-1)*8+8)
      
C       k_nz_sum=k_nz_sum/8.0 
       
C         k_mod=(k_nx_sum**2+k_ny_sum**2+k_nz_sum**2)**0.5   
         
C       k_nx_sum=k_nx_sum/k_mod 
C       k_ny_sum=k_ny_sum/k_mod 
C       k_nz_sum=k_nz_sum/k_mod 
         
C      DO j=1, 8
       
C        k_nx((i-1)*8+j)= k_nx_sum
C        k_ny((i-1)*8+j)= k_ny_sum
C        k_nz((i-1)*8+j)= k_nz_sum
        
C      ENDDO
      
C      ENDDO    
      
      
      END IF
      
      END IF
      
      RETURN
      END
      
C----------    function Rotate_Vector
  
       function Rotate_Vector(nr0,n0,ksi) result(f)
C function for rotation of vector
      INCLUDE 'ABA_PARAM.INC'
        DOUBLE PRECISION, dimension(3), intent(in):: nr0
        DOUBLE PRECISION, dimension(3), intent(in):: n0 
        DOUBLE PRECISION, intent(in)              :: ksi        
        DOUBLE PRECISION, dimension(3)            :: f
        DOUBLE PRECISION, dimension(3)            :: v
        DOUBLE PRECISION, dimension(3,3)          :: M
        DOUBLE PRECISION, dimension(3)            :: n00
        DOUBLE PRECISION            :: temp1, temp2, k_sign, ksi1
        DOUBLE PRECISION                         :: alfa, betta, gamma
        DOUBLE PRECISION, PARAMETER              :: pi=3.141592653589793
        integer :: i,j
        
       temp1=0
       k_sign=1.0
      
       ksi1=ksi       
       n00=n0
       
       ksi1=ksi1*pi/180.0 
       
C------- Rotation vector       
       
       temp1=nr0(1)*n00(1)+nr0(2)*n00(2)+nr0(3)*n00(3)
       
         IF (temp1.LT.0) THEN
             
             k_sign=-1.0

         END IF       
       
        n00(1)=k_sign*n00(1)
        n00(2)=k_sign*n00(2)
        n00(3)=k_sign*n00(3)
        
       v(1)=nr0(2)*n00(3)-nr0(3)*n00(2)
       
       v(2)=nr0(3)*n00(1)-nr0(1)*n00(3)
       
       v(3)=nr0(1)*n00(2)-nr0(2)*n00(1)
       
       temp2=(v(1)**2+v(2)**2+v(3)**2)**0.5

       v(1)=v(1)/temp2  
       v(2)=v(2)/temp2
       v(3)=v(3)/temp2 
 
C------ Rotation angle

C       ksi1=min(ksi1,acos(k_sign*temp1)/2.0) 

C       ksi1=max(ksi1,0.1)
       
C------ Rotation Matrix

       alfa=v(1)
       betta=v(2)
       gamma=v(3)
       
       M(1,1)=cos(ksi1)+(1.0-cos(ksi1))*alfa**2
       M(1,2)=(1.0-cos(ksi1))*alfa*betta-sin(ksi1)*gamma
       M(1,3)=(1.0-cos(ksi1))*alfa*gamma+sin(ksi1)*betta
       M(2,1)=(1.0-cos(ksi1))*alfa*betta+sin(ksi1)*gamma
       M(2,2)=cos(ksi1)+(1.0-cos(ksi1))*betta**2
       M(2,3)=(1.0-cos(ksi1))*betta*gamma-sin(ksi1)*alfa
       M(3,1)=(1.0-cos(ksi1))*alfa*gamma-sin(ksi1)*betta
       M(3,2)=(1.0-cos(ksi1))*betta*gamma+sin(ksi1)*alfa
       M(3,3)=cos(ksi1)+(1.0-cos(ksi1))*gamma**2

C------ Rotation of vector
       
       f(1)=M(1,1)*nr0(1)+M(1,2)*nr0(2)+M(1,3)*nr0(3)
       f(2)=M(2,1)*nr0(1)+M(2,2)*nr0(2)+M(2,3)*nr0(3)        
       f(3)=M(3,1)*nr0(1)+M(3,2)*nr0(2)+M(3,3)*nr0(3)
       
         IF (f(1).LT.0) THEN
             
             f(1)=-f(1)
             f(2)=-f(2)
             f(3)=-f(3)

         END IF          
        

       return
      end function Rotate_Vector
      
C---------------------------------------------------------      
      
       function k_Energy(s,n) result(E)
        DOUBLE PRECISION, dimension(6), intent(in):: s
        DOUBLE PRECISION, dimension(3), intent(in):: n
        DOUBLE PRECISION                          :: E
        DOUBLE PRECISION, dimension(6)            :: sigma
        DOUBLE PRECISION, dimension(6)            :: eps     
        DOUBLE PRECISION, dimension(3)            :: n1 
        DOUBLE PRECISION, dimension(3)            :: n2       
        DOUBLE PRECISION, dimension(3)            :: n3
        DOUBLE PRECISION, dimension(6,6)          :: A
        DOUBLE PRECISION, dimension(6,6)          :: B    
        DOUBLE PRECISION, dimension(6,6)          :: St    
        DOUBLE PRECISION, dimension(6,6)          :: R   
        DOUBLE PRECISION, dimension(6,6)          :: RR 
        DOUBLE PRECISION, dimension(6,6)          :: T1
        DOUBLE PRECISION, dimension(6,6)          :: T2        
        DOUBLE PRECISION:: temp1, A11, A21, A31, A12, A22, A32, A13, A23
        DOUBLE PRECISION:: A33, B11, B21, B31, B12, B22, B32, B13, B23
        DOUBLE PRECISION:: B33, G23                
        DOUBLE PRECISION, PARAMETER              :: E1=100.0e9
        DOUBLE PRECISION, PARAMETER              :: E2=10.0e9
        DOUBLE PRECISION, PARAMETER              :: G12=3.0e9
        DOUBLE PRECISION, PARAMETER              :: nu12=0.3   
        DOUBLE PRECISION, PARAMETER              :: nu23=0.3   
        integer :: i, j, k  
        
C------- Stresses
        sigma(1)=s(1)
        sigma(2)=s(2)
        sigma(3)=s(3)
        sigma(3)=s(3)
        sigma(4)=s(6)
        sigma(5)=s(5)
        sigma(6)=s(4)
              
C-------  Compliance matrix S

        G23=E2/(2*(1.0+nu23))
       
        St=0
        
       St(1,1)=1.0/E1
       St(1,2)=-nu12/E1
       St(2,1)=St(1,2)
       St(2,2)=1.0/E2
       St(1,3)=-nu12/E1
       St(3,1)=St(1,3)
       St(3,3)=St(2,2)
       St(3,2)=-nu23/E2
       St(2,3)=St(3,2)
       St(4,4)=1/G23
       St(5,5)=1/G12
       St(6,6)=1/G12
       
C------- Reuter's matrix

      R=0
      R(1,1)=1.0
      R(2,2)=1.0
      R(3,3)=1.0
      R(4,4)=2.0
      R(5,5)=2.0       
      R(6,6)=2.0 
      
C------- Re-Reuter's matrix

      RR=0
      RR(1,1)=1.0
      RR(2,2)=1.0
      RR(3,3)=1.0
      RR(4,4)=0.5
      RR(5,5)=0.5       
      RR(6,6)=0.5       
      
        
C-------  Basis
        
        n1(1)=n(1)
        n1(2)=n(2)
        n1(3)=n(3)
        
        IF (n1(1).EQ.1) THEN
        
        n2(1)=0
        n2(2)=1.0
        n2(3)=0
        
        n3(1)=0
        n3(2)=0
        n3(3)=1.0             
                
        ELSE
        
        n2(1)=0
        n2(2)=n1(3)
        n2(3)=-n1(2)
        
        temp1=(n2(1)**2+n2(2)**2+n2(3)**2)**0.5

        n2(1)=n2(1)/temp1  
        n2(2)=n2(2)/temp1
        n2(3)=n2(3)/temp1 
        
        n3(1)=n1(2)*n2(3)-n2(2)*n1(3)
        n3(2)=n1(3)*n2(1)-n1(1)*n2(3)
        n3(3)=n1(1)*n2(2)-n1(2)*n2(1)
        
        temp1=(n3(1)**2+n3(2)**2+n3(3)**2)**0.5   
        
        n3(1)=n3(1)/temp1  
        n3(2)=n3(2)/temp1
        n3(3)=n3(3)/temp1                     
       
        END IF

C------ Rotation Matrix T

        A11=n1(1)
        A21=n1(2)
        A31=n1(3)

        A12=n2(1)
        A22=n2(2)
        A32=n2(3)

        A13=n3(1)
        A23=n3(2)
        A33=n3(3)
        
      A(1,1)=A11*A11
      A(2,1)=A21*A21
      A(3,1)=A31*A31

      A(4,1)=A21*A31
      A(5,1)=A11*A31
      A(6,1)=A11*A21

      A(1,2)=A12*A12
      A(2,2)=A22*A22
      A(3,2)=A32*A32

      A(4,2)=A22*A32
      A(5,2)=A12*A32
      A(6,2)=A12*A22

      A(1,3)=A13*A13
      A(2,3)=A23*A23
      A(3,3)=A33*A33

      A(4,3)=A23*A33
      A(5,3)=A13*A33
      A(6,3)=A13*A23

      A(1,4)=2*A12*A13
      A(2,4)=2*A22*A23
      A(3,4)=2*A32*A33

      A(4,4)=A22*A33+A23*A32
      A(5,4)=A12*A33+A13*A32
      A(6,4)=A12*A23+A13*A22

      A(1,5)=2*A11*A13
      A(2,5)=2*A21*A23
      A(3,5)=2*A31*A33

      A(4,5)=A21*A33+A23*A31
      A(5,5)=A11*A33+A13*A31
      A(6,5)=A11*A23+A13*A21

      A(1,6)=2*A11*A12
      A(2,6)=2*A21*A22
      A(3,6)=2*A31*A32

      A(4,6)=A21*A32+A22*A31
      A(5,6)=A11*A32+A12*A31
      A(6,6)=A11*A22+A12*A21       
        
C------ Rotation Matrix T Re

      B(1,1)=A11*A11
      B(2,1)=A12*A12
      B(3,1)=A13*A13

      B(4,1)=A12*A13
      B(5,1)=A11*A13
      B(6,1)=A11*A12

      B(1,2)=A21*A21
      B(2,2)=A22*A22
      B(3,2)=A23*A23

      B(4,2)=A22*A23
      B(5,2)=A21*A23
      B(6,2)=A21*A22

      B(1,3)=A31*A31
      B(2,3)=A32*A32
      B(3,3)=A33*A33

      B(4,3)=A32*A33
      B(5,3)=A31*A33
      B(6,3)=A31*A32

      B(1,4)=2*A21*A31
      B(2,4)=2*A22*A32
      B(3,4)=2*A23*A33

      B(4,4)=A22*A33+A32*A23
      B(5,4)=A21*A33+A31*A23
      B(6,4)=A21*A32+A31*A22

      B(1,5)=2*A11*A31
      B(2,5)=2*A12*A32
      B(3,5)=2*A13*A33

      B(4,5)=A12*A33+A32*A13
      B(5,5)=A11*A33+A31*A13
      B(6,5)=A11*A32+A31*A12

      B(1,6)=2*A11*A21
      B(2,6)=2*A12*A22
      B(3,6)=2*A13*A23

      B(4,6)=A12*A23+A22*A13
      B(5,6)=A11*A23+A21*A13
      B(6,6)=A11*A22+A21*A12
      
C-------  Rotation of Compliance matrix S     

C------ R*A
       do i=1,6
        do j=1,6
         temp1=0
         
         do k=1,6
            temp1=temp1+R(i,k)*A(k,j)
         enddo
            
         T1(i,j)=temp1   
        
        enddo        
       enddo 
      
C------ R*A*RR
       do i=1,6
        do j=1,6
         temp1=0
         
         do k=1,6
            temp1=temp1+T1(i,k)*RR(k,j)
         enddo
            
         T2(i,j)=temp1   
        
        enddo        
       enddo         
          
      T1=T2
      
C------ R*A*RR*S      
     
       do i=1,6
        do j=1,6
         temp1=0
         
         do k=1,6
            temp1=temp1+T1(i,k)*St(k,j)
         enddo
            
         T2(i,j)=temp1   
        
        enddo        
       enddo         
          
      T1=T2              
      
C------ R*A*RR*S*B

       do i=1,6
        do j=1,6
         temp1=0
         
         do k=1,6
            temp1=temp1+T1(i,k)*B(k,j)
         enddo
            
         T2(i,j)=temp1   
        
        enddo        
       enddo         
          
      T1=T2 
      
C------ Strains

      do i=1,6
      
         temp1=0
         
         do k=1,6
            temp1=temp1+T1(i,k)*sigma(k)
         enddo  
         
         eps(i)=temp1    
      
      enddo         
 
C------ Energy

      temp1=0
      
      do i=1,6
        temp1=temp1+eps(i)*sigma(i)
      enddo
      
      E=temp1      
       
       return  
       end function k_Energy
      
