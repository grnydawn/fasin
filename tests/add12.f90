        program main
            j_log = -12.861848898625231_r8 + 4.905527742256349_r8*c3 - 358.2337705052991_r8*rh -   0.05463019231872484_r8*c3*t + 4.8630382337426985_r8*rh*t +    0.00020258394697064567_r8*c3*t**2 - 0.02175548069741675_r8*rh*t**2 -    2.502406532869512e-7_r8*c3*t**3 + 0.00003212869941055865_r8*rh*t**3 -    4.39129415725234e6_r8/log(c2)**2 + (56383.93843154586_r8*t)/log(c2)**2 -   (239.835990963361_r8*t**2)/log(c2)**2 +    (0.33765136625580167_r8*t**3)/log(c2)**2 -    (629.7882041830943_r8*rh)/(c3**3*log(c2)) +    (7.772806552631709_r8*rh*t)/(c3**3*log(c2)) -    (0.031974053936299256_r8*rh*t**2)/(c3**3*log(c2)) +    (0.00004383764128775082_r8*rh*t**3)/(c3**3*log(c2)) +    1200.472096232311_r8*log(c2) - 17.37107890065621_r8*t*log(c2) +    0.08170681335921742_r8*t**2*log(c2) - 0.00012534476159729881_r8*t**3*log(c2) -    14.833042158178936_r8*log(c2)**2 + 0.2932631303555295_r8*t*log(c2)**2 -    0.0016497524241142845_r8*t**2*log(c2)**2 +    2.844074805239367e-6_r8*t**3*log(c2)**2 - 231375.56676032578_r8*log(c3) -    100.21645273730675_r8*rh*log(c3) + 2919.2852552424706_r8*t*log(c3) +    0.977886555834732_r8*rh*t*log(c3) - 12.286497122264588_r8*t**2*log(c3) -    0.0030511783284506377_r8*rh*t**2*log(c3) +    0.017249301826661612_r8*t**3*log(c3) + 2.967320346100855e-6_r8*rh*t**3*log(c3) +    (2.360931724951942e6_r8*log(c3))/log(c2) -    (29752.130254319443_r8*t*log(c3))/log(c2) +    (125.04965118142027_r8*t**2*log(c3))/log(c2) -    (0.1752996881934318_r8*t**3*log(c3))/log(c2) +    5599.912337254629_r8*log(c2)*log(c3) - 70.70896612937771_r8*t*log(c2)*log(c3) +    0.2978801613269466_r8*t**2*log(c2)*log(c3) -    0.00041866525019504_r8*t**3*log(c2)*log(c3) + 75061.15281456841_r8*log(c3)**2 -    931.8802278173565_r8*t*log(c3)**2 + 3.863266220840964_r8*t**2*log(c3)**2 -    0.005349472062284983_r8*t**3*log(c3)**2 -    (732006.8180571689_r8*log(c3)**2)/log(c2) +    (9100.06398573816_r8*t*log(c3)**2)/log(c2) -    (37.771091915932004_r8*t**2*log(c3)**2)/log(c2) +    (0.05235455395566905_r8*t**3*log(c3)**2)/log(c2) -    1911.0303773001353_r8*log(c2)*log(c3)**2 +    23.6903969622286_r8*t*log(c2)*log(c3)**2 -    0.09807872005428583_r8*t**2*log(c2)*log(c3)**2 +    0.00013564560238552576_r8*t**3*log(c2)*log(c3)**2 -    3180.5610833308_r8*log(c3)**3 + 39.08268568672095_r8*t*log(c3)**3 -    0.16048521066690752_r8*t**2*log(c3)**3 +    0.00022031380023793877_r8*t**3*log(c3)**3 +    (40751.075322248245_r8*log(c3)**3)/log(c2) -    (501.66977622013934_r8*t*log(c3)**3)/log(c2) +    (2.063469732254135_r8*t**2*log(c3)**3)/log(c2) -    (0.002836873785758324_r8*t**3*log(c3)**3)/log(c2) +    2.792313345723013_r8*log(c2)**2*log(c3)**3 -    0.03422552111802899_r8*t*log(c2)**2*log(c3)**3 +    0.00014019195277521142_r8*t**2*log(c2)**2*log(c3)**3 -    1.9201227328396297e-7_r8*t**3*log(c2)**2*log(c3)**3 -    980.923146020468_r8*log(rh) + 10.054155220444462_r8*t*log(rh) -    0.03306644502023841_r8*t**2*log(rh) + 0.000034274041225891804_r8*t**3*log(rh) +    (16597.75554295064_r8*log(rh))/log(c2) -    (175.2365504237746_r8*t*log(rh))/log(c2) +    (0.6033215603167458_r8*t**2*log(rh))/log(c2) -    (0.0006731787599587544_r8*t**3*log(rh))/log(c2) -    89.38961120336789_r8*log(c3)*log(rh) + 1.153344219304926_r8*t*log(c3)*log(rh) -    0.004954549700267233_r8*t**2*log(c3)*log(rh) +    7.096309866238719e-6_r8*t**3*log(c3)*log(rh) +    3.1712136610383244_r8*log(c3)**3*log(rh) -    0.037822330602328806_r8*t*log(c3)**3*log(rh) +    0.0001500555743561457_r8*t**2*log(c3)**3*log(rh) -    1.9828365865570703e-7_r8*t**3*log(c3)**3*log(rh)

!            erfc = ((((((((((((((((((((((p22 * u + p21) * u + p20)   &
!                                       * u + p19) * u + p18)   &
!                                       * u + p17) * u + p16)   &
!                                       * u + p15) * u + p14)   &
!                                       * u + p13) * u + p12)   &
!                                       * u + p11) * u + p10)   &
!                                       * u + p09) * u + p08)   &
!                                       * u + p07) * u + p06)   &
!                                       * u + p05) * u + p04)   &
!                                       * u + p03) * u + p02)   &
!                                       * u + p01) * u + p00) * t * exp(-x**2)
        end program
