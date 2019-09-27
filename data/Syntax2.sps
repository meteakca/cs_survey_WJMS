* Encoding: UTF-8.
* programming extent.
UNIANOVA Int_Mean_1 BY ProgramingExtent Test 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /PLOT=PROFILE(ProgramingExtent*Test) 
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(ProgramingExtent) 
  /EMMEANS=TABLES(Test) 
  /EMMEANS=TABLES(ProgramingExtent*Test) 
  /PRINT=OPOWER ETASQ 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=ProgramingExtent Test ProgramingExtent*Test.

UNIANOVA Val_Mean_1 BY ProgramingExtent Test 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /PLOT=PROFILE(ProgramingExtent*Test) 
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(ProgramingExtent) 
  /EMMEANS=TABLES(Test) 
  /EMMEANS=TABLES(ProgramingExtent*Test) 
  /PRINT=OPOWER ETASQ 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=ProgramingExtent Test ProgramingExtent*Test.

UNIANOVA Exp_Mean_1 BY ProgramingExtent Test 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /PLOT=PROFILE(ProgramingExtent*Test) 
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(ProgramingExtent) 
  /EMMEANS=TABLES(Test) 
  /EMMEANS=TABLES(ProgramingExtent*Test) 
  /PRINT=OPOWER ETASQ 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=ProgramingExtent Test ProgramingExtent*Test.

* programming experience.
UNIANOVA Int_Mean_1 BY Programming Test 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /PLOT=PROFILE(Programming*Test) 
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(Programming) 
  /EMMEANS=TABLES(Test) 
  /EMMEANS=TABLES(Programming*Test) 
  /PRINT=OPOWER ETASQ 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Programming Test Programming*Test.

UNIANOVA Val_Mean_1 BY Programming Test 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /PLOT=PROFILE(Programming*Test) 
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(Programming) 
  /EMMEANS=TABLES(Test) 
  /EMMEANS=TABLES(Programming*Test) 
  /PRINT=OPOWER ETASQ 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Programming Test Programming*Test.

UNIANOVA Exp_Mean_1 BY Programming Test 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /PLOT=PROFILE(Programming*Test) 
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(Programming) 
  /EMMEANS=TABLES(Test) 
  /EMMEANS=TABLES(Programming*Test) 
  /PRINT=OPOWER ETASQ 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Programming Test Programming*Test.

* gender.
UNIANOVA Int_Mean_1 BY Gender Test
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /PLOT=PROFILE(Gender*Test) 
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(Gender) 
  /EMMEANS=TABLES(Test) 
  /EMMEANS=TABLES(Gender*Test) 
  /PRINT=OPOWER ETASQ 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Gender Test Gender*Test.

UNIANOVA Val_Mean_1 BY Gender Test
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /PLOT=PROFILE(Gender*Test) 
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(Gender) 
  /EMMEANS=TABLES(Test) 
  /EMMEANS=TABLES(Gender*Test) 
  /PRINT=OPOWER ETASQ 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Gender Test Gender*Test.

UNIANOVA Exp_Mean_1 BY Gender Test
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /PLOT=PROFILE(Gender*Test) 
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(Gender) 
  /EMMEANS=TABLES(Test) 
  /EMMEANS=TABLES(Gender*Test) 
  /PRINT=OPOWER ETASQ 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Gender Test Gender*Test.
