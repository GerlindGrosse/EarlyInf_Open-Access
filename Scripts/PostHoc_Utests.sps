* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
*************************************** Paarweise Post-hoch vergleiche U-tests****
***- adhoc***

NPAR TESTS
  /M-W= adhoc BY age_group(3 35)
/STATISTICS=DESCRIPTIVES
  /MISSING ANALYSIS.

NPAR TESTS
  /M-W= adhoc BY age_group(5 35)
/STATISTICS=DESCRIPTIVES
  /MISSING ANALYSIS.

NPAR TESTS
  /M-W= adhoc BY age_group(5 3)
/STATISTICS=DESCRIPTIVES
  /MISSING ANALYSIS.

**scalar***

NPAR TESTS
  /M-W= scalar BY age_group(3 35)
/STATISTICS=DESCRIPTIVES
  /MISSING ANALYSIS.

NPAR TESTS
  /M-W= scalar BY age_group(5 35)
/STATISTICS=DESCRIPTIVES
  /MISSING ANALYSIS.

NPAR TESTS
  /M-W= scalar BY age_group(5 3)
/STATISTICS=DESCRIPTIVES
  /MISSING ANALYSIS.
