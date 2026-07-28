#! /bin/bash

###Ran with Slurm using 50G memory. This script takes the output from the ancestor.py script and combines the output for all individuals into a single file.

###Load the input directory containing the individual ancestor output files and the type of admixture run (two-way or three-way).
IN_DIR=$DIR/ANC_OUTPUT
RUN=TWO_WAY
RUN=THREE_WAY

###Pull the first line and last line two lines of each individual output file which contains the name of the file and the overall parent ancestry scores, with parent 1 on the second to last line and parent 2 on the last line. The output is saved to a temporary file TEMP_HOLD.txt.
awk '
  { prev2 = prev1; prev1 = $0 }

  FNR==1 {
    first = $0
  }

  ENDFILE {
    gsub(/[\[\]]/, "", prev2)
    gsub(/[\[\]]/, "", prev1)
    print first, prev2, prev1
  }
'  $IN_DIR/*.txt > TEMP_HOLD.txt

####Remove the text "Parent 1's genomic ancestry: " and "Parent 2's genomic ancestry: " from the output and save the final output to a file named ancestor_<RUN>.txt in the input directory.
awk '
{
    gsub(/Parent 1'\''s genomic ancestry: /, "")
    gsub(/Parent 2'\''s genomic ancestry: /, "")
    print
}
' TEMP_HOLD.txt > $IN_DIR/ancestor_$RUN.txt
