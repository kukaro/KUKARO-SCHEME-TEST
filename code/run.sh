page=$1
scm=`ls ${page}*.scm`
scheme --quiet < $scm
