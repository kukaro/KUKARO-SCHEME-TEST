page=$1
scm=`ls ${page}*`
scheme --quiet < $scm
