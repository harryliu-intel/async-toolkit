dep1=$1
dep2=$2

function getCDBFilesFromDepFile {
    local depFile=$1
    cat "$depFile" | xargs -n 1 echo | grep '^\/'
}

