dfII_dir=$1
fake_dfII_dir=$2
view=$3
blank_cell=$4

mkdir -p "$fake_dfII_dir"
for file in $(find "$dfII_dir" -maxdepth 1 -type f ) ; do
    cp "$file" "$fake_dfII_dir"&>/dev/null
done

for dir in $(find "$dfII_dir" -type d -name $view -printf "%P\n" ) ; do
    [[ -n "$(find "$fake_dfII_dir/$dir" -type f -not -perm -200)" ]] || \
        ( mkdir -p $(dirname "$fake_dfII_dir/$dir" ) && \
          cp -a "$blank_cell" "$fake_dfII_dir/$dir" )
done
                                                                              
                                                                              
                                                                              

