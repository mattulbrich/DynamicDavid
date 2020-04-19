
full=$(readlink -f $0)
dir=$(dirname $full)

cd $dir/..

./gradlew shadowJar || exit 1

cd $dir

for i in *.dd
do
    if java -jar ../build/libs/Hilbert-0.1-all.jar "$i"
    then
        echo "$i : OK"
    else
        echo "$i : FAIL"
        exit 1
    fi
done
